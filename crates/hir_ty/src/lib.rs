use arena::{ArenaMap, Id};
use interner::{Interner, Key};
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Clone)]
pub struct InferenceResult {
    signatures: FxHashMap<hir::Name, Signature>,
    expr_tys: ArenaMap<Id<hir::Expr>, ResolvedTy>,
    local_tys: ArenaMap<Id<hir::LocalDef>, ResolvedTy>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ResolvedTy {
    Unknown,
    S32,
    String,
    Named(hir::Fqn),
    Unit,
}

impl std::ops::Index<Id<hir::Expr>> for InferenceResult {
    type Output = ResolvedTy;

    fn index(&self, expr: Id<hir::Expr>) -> &Self::Output {
        &self.expr_tys[expr]
    }
}

impl std::ops::Index<Id<hir::LocalDef>> for InferenceResult {
    type Output = ResolvedTy;

    fn index(&self, local_def: Id<hir::LocalDef>) -> &Self::Output {
        &self.local_tys[local_def]
    }
}

#[derive(Clone)]
struct Signature {
    return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyDiagnostic {
    pub kind: TyDiagnosticKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyDiagnosticKind {
    Mismatch { expected: ResolvedTy, found: ResolvedTy },
    Undefined { name: Key },
}

pub fn infer_all(
    bodies: &hir::Bodies,
    index: &hir::Index,
    world_index: &hir::WorldIndex,
) -> (InferenceResult, Vec<TyDiagnostic>) {
    let mut expr_tys = ArenaMap::default();
    let mut local_tys = ArenaMap::default();
    let mut diagnostics = Vec::new();
    let mut signatures = FxHashMap::default();

    for (name, function) in index.functions() {
        let signature = get_signature(
            function,
            hir::Path::ThisModule(name),
            index,
            world_index,
            &mut diagnostics,
        );

        FunctionInferenceCtx {
            expr_tys: &mut expr_tys,
            local_tys: &mut local_tys,
            param_tys: &signature.param_tys,
            bodies,
            index,
            world_index,
            diagnostics: &mut diagnostics,
        }
        .finish(name, &signature);

        signatures.insert(name, signature);
    }

    let mut result = InferenceResult { signatures, expr_tys, local_tys };
    result.shrink_to_fit();

    (result, diagnostics)
}

pub fn infer(
    function_name: hir::Name,
    bodies: &hir::Bodies,
    index: &hir::Index,
    world_index: &hir::WorldIndex,
) -> (InferenceResult, Vec<TyDiagnostic>) {
    let function = match index.get_definition(function_name) {
        Some(hir::Definition::Function(f)) => f,
        Some(hir::Definition::Record(_)) | None => panic!("passed non-function name"),
    };

    let mut expr_tys = ArenaMap::default();
    let mut local_tys = ArenaMap::default();
    let mut diagnostics = Vec::new();

    let signature = get_signature(
        function,
        hir::Path::ThisModule(function_name),
        index,
        world_index,
        &mut diagnostics,
    );

    FunctionInferenceCtx {
        expr_tys: &mut expr_tys,
        local_tys: &mut local_tys,
        param_tys: &signature.param_tys,
        bodies,
        index,
        world_index,
        diagnostics: &mut diagnostics,
    }
    .finish(function_name, &signature);

    let mut signatures = FxHashMap::default();
    signatures.insert(function_name, signature);

    let mut result = InferenceResult { signatures, expr_tys, local_tys };
    result.shrink_to_fit();

    (result, diagnostics)
}

struct FunctionInferenceCtx<'a> {
    expr_tys: &'a mut ArenaMap<Id<hir::Expr>, ResolvedTy>,
    local_tys: &'a mut ArenaMap<Id<hir::LocalDef>, ResolvedTy>,
    param_tys: &'a [ResolvedTy],
    bodies: &'a hir::Bodies,
    index: &'a hir::Index,
    world_index: &'a hir::WorldIndex,
    diagnostics: &'a mut Vec<TyDiagnostic>,
}

impl FunctionInferenceCtx<'_> {
    fn finish(mut self, function_name: hir::Name, signature: &Signature) {
        let function_body = self.bodies.function_body(function_name);
        let actual_return_ty = self.infer_expr(function_body);
        self.expect_match(actual_return_ty, signature.return_ty, function_body);
    }

    fn infer_statement(&mut self, statement_id: Id<hir::Statement>) {
        match &self.bodies[statement_id] {
            hir::Statement::Expr(expr) => {
                self.infer_expr(*expr);
            }

            hir::Statement::LocalDef(local_def) => {
                let ty = self.infer_expr(self.bodies[*local_def].value);
                self.local_tys.insert(*local_def, ty);
            }
        }
    }

    fn infer_expr(&mut self, expr: Id<hir::Expr>) -> ResolvedTy {
        let ty = match &self.bodies[expr] {
            hir::Expr::Missing => ResolvedTy::Unknown,
            hir::Expr::IntLiteral(_) => ResolvedTy::S32,
            hir::Expr::StringLiteral(_) => ResolvedTy::String,
            hir::Expr::Binary { lhs, rhs, .. } => {
                let lhs_ty = self.infer_expr(*lhs);
                let rhs_ty = self.infer_expr(*rhs);

                self.expect_match(lhs_ty, ResolvedTy::S32, *lhs);
                self.expect_match(rhs_ty, ResolvedTy::S32, *rhs);

                ResolvedTy::S32
            }
            hir::Expr::Block { statements, tail_expr, .. } => {
                for statement in statements {
                    self.infer_statement(*statement);
                }

                match tail_expr {
                    Some(tail) => self.infer_expr(*tail),
                    None => ResolvedTy::Unit,
                }
            }
            hir::Expr::Local(local_def) => self.local_tys[*local_def],
            hir::Expr::Param { idx } => self.param_tys[*idx as usize],
            hir::Expr::Call { path, args } => {
                let definition = match *path {
                    hir::PathWithRange::ThisModule { name, .. } => {
                        self.index.get_definition(name).unwrap()
                    }
                    hir::PathWithRange::OtherModule { fqn, .. } => {
                        self.world_index.get_definition(fqn).unwrap()
                    }
                };

                let function = match definition {
                    hir::Definition::Function(f) => f,
                    hir::Definition::Record(_) => todo!(),
                };

                let signature = get_signature(
                    function,
                    path.path(),
                    self.index,
                    self.world_index,
                    self.diagnostics,
                );

                for (idx, arg) in args.iter().enumerate() {
                    let arg_ty = self.infer_expr(*arg);
                    self.expect_match(arg_ty, signature.param_tys[idx], *arg);
                }

                signature.return_ty
            }
        };

        self.expr_tys.insert(expr, ty);

        ty
    }

    fn expect_match(&mut self, found: ResolvedTy, expected: ResolvedTy, expr: Id<hir::Expr>) {
        if found == ResolvedTy::Unknown || expected == ResolvedTy::Unknown {
            return;
        }

        if found != expected {
            // if the erroneous expression is a block with a tail expression,
            // attach the error to the tail instead of the whole block
            let expr = match self.bodies[expr] {
                hir::Expr::Block { tail_expr: Some(tail_expr), .. } => tail_expr,
                _ => expr,
            };

            self.diagnostics.push(TyDiagnostic {
                kind: TyDiagnosticKind::Mismatch { expected, found },
                range: self.bodies.range_for_expr(expr),
            });
        }
    }
}

fn get_signature(
    function: &hir::Function,
    path: hir::Path,
    index: &hir::Index,
    world_index: &hir::WorldIndex,
    diagnostics: &mut Vec<TyDiagnostic>,
) -> Signature {
    let range_info = match path {
        hir::Path::ThisModule(name) => index.range_info(name),
        hir::Path::OtherModule(fqn) => world_index.range_info(fqn),
    };

    let (return_ty_range, param_ty_ranges) = match &range_info.tys {
        hir::TysRangeInfo::Function { return_ty, param_tys } => (return_ty, param_tys),
        hir::TysRangeInfo::Record { .. } => unreachable!(),
    };

    let return_ty = resolve_ty(function.return_ty, *return_ty_range, index, diagnostics);

    let param_tys: Vec<_> = function
        .params
        .iter()
        .zip(param_ty_ranges)
        .map(|(param, ty_range)| resolve_ty(param.ty, *ty_range, index, diagnostics))
        .collect();

    Signature { return_ty, param_tys }
}

fn resolve_ty(
    ty: hir::Ty,
    range: Option<TextRange>,
    index: &hir::Index,
    diagnostics: &mut Vec<TyDiagnostic>,
) -> ResolvedTy {
    match ty {
        hir::Ty::Unknown => ResolvedTy::Unknown,
        hir::Ty::S32 => ResolvedTy::S32,
        hir::Ty::String => ResolvedTy::String,
        hir::Ty::Named(name) => match index.get_definition(name) {
            Some(definition) => match definition {
                hir::Definition::Function(_) => todo!(),
                hir::Definition::Record(_) => todo!(),
            },
            None => {
                diagnostics.push(TyDiagnostic {
                    kind: TyDiagnosticKind::Undefined { name: name.0 },
                    range: range.unwrap(),
                });
                ResolvedTy::Unknown
            }
        },
        hir::Ty::Unit => ResolvedTy::Unit,
    }
}

impl InferenceResult {
    fn shrink_to_fit(&mut self) {
        let Self { signatures, expr_tys, local_tys } = self;
        signatures.shrink_to_fit();
        expr_tys.shrink_to_fit();
        local_tys.shrink_to_fit();
    }
}

impl InferenceResult {
    pub fn debug(&self, interner: &Interner) -> String {
        let mut s = String::new();

        for (name, signature) in &self.signatures {
            s.push_str(&format!("{}(", interner.lookup(name.0)));
            for (idx, param_ty) in signature.param_tys.iter().enumerate() {
                if idx != 0 {
                    s.push_str(", ");
                }
                s.push_str(&param_ty.display(interner));
            }
            s.push(')');

            s.push_str(&format!(": {}\n", signature.return_ty.display(interner)));
        }

        s.push('\n');
        for (expr_id, ty) in self.expr_tys.iter() {
            s.push_str(&format!("{}: {}\n", expr_id.to_raw(), ty.display(interner)));
        }

        if self.local_tys.is_empty() {
            return s;
        }

        s.push('\n');
        for (local_def_id, ty) in self.local_tys.iter() {
            s.push_str(&format!("l{}: {}\n", local_def_id.to_raw(), ty.display(interner)));
        }

        s
    }
}

impl ResolvedTy {
    pub fn display(self, interner: &Interner) -> String {
        match self {
            Self::Unknown => "<unknown>".to_string(),
            Self::S32 => "s32".to_string(),
            Self::String => "string".to_string(),
            Self::Named(fqn) => {
                format!("{}.{}", interner.lookup(fqn.module.0), interner.lookup(fqn.name.0))
            }
            Self::Unit => "unit".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use expect_test::{expect, Expect};
    use interner::Interner;

    #[track_caller]
    fn check<const N: usize>(
        input: &str,
        function_name: &str,
        expect: Expect,
        expected_diagnostics: impl Fn(&mut Interner) -> [(TyDiagnosticKind, std::ops::Range<u32>); N],
    ) {
        let modules = utils::split_multi_module_test_data(input);
        let mut interner = Interner::default();
        let mut world_index = hir::WorldIndex::default();

        for (name, text) in &modules {
            if *name == "main" {
                continue;
            }

            let tokens = lexer::lex(text);
            let tree = parser::parse_source_file(&tokens, text).into_syntax_tree();
            let root = ast::Root::cast(tree.root(), &tree).unwrap();
            let (index, _) = hir::index(root, &tree, &mut interner);

            world_index.add_module(hir::Name(interner.intern(name)), index);
        }

        let text = &modules["main"];
        let tokens = lexer::lex(text);
        let tree = parser::parse_source_file(&tokens, text).into_syntax_tree();
        let root = ast::Root::cast(tree.root(), &tree).unwrap();
        let (index, _) = hir::index(root, &tree, &mut interner);
        let (bodies, _) = hir::lower(root, &tree, &index, &world_index, &mut interner);

        let (inference_result, actual_diagnostics) =
            infer(hir::Name(interner.intern(function_name)), &bodies, &index, &world_index);

        expect.assert_eq(&inference_result.debug(&interner));

        let expected_diagnostics: Vec<_> = expected_diagnostics(&mut interner)
            .into_iter()
            .map(|(kind, range)| TyDiagnostic {
                kind,
                range: TextRange::new(range.start.into(), range.end.into()),
            })
            .collect();

        assert_eq!(expected_diagnostics, actual_diagnostics);
    }

    #[test]
    fn unit_function() {
        check(
            r#"
                fnc foo -> {};
            "#,
            "foo",
            expect![[r#"
                foo(): unit

                0: unit
            "#]],
            |_| [],
        );
    }

    #[test]
    fn function_with_return_ty() {
        check(
            r#"
                fnc one: s32 -> 1;
            "#,
            "one",
            expect![[r#"
                one(): s32

                0: s32
            "#]],
            |_| [],
        );
    }

    #[test]
    fn functions_with_undefined_return_ty() {
        check(
            r#"
                fnc one: foo -> 1;
                fnc two: bar.baz -> 2;
            "#,
            "one",
            expect![[r#"
                one(): <unknown>
                two(): <unknown>

                0: s32
                1: s32
            "#]],
            |i| {
                [
                    (TyDiagnosticKind::Undefined { name: i.intern("foo") }, 26..29),
                    (TyDiagnosticKind::Undefined { name: i.intern("foo") }, 26..29),
                ]
            },
        );
    }

    #[test]
    fn function_with_missing_return_ty() {
        check(
            r#"
                fnc one: -> 1;
            "#,
            "one",
            expect![[r#"
                one(): <unknown>

                0: s32
            "#]],
            |_| [],
        );
    }

    #[test]
    fn binary_expr() {
        check(
            r#"
                fnc twenty: s32 -> 10 + 10;
            "#,
            "twenty",
            expect![[r#"
                twenty(): s32

                0: s32
                1: s32
                2: s32
            "#]],
            |_| [],
        );
    }

    #[test]
    fn function_with_params() {
        check(
            r#"
                fnc add(x: s32, y: s32): s32 -> x + y;
            "#,
            "add",
            expect![[r#"
                add(s32, s32): s32

                0: s32
                1: s32
                2: s32
            "#]],
            |_| [],
        );
    }

    #[test]
    fn local_definition_and_usage() {
        check(
            r#"
                fnc main -> {
                    let a = 10;
                    a;
                };
            "#,
            "main",
            expect![[r#"
                main(): unit

                0: s32
                1: s32
                2: unit

                l0: s32
            "#]],
            |_| [],
        );
    }

    #[test]
    fn local_shadowing() {
        check(
            r#"
                fnc foo -> {
                    let a = 10;
                    let a = "10";
                    a;
                };
            "#,
            "foo",
            expect![[r#"
                foo(): unit

                0: s32
                1: string
                2: string
                3: unit

                l0: s32
                l1: string
            "#]],
            |_| [],
        );
    }

    #[test]
    fn non_s32_binary_expr() {
        check(
            r#"
                fnc sum: s32 -> "foo" + 1;
            "#,
            "sum",
            expect![[r#"
                sum(): s32

                0: string
                1: s32
                2: s32
            "#]],
            |_| {
                [(
                    TyDiagnosticKind::Mismatch {
                        expected: ResolvedTy::S32,
                        found: ResolvedTy::String,
                    },
                    33..38,
                )]
            },
        );
    }

    #[test]
    fn binary_expr_with_missing_operand() {
        check(
            r#"
                fnc f: s32 -> 5 +;
            "#,
            "f",
            expect![[r#"
                f(): s32

                0: s32
                1: <unknown>
                2: s32
            "#]],
            |_| [],
        );
    }

    #[test]
    fn mismatched_function_body() {
        check(
            r#"
                fnc s: string -> 92;
            "#,
            "s",
            expect![[r#"
                s(): string

                0: s32
            "#]],
            |_| {
                [(
                    TyDiagnosticKind::Mismatch {
                        expected: ResolvedTy::String,
                        found: ResolvedTy::S32,
                    },
                    34..36,
                )]
            },
        );
    }

    #[test]
    fn call_unit_function() {
        check(
            r#"
                fnc main -> nothing;
                fnc nothing -> {};
            "#,
            "main",
            expect![[r#"
                main(): unit

                0: unit
            "#]],
            |_| [],
        );
    }

    #[test]
    fn call_function_with_return_ty() {
        check(
            r#"
                fnc main: s32 -> number;
                fnc number: s32 -> 5;
            "#,
            "main",
            expect![[r#"
                main(): s32

                0: s32
            "#]],
            |_| [],
        );
    }

    #[test]
    fn call_function_with_params() {
        check(
            r#"
                fnc main: s32 -> id 10;
                fnc id(n: s32): s32 -> n;
            "#,
            "main",
            expect![[r#"
                main(): s32

                0: s32
                1: s32
            "#]],
            |_| [],
        );
    }

    #[test]
    fn mismatched_param_tys() {
        check(
            r#"
                fnc main: s32 -> multiply {}, "a";
                fnc multiply(x: s32, y: s32): s32 -> x * y;
            "#,
            "main",
            expect![[r#"
                main(): s32

                0: unit
                1: string
                2: s32
            "#]],
            |_| {
                [
                    (
                        TyDiagnosticKind::Mismatch {
                            expected: ResolvedTy::S32,
                            found: ResolvedTy::Unit,
                        },
                        43..45,
                    ),
                    (
                        TyDiagnosticKind::Mismatch {
                            expected: ResolvedTy::S32,
                            found: ResolvedTy::String,
                        },
                        47..50,
                    ),
                ]
            },
        );
    }

    #[test]
    fn call_function_from_other_module() {
        check(
            r#"
                #- main
                fnc a: string -> greetings.informal 10;
                #- greetings
                fnc informal(n: s32): string -> "Hello!";
            "#,
            "a",
            expect![[r#"
                a(): string

                0: s32
                1: string
            "#]],
            |_| [],
        );
    }

    #[test]
    fn attach_mismatch_diagnostics_to_block_tail_expr() {
        check(
            r#"
                fnc main -> take_s32 {
                    let a = 10 + 10;
                    "foo"
                };

                fnc take_s32(n: s32) -> {};
            "#,
            "main",
            expect![[r#"
                main(): unit

                0: s32
                1: s32
                2: s32
                3: string
                4: string
                5: unit

                l0: s32
            "#]],
            |_| {
                [(
                    TyDiagnosticKind::Mismatch {
                        expected: ResolvedTy::S32,
                        found: ResolvedTy::String,
                    },
                    97..102,
                )]
            },
        );
    }
}
