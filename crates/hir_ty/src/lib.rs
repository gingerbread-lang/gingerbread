use arena::{ArenaMap, Id};
use interner::Interner;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Clone)]
pub struct InferenceResult {
    signatures: FxHashMap<hir::Name, Signature>,
    expr_tys: ArenaMap<Id<hir::Expr>, hir::Ty>,
    local_tys: ArenaMap<Id<hir::LocalDef>, hir::Ty>,
}

impl std::ops::Index<Id<hir::Expr>> for InferenceResult {
    type Output = hir::Ty;

    fn index(&self, expr: Id<hir::Expr>) -> &Self::Output {
        &self.expr_tys[expr]
    }
}

impl std::ops::Index<Id<hir::LocalDef>> for InferenceResult {
    type Output = hir::Ty;

    fn index(&self, local_def: Id<hir::LocalDef>) -> &Self::Output {
        &self.local_tys[local_def]
    }
}

#[derive(Clone)]
struct Signature {
    return_ty: hir::Ty,
    param_tys: Vec<hir::Ty>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyDiagnostic {
    pub kind: TyDiagnosticKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyDiagnosticKind {
    Mismatch { expected: hir::Ty, found: hir::Ty },
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
        let signature = get_signature(function);

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

    let signature = get_signature(function);

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
    expr_tys: &'a mut ArenaMap<Id<hir::Expr>, hir::Ty>,
    local_tys: &'a mut ArenaMap<Id<hir::LocalDef>, hir::Ty>,
    param_tys: &'a [hir::Ty],
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

    fn infer_expr(&mut self, expr: Id<hir::Expr>) -> hir::Ty {
        let ty = match &self.bodies[expr] {
            hir::Expr::Missing => hir::Ty::Unknown,
            hir::Expr::IntLiteral(_) => hir::Ty::S32,
            hir::Expr::StringLiteral(_) => hir::Ty::String,
            hir::Expr::Binary { lhs, rhs, .. } => {
                let lhs_ty = self.infer_expr(*lhs);
                let rhs_ty = self.infer_expr(*rhs);

                self.expect_match(lhs_ty, hir::Ty::S32, *lhs);
                self.expect_match(rhs_ty, hir::Ty::S32, *rhs);

                hir::Ty::S32
            }
            hir::Expr::Block { statements, tail_expr, .. } => {
                for statement in statements {
                    self.infer_statement(*statement);
                }

                match tail_expr {
                    Some(tail) => self.infer_expr(*tail),
                    None => hir::Ty::Unit,
                }
            }
            hir::Expr::Local(local_def) => self.local_tys[*local_def],
            hir::Expr::Param { idx } => self.param_tys[*idx as usize],
            hir::Expr::Call { path, args } => {
                let definition = match *path {
                    hir::Path::ThisModule(name) => self.index.get_definition(name).unwrap(),
                    hir::Path::OtherModule(fqn) => self.world_index.get_definition(fqn).unwrap(),
                };

                let function = match definition {
                    hir::Definition::Function(f) => f,
                    hir::Definition::Record(_) => todo!(),
                };

                let signature = get_signature(function);

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

    fn expect_match(&mut self, found: hir::Ty, expected: hir::Ty, expr: Id<hir::Expr>) {
        if found == hir::Ty::Unknown || expected == hir::Ty::Unknown {
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

fn get_signature(function: &hir::Function) -> Signature {
    let return_ty = function.return_ty;
    let param_tys: Vec<_> = function.params.iter().map(|param| param.ty).collect();

    Signature { return_ty, param_tys }
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

        let display_ty = |ty| match ty {
            hir::Ty::Unknown => "<unknown>",
            hir::Ty::S32 => "s32",
            hir::Ty::String => "string",
            hir::Ty::Named(n) => interner.lookup(n.0),
            hir::Ty::Unit => "unit",
        };

        for (name, signature) in &self.signatures {
            s.push_str(&format!("{}(", interner.lookup(name.0)));
            for (idx, param_ty) in signature.param_tys.iter().enumerate() {
                if idx != 0 {
                    s.push_str(", ");
                }
                s.push_str(display_ty(*param_ty));
            }
            s.push(')');

            s.push_str(&format!(": {}\n", display_ty(signature.return_ty)));
        }

        s.push('\n');
        for (expr_id, ty) in self.expr_tys.iter() {
            s.push_str(&format!("{}: {}\n", expr_id.to_raw(), display_ty(*ty)));
        }

        if self.local_tys.is_empty() {
            return s;
        }

        s.push('\n');
        for (local_def_id, ty) in self.local_tys.iter() {
            s.push_str(&format!("l{}: {}\n", local_def_id.to_raw(), display_ty(*ty)));
        }

        s
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
        expected_diagnostics: [(TyDiagnosticKind, std::ops::Range<u32>); N],
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

        let expected_diagnostics: Vec<_> = expected_diagnostics
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
            [],
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
            [],
        );
    }

    #[test]
    fn function_with_undefined_return_ty() {
        check(
            r#"
                fnc one: foo -> 1;
            "#,
            "one",
            expect![[r#"
                one(): <unknown>

                0: s32
            "#]],
            [],
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
            [],
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
            [],
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
            [],
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
            [],
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
            [],
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
            [(
                TyDiagnosticKind::Mismatch { expected: hir::Ty::S32, found: hir::Ty::String },
                33..38,
            )],
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
            [],
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
            [(
                TyDiagnosticKind::Mismatch { expected: hir::Ty::String, found: hir::Ty::S32 },
                34..36,
            )],
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
            [],
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
            [],
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
            [],
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
            [
                (
                    TyDiagnosticKind::Mismatch { expected: hir::Ty::S32, found: hir::Ty::Unit },
                    43..45,
                ),
                (
                    TyDiagnosticKind::Mismatch { expected: hir::Ty::S32, found: hir::Ty::String },
                    47..50,
                ),
            ],
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
            [],
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
            [(
                TyDiagnosticKind::Mismatch { expected: hir::Ty::S32, found: hir::Ty::String },
                97..102,
            )],
        );
    }
}
