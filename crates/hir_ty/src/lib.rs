use arena::{ArenaMap, Id};
use std::collections::HashMap;
use std::fmt;
use text_size::TextRange;

pub struct InferenceResult {
    signature: Signature,
    expr_tys: ArenaMap<Id<hir::Expr>, hir::TyKind>,
    local_tys: ArenaMap<Id<hir::LocalDef>, hir::TyKind>,
}

struct Signature {
    return_ty: hir::TyKind,
    param_tys: Vec<hir::TyKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyDiagnostic {
    pub kind: TyDiagnosticKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyDiagnosticKind {
    Mismatch { expected: hir::TyKind, found: hir::TyKind },
}

pub fn infer_all(
    bodies: &hir::Bodies,
    index: &hir::Index,
    world_index: &hir::WorldIndex,
) -> (HashMap<hir::Name, InferenceResult>, Vec<TyDiagnostic>) {
    let mut results = HashMap::new();
    let mut diagnostics = Vec::new();

    for function_name in index.functions() {
        let (r, mut d) = infer(function_name, bodies, index, world_index);
        results.insert(function_name.clone(), r);
        diagnostics.append(&mut d);
    }

    (results, diagnostics)
}

pub fn infer(
    function_name: &hir::Name,
    bodies: &hir::Bodies,
    index: &hir::Index,
    world_index: &hir::WorldIndex,
) -> (InferenceResult, Vec<TyDiagnostic>) {
    let signature = get_signature(index.get_function(function_name).unwrap());

    let mut ctx = Ctx {
        expr_tys: ArenaMap::default(),
        local_tys: ArenaMap::default(),
        param_tys: &signature.param_tys,
        bodies,
        index,
        world_index,
        diagnostics: Vec::new(),
    };

    let function_body = bodies.function_body(function_name);

    let actual_return_ty = ctx.infer_expr(function_body);
    ctx.expect_match(actual_return_ty, signature.return_ty, function_body);

    let Ctx { expr_tys, local_tys, diagnostics, .. } = ctx;

    (InferenceResult { signature, expr_tys, local_tys }, diagnostics)
}

struct Ctx<'a> {
    expr_tys: ArenaMap<Id<hir::Expr>, hir::TyKind>,
    local_tys: ArenaMap<Id<hir::LocalDef>, hir::TyKind>,
    param_tys: &'a [hir::TyKind],
    bodies: &'a hir::Bodies,
    index: &'a hir::Index,
    world_index: &'a hir::WorldIndex,
    diagnostics: Vec<TyDiagnostic>,
}

impl Ctx<'_> {
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

    fn infer_expr(&mut self, expr: Id<hir::Expr>) -> hir::TyKind {
        let ty = match &self.bodies[expr] {
            hir::Expr::Missing => hir::TyKind::Unknown,
            hir::Expr::IntLiteral(_) => hir::TyKind::S32,
            hir::Expr::StringLiteral(_) => hir::TyKind::String,
            hir::Expr::Binary { lhs, rhs, .. } => {
                let lhs_ty = self.infer_expr(*lhs);
                let rhs_ty = self.infer_expr(*rhs);

                self.expect_match(lhs_ty, hir::TyKind::S32, *lhs);
                self.expect_match(rhs_ty, hir::TyKind::S32, *rhs);

                hir::TyKind::S32
            }
            hir::Expr::Block { statements, tail_expr, .. } => {
                for statement in statements {
                    self.infer_statement(*statement);
                }

                match tail_expr {
                    Some(tail) => self.infer_expr(*tail),
                    None => hir::TyKind::Unit,
                }
            }
            hir::Expr::Local(local_def) => self.local_tys[*local_def],
            hir::Expr::Param { idx } => self.param_tys[*idx as usize],
            hir::Expr::Call { path, args } => {
                let function = match path {
                    hir::Path::ThisModule { name } => self.index.get_function(name).unwrap(),
                    hir::Path::OtherModule { module, name } => {
                        match self.world_index.get_function(module, name) {
                            hir::GetFunctionResult::Found(function) => function,
                            hir::GetFunctionResult::UnknownModule
                            | hir::GetFunctionResult::UnknownFunction => unreachable!(),
                        }
                    }
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

    fn expect_match(&mut self, found: hir::TyKind, expected: hir::TyKind, expr: Id<hir::Expr>) {
        if expected == hir::TyKind::Unknown {
            return;
        }

        if found != expected {
            self.diagnostics.push(TyDiagnostic {
                kind: TyDiagnosticKind::Mismatch { expected, found },
                range: self.bodies.range_for_expr(expr),
            });
        }
    }
}

fn get_signature(function: &hir::Function) -> Signature {
    let return_ty = function.return_ty.kind;
    let param_tys: Vec<_> = function.params.iter().map(|param| param.ty.kind).collect();

    Signature { return_ty, param_tys }
}

impl fmt::Debug for InferenceResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let display_ty = |ty| match ty {
            hir::TyKind::Unknown => "<unknown>",
            hir::TyKind::Unit => "unit",
            hir::TyKind::S32 => "s32",
            hir::TyKind::String => "string",
        };

        write!(f, "(")?;
        for (idx, param_ty) in self.signature.param_tys.iter().enumerate() {
            if idx != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", display_ty(*param_ty))?;
        }
        write!(f, ")")?;

        write!(f, ": {}", display_ty(self.signature.return_ty))?;

        writeln!(f, "\n")?;
        for (expr_id, ty) in self.expr_tys.iter() {
            writeln!(f, "{}: {}", expr_id.to_raw(), display_ty(*ty))?;
        }

        if self.local_tys.is_empty() {
            return Ok(());
        }

        writeln!(f)?;
        for (local_def_id, ty) in self.local_tys.iter() {
            writeln!(f, "l{}: {}", local_def_id.to_raw(), display_ty(*ty))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use expect_test::{expect, Expect};
    use hir::WorldIndex;

    #[track_caller]
    fn check<const N: usize>(
        input: &str,
        function_name: &str,
        expect: Expect,
        expected_diagnostics: [(TyDiagnosticKind, std::ops::Range<u32>); N],
    ) {
        let modules = utils::split_multi_module_test_data(input);
        let mut world_index = WorldIndex::default();

        for (name, text) in &modules {
            if *name == "main" {
                continue;
            }

            let tokens = lexer::lex(text);
            let parse = parser::parse_source_file(&tokens);
            let root = ast::Root::cast(parse.syntax_node()).unwrap();
            let (index, _) = hir::index(&root, &WorldIndex::default());

            world_index.add_module(hir::Name(name.to_string()), index);
        }

        let tokens = lexer::lex(modules["main"]);
        let parse = parser::parse_source_file(&tokens);
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (index, _) = hir::index(&root, &WorldIndex::default());
        let (bodies, _) = hir::lower(&root, &index, &world_index);

        let (inference_result, actual_diagnostics) =
            infer(&hir::Name(function_name.to_string()), &bodies, &index, &world_index);

        expect.assert_eq(&format!("{:?}", inference_result));

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
                (): unit

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
                (): s32

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
                (): <unknown>

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
                (): <unknown>

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
                (): s32

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
                (s32, s32): s32

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
                (): unit

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
                (): unit

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
                (): s32

                0: string
                1: s32
                2: s32
            "#]],
            [(
                TyDiagnosticKind::Mismatch {
                    expected: hir::TyKind::S32,
                    found: hir::TyKind::String,
                },
                33..38,
            )],
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
                (): string

                0: s32
            "#]],
            [(
                TyDiagnosticKind::Mismatch {
                    expected: hir::TyKind::String,
                    found: hir::TyKind::S32,
                },
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
                (): unit

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
                (): s32

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
                (): s32

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
                (): s32

                0: unit
                1: string
                2: s32
            "#]],
            [
                (
                    TyDiagnosticKind::Mismatch {
                        expected: hir::TyKind::S32,
                        found: hir::TyKind::Unit,
                    },
                    43..45,
                ),
                (
                    TyDiagnosticKind::Mismatch {
                        expected: hir::TyKind::S32,
                        found: hir::TyKind::String,
                    },
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
                (): string

                0: s32
                1: string
            "#]],
            [],
        )
    }
}
