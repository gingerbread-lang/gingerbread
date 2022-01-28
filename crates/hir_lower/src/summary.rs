use ast::AstToken;

pub fn lower(ast: ast::Root) -> hir::summary::Summary {
    let mut summary = hir::summary::Summary::default();

    for def in ast.defs() {
        match def {
            ast::Def::FncDef(fnc_def) => {
                let name = match fnc_def.name() {
                    Some(name) => hir::summary::Name(name.text().to_string()),
                    None => continue,
                };

                let params = match fnc_def.param_list() {
                    Some(param_list) => param_list
                        .params()
                        .map(|param| hir::summary::Param {
                            name: param
                                .name()
                                .map(|ast| hir::summary::Name(ast.text().to_string())),
                            ty: lower_ty(param.ty()),
                        })
                        .collect(),
                    None => Vec::new(),
                };

                let ret_ty = match fnc_def.ret_ty() {
                    Some(ret_ty) => lower_ty(ret_ty.ty()),
                    None => hir::summary::Ty::Unit,
                };

                summary.fncs.insert(name, hir::summary::Fnc { params, ret_ty });
            }
        }
    }

    summary
}

fn lower_ty(ast: Option<ast::Ty>) -> hir::summary::Ty {
    if let Some(ast) = ast {
        if let Some(ast) = ast.name() {
            return hir::summary::Ty::Name(hir::summary::Name(ast.text().to_string()));
        }
    }

    hir::summary::Ty::Missing
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use expect_test::{expect, Expect};

    fn check(input: &str, expected: Expect) {
        let parse = parser::parse_source_file(&lexer::lex(input));
        let ast = ast::Root::cast(parse.syntax_node()).unwrap();
        let summary = lower(ast);
        expected.assert_eq(&format!("{:?}", summary));
    }

    #[test]
    fn empty() {
        check("", expect![[""]]);
    }

    #[test]
    fn fnc() {
        check(
            r#"fnc main -> {};"#,
            expect![[r#"
                fnc main;
            "#]],
        );
    }

    #[test]
    fn fnc_with_non_unit_body() {
        check(
            r#"fnc five -> 5;"#,
            expect![[r#"
                fnc five;
            "#]],
        );
    }

    #[test]
    fn fnc_with_ret_ty() {
        check(
            r#"fnc one: s32 -> 1;"#,
            expect![[r#"
                fnc one: s32;
            "#]],
        );
    }

    #[test]
    fn fnc_with_malformed_ret_ty() {
        check(
            r#"fnc a: @ -> {};"#,
            expect![[r#"
                fnc a: ?;
            "#]],
        );
    }

    #[test]
    fn fnc_with_params() {
        check(
            r#"fnc add(x: s32, y: s32): s32 -> x + y;"#,
            expect![[r#"
                fnc add(x: s32, y: s32): s32;
            "#]],
        );
    }

    #[test]
    fn fnc_with_missing_param_name() {
        check(
            r#"fnc f(: string) -> {};"#,
            expect![[r#"
                fnc f(?: string);
            "#]],
        );
    }

    #[test]
    fn fnc_with_missing_param_ty() {
        check(
            r#"fnc foo(a:) -> {};"#,
            expect![[r#"
                fnc foo(a: ?);
            "#]],
        );
    }

    #[test]
    fn fnc_with_missing_name() {
        check(r#"fnc -> {};"#, expect![[""]]);
    }

    #[test]
    fn multiple_fncs() {
        check(
            r#"
                fnc a -> {};
                fnc b -> {};
                fnc c -> {};
                fnc d -> {};
                fnc e -> {};
            "#,
            expect![[r#"
                fnc a;
                fnc b;
                fnc c;
                fnc d;
                fnc e;
            "#]],
        );
    }
}
