use ast::AstToken;

pub fn lower(ast: &ast::Root) -> hir::Program {
    hir::Program {
        stmts: ast
            .stmts()
            .map(|stmt| match stmt {
                ast::Stmt::VarDef(ast) => hir::Stmt::VarDef(hir::VarDef {
                    name: hir::Name(ast.name().map(|ast| ast.text().to_string())),
                    value: lower_expr(ast.value()),
                }),
                ast::Stmt::Expr(ast) => hir::Stmt::Expr(lower_expr(Some(ast))),
            })
            .collect(),
    }
}

fn lower_expr(ast: Option<ast::Expr>) -> hir::Expr {
    match ast {
        Some(ast::Expr::Bin(ast)) => hir::Expr::Bin {
            lhs: Box::new(lower_expr(ast.lhs())),
            rhs: Box::new(lower_expr(ast.rhs())),
            op: ast.op().map(lower_op),
        },
        Some(ast::Expr::Paren(ast)) => lower_expr(ast.inner()),
        Some(ast::Expr::VarRef(ast)) => {
            hir::Expr::VarRef { name: hir::Name(ast.name().map(|ast| ast.text().to_string())) }
        }
        Some(ast::Expr::IntLiteral(ast)) => {
            hir::Expr::IntLiteral { value: ast.value().and_then(|ast| ast.text().parse().ok()) }
        }
        None => hir::Expr::Missing,
    }
}

fn lower_op(ast: ast::Op) -> hir::BinOp {
    match ast {
        ast::Op::Add(_) => hir::BinOp::Add,
        ast::Op::Sub(_) => hir::BinOp::Sub,
        ast::Op::Mul(_) => hir::BinOp::Mul,
        ast::Op::Div(_) => hir::BinOp::Div,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;

    fn check<const LEN: usize>(input: &str, stmts: [hir::Stmt; LEN]) {
        let parse = parser::parse(lexer::lex(input));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        assert_eq!(lower(&root), hir::Program { stmts: stmts.to_vec() });
    }

    #[test]
    fn lower_var_def() {
        check(
            "let foo = bar",
            [hir::Stmt::VarDef(hir::VarDef {
                name: hir::Name(Some("foo".to_string())),
                value: hir::Expr::VarRef { name: hir::Name(Some("bar".to_string())) },
            })],
        );
    }

    #[test]
    fn lower_bin_expr() {
        check(
            "10 - 5 + 1",
            [hir::Stmt::Expr(hir::Expr::Bin {
                lhs: Box::new(hir::Expr::Bin {
                    lhs: Box::new(hir::Expr::IntLiteral { value: Some(10) }),
                    rhs: Box::new(hir::Expr::IntLiteral { value: Some(5) }),
                    op: Some(hir::BinOp::Sub),
                }),
                rhs: Box::new(hir::Expr::IntLiteral { value: Some(1) }),
                op: Some(hir::BinOp::Add),
            })],
        );
    }

    #[test]
    fn lower_paren_expr() {
        check("((((92))))", [hir::Stmt::Expr(hir::Expr::IntLiteral { value: Some(92) })]);
    }

    #[test]
    fn lower_var_ref() {
        check(
            "idx",
            [hir::Stmt::Expr(hir::Expr::VarRef { name: hir::Name(Some("idx".to_string())) })],
        );
    }

    #[test]
    fn lower_int_literal() {
        check("92", [hir::Stmt::Expr(hir::Expr::IntLiteral { value: Some(92) })]);
    }

    #[test]
    fn lower_multiple_stmts() {
        check(
            "let a = b\na - 4",
            [
                hir::Stmt::VarDef(hir::VarDef {
                    name: hir::Name(Some("a".to_string())),
                    value: hir::Expr::VarRef { name: hir::Name(Some("b".to_string())) },
                }),
                hir::Stmt::Expr(hir::Expr::Bin {
                    lhs: Box::new(hir::Expr::VarRef { name: hir::Name(Some("a".to_string())) }),
                    rhs: Box::new(hir::Expr::IntLiteral { value: Some(4) }),
                    op: Some(hir::BinOp::Sub),
                }),
            ],
        );
    }

    #[test]
    fn lower_var_def_without_value() {
        check(
            "let foo =",
            [hir::Stmt::VarDef(hir::VarDef {
                name: hir::Name(Some("foo".to_string())),
                value: hir::Expr::Missing,
            })],
        );
    }

    #[test]
    fn lower_var_def_without_name() {
        check(
            "let = 10",
            [hir::Stmt::VarDef(hir::VarDef {
                name: hir::Name(None),
                value: hir::Expr::IntLiteral { value: Some(10) },
            })],
        );
    }

    #[test]
    fn lower_too_big_int_literal() {
        check("9999999999999999", [hir::Stmt::Expr(hir::Expr::IntLiteral { value: None })]);
    }
}
