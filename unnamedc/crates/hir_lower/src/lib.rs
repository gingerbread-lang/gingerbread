use ast::AstToken;
use la_arena::{Arena, ArenaMap};
use std::collections::HashMap;

pub fn lower(ast: &ast::Root) -> (hir::Program, SourceMap) {
    let mut lower_ctx = LowerCtx::default();

    for stmt in ast.stmts() {
        lower_ctx.lower_stmt(stmt);
    }

    (hir::Program { exprs: lower_ctx.exprs, stmts: lower_ctx.stmts }, lower_ctx.source_map)
}

#[derive(Debug, Default)]
pub struct SourceMap {
    pub expr_map: ArenaMap<hir::ExprIdx, ast::Expr>,
    pub expr_map_back: HashMap<ast::Expr, hir::ExprIdx>,
}

#[derive(Default)]
struct LowerCtx {
    exprs: Arena<hir::Expr>,
    stmts: Vec<hir::Stmt>,
    source_map: SourceMap,
}

impl LowerCtx {
    fn lower_stmt(&mut self, ast: ast::Stmt) {
        let stmt = match ast {
            ast::Stmt::VarDef(ast) => hir::Stmt::VarDef(hir::VarDef {
                name: hir::Name(ast.name().map(|ast| ast.text().to_string())),
                value: self.lower_expr(ast.value()),
            }),
            ast::Stmt::Expr(ast) => hir::Stmt::Expr(self.lower_expr(Some(ast))),
        };

        self.stmts.push(stmt);
    }

    fn lower_expr(&mut self, ast: Option<ast::Expr>) -> hir::ExprIdx {
        let ast = match ast {
            Some(ast) => ast,
            None => return self.exprs.alloc(hir::Expr::Missing),
        };

        let expr = match &ast {
            ast::Expr::Bin(ast) => hir::Expr::Bin {
                lhs: self.lower_expr(ast.lhs()),
                rhs: self.lower_expr(ast.rhs()),
                op: ast.op().map(|op| self.lower_op(op)),
            },

            ast::Expr::Paren(ast) => return self.lower_expr(ast.inner()),

            ast::Expr::VarRef(ast) => {
                hir::Expr::VarRef { name: hir::Name(ast.name().map(|ast| ast.text().to_string())) }
            }

            ast::Expr::IntLiteral(ast) => {
                hir::Expr::IntLiteral { value: ast.value().and_then(|ast| ast.text().parse().ok()) }
            }

            ast::Expr::StringLiteral(ast) => hir::Expr::StringLiteral {
                value: ast.value().map(|ast| {
                    let text = ast.text();
                    text[1..text.len() - 1].to_string()
                }),
            },
        };

        let expr = self.exprs.alloc(expr);
        self.source_map.expr_map.insert(expr, ast.clone());
        self.source_map.expr_map_back.insert(ast, expr);

        expr
    }

    fn lower_op(&mut self, ast: ast::Op) -> hir::BinOp {
        match ast {
            ast::Op::Add(_) => hir::BinOp::Add,
            ast::Op::Sub(_) => hir::BinOp::Sub,
            ast::Op::Mul(_) => hir::BinOp::Mul,
            ast::Op::Div(_) => hir::BinOp::Div,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;

    fn check<const LEN: usize>(input: &str, exprs: Arena<hir::Expr>, stmts: [hir::Stmt; LEN]) {
        let parse = parser::parse(&lexer::lex(input));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _) = lower(&root);
        assert_eq!(program, hir::Program { exprs, stmts: stmts.to_vec() });
    }

    #[test]
    fn lower_var_def() {
        let mut exprs = Arena::new();
        let bar = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("bar".to_string())) });

        check(
            "let foo = bar",
            exprs,
            [hir::Stmt::VarDef(hir::VarDef {
                name: hir::Name(Some("foo".to_string())),
                value: bar,
            })],
        );
    }

    #[test]
    fn lower_bin_expr() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(hir::Expr::IntLiteral { value: Some(10) });
        let five = exprs.alloc(hir::Expr::IntLiteral { value: Some(5) });
        let ten_minus_five =
            exprs.alloc(hir::Expr::Bin { lhs: ten, rhs: five, op: Some(hir::BinOp::Sub) });
        let one = exprs.alloc(hir::Expr::IntLiteral { value: Some(1) });
        let ten_minus_five_plus_one = exprs.alloc(hir::Expr::Bin {
            lhs: ten_minus_five,
            rhs: one,
            op: Some(hir::BinOp::Add),
        });

        check("10 - 5 + 1", exprs, [hir::Stmt::Expr(ten_minus_five_plus_one)]);
    }

    #[test]
    fn lower_paren_expr() {
        let mut exprs = Arena::new();
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral { value: Some(92) });

        check("((((92))))", exprs, [hir::Stmt::Expr(ninety_two)]);
    }

    #[test]
    fn lower_var_ref() {
        let mut exprs = Arena::new();
        let idx = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("idx".to_string())) });

        check("idx", exprs, [hir::Stmt::Expr(idx)]);
    }

    #[test]
    fn lower_int_literal() {
        let mut exprs = Arena::new();
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral { value: Some(92) });

        check("92", exprs, [hir::Stmt::Expr(ninety_two)]);
    }

    #[test]
    fn lower_string_literal() {
        let mut exprs = Arena::new();
        let hello = exprs.alloc(hir::Expr::StringLiteral { value: Some("hello".to_string()) });

        check("\"hello\"", exprs, [hir::Stmt::Expr(hello)]);
    }

    #[test]
    fn lower_multiple_stmts() {
        let mut exprs = Arena::new();
        let b = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("b".to_string())) });
        let a = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("a".to_string())) });
        let four = exprs.alloc(hir::Expr::IntLiteral { value: Some(4) });
        let a_minus_four =
            exprs.alloc(hir::Expr::Bin { lhs: a, rhs: four, op: Some(hir::BinOp::Sub) });

        check(
            "let a = b\na - 4",
            exprs,
            [
                hir::Stmt::VarDef(hir::VarDef { name: hir::Name(Some("a".to_string())), value: b }),
                hir::Stmt::Expr(a_minus_four),
            ],
        );
    }

    #[test]
    fn lower_var_def_without_value() {
        let mut exprs = Arena::new();
        let missing = exprs.alloc(hir::Expr::Missing);

        check(
            "let foo =",
            exprs,
            [hir::Stmt::VarDef(hir::VarDef {
                name: hir::Name(Some("foo".to_string())),
                value: missing,
            })],
        );
    }

    #[test]
    fn lower_var_def_without_name() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(hir::Expr::IntLiteral { value: Some(10) });

        check(
            "let = 10",
            exprs,
            [hir::Stmt::VarDef(hir::VarDef { name: hir::Name(None), value: ten })],
        );
    }

    #[test]
    fn lower_too_big_int_literal() {
        let mut exprs = Arena::new();
        let int_literal = exprs.alloc(hir::Expr::IntLiteral { value: None });

        check("9999999999999999", exprs, [hir::Stmt::Expr(int_literal)]);
    }

    #[test]
    fn source_map() {
        let parse = parser::parse(&lexer::lex("10 - 5"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();

        let bin_expr_ast = match root.stmts().next().unwrap() {
            ast::Stmt::Expr(ast::Expr::Bin(bin_expr)) => bin_expr,
            _ => unreachable!(),
        };
        let ten_ast = bin_expr_ast.lhs().unwrap();
        let five_ast = bin_expr_ast.rhs().unwrap();
        let bin_expr_ast = ast::Expr::Bin(bin_expr_ast);

        let mut exprs = Arena::new();
        let ten_hir = exprs.alloc(hir::Expr::IntLiteral { value: Some(10) });
        let five_hir = exprs.alloc(hir::Expr::IntLiteral { value: Some(5) });
        let bin_expr_hir =
            exprs.alloc(hir::Expr::Bin { lhs: ten_hir, rhs: five_hir, op: Some(hir::BinOp::Sub) });

        let (program, source_map) = lower(&root);
        assert_eq!(program, hir::Program { exprs, stmts: vec![hir::Stmt::Expr(bin_expr_hir)] });

        assert_eq!(source_map.expr_map[bin_expr_hir], bin_expr_ast);
        assert_eq!(source_map.expr_map[ten_hir], ten_ast);
        assert_eq!(source_map.expr_map[five_hir], five_ast);
        assert_eq!(source_map.expr_map_back[&bin_expr_ast], bin_expr_hir);
        assert_eq!(source_map.expr_map_back[&ten_ast], ten_hir);
        assert_eq!(source_map.expr_map_back[&five_ast], five_hir);
    }
}
