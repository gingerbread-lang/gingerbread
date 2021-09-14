use ast::AstToken;
use la_arena::{Arena, ArenaMap};
use std::collections::HashMap;
use text_size::TextRange;

pub fn lower(
    ast: &ast::Root,
) -> (hir::Program, SourceMap, Vec<LowerError>, HashMap<String, hir::VarDefIdx>) {
    lower_with_var_defs(ast, Arena::new(), HashMap::new())
}

pub fn lower_with_var_defs(
    ast: &ast::Root,
    var_defs: Arena<hir::VarDef>,
    var_def_names: HashMap<String, hir::VarDefIdx>,
) -> (hir::Program, SourceMap, Vec<LowerError>, HashMap<String, hir::VarDefIdx>) {
    let mut lower_store = LowerStore { var_defs, ..LowerStore::default() };
    let mut lower_ctx = LowerCtx {
        store: &mut lower_store,
        var_def_names: VarDefNames { this: var_def_names, parent: None },
    };
    let mut stmts = Vec::new();

    for stmt in ast.stmts() {
        stmts.push(lower_ctx.lower_stmt(stmt));
    }

    let var_def_names = lower_ctx.var_def_names.this;
    (
        hir::Program { var_defs: lower_store.var_defs, exprs: lower_store.exprs, stmts },
        lower_store.source_map,
        lower_store.errors,
        var_def_names,
    )
}

#[derive(Debug, Default)]
pub struct SourceMap {
    pub expr_map: ArenaMap<hir::ExprIdx, ast::Expr>,
    pub expr_map_back: HashMap<ast::Expr, hir::ExprIdx>,
}

#[derive(Debug, PartialEq)]
pub struct LowerError {
    pub range: TextRange,
    pub kind: LowerErrorKind,
}

#[derive(Debug, PartialEq)]
pub enum LowerErrorKind {
    UndefinedVar { name: String },
}

struct LowerCtx<'a> {
    store: &'a mut LowerStore,
    var_def_names: VarDefNames<'a>,
}

#[derive(Default)]
struct LowerStore {
    var_defs: Arena<hir::VarDef>,
    exprs: Arena<hir::Expr>,
    source_map: SourceMap,
    errors: Vec<LowerError>,
}

impl LowerCtx<'_> {
    fn lower_stmt(&mut self, ast: ast::Stmt) -> hir::Stmt {
        match ast {
            ast::Stmt::VarDef(ast) => hir::Stmt::VarDef(self.lower_var_def(ast)),
            ast::Stmt::Expr(ast) => hir::Stmt::Expr(self.lower_expr(Some(ast))),
        }
    }

    fn lower_var_def(&mut self, ast: ast::VarDef) -> hir::VarDefIdx {
        let value = self.lower_expr(ast.value());
        let idx = self.store.var_defs.alloc(hir::VarDef { value });

        if let Some(name) = ast.name() {
            self.var_def_names.this.insert(name.text().to_string(), idx);
        }

        idx
    }

    fn lower_expr(&mut self, ast: Option<ast::Expr>) -> hir::ExprIdx {
        let ast = match ast {
            Some(ast) => ast,
            None => return self.store.exprs.alloc(hir::Expr::Missing),
        };

        let expr = match &ast {
            ast::Expr::Bin(ast) => hir::Expr::Bin {
                lhs: self.lower_expr(ast.lhs()),
                rhs: self.lower_expr(ast.rhs()),
                op: ast.op().map(|op| self.lower_op(op)),
            },

            ast::Expr::Block(ast) => {
                let mut child = self.new_child();
                hir::Expr::Block { stmts: ast.stmts().map(|ast| child.lower_stmt(ast)).collect() }
            }

            ast::Expr::Paren(ast) => return self.lower_expr(ast.inner()),

            ast::Expr::VarRef(ast) => ast.name().map_or(hir::Expr::Missing, |ast| {
                let name = ast.text();

                match self.var_def_names.get_var_def(name) {
                    Some(var_def) => hir::Expr::VarRef { var_def },
                    None => {
                        self.store.errors.push(LowerError {
                            range: ast.range(),
                            kind: LowerErrorKind::UndefinedVar { name: name.to_string() },
                        });

                        hir::Expr::Missing
                    }
                }
            }),

            ast::Expr::IntLiteral(ast) => ast
                .value()
                .and_then(|ast| ast.text().parse().ok())
                .map_or(hir::Expr::Missing, |value| hir::Expr::IntLiteral { value }),

            ast::Expr::StringLiteral(ast) => ast
                .value()
                .map(|ast| {
                    let text = ast.text();
                    hir::Expr::StringLiteral { value: text[1..text.len() - 1].to_string() }
                })
                .unwrap_or(hir::Expr::Missing),
        };

        let expr = self.store.exprs.alloc(expr);
        self.store.source_map.expr_map.insert(expr, ast.clone());
        self.store.source_map.expr_map_back.insert(ast, expr);

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

    // must use LowerCtx instead of Self due to lifetime
    fn new_child(&mut self) -> LowerCtx<'_> {
        LowerCtx {
            store: self.store,
            var_def_names: VarDefNames { this: HashMap::new(), parent: Some(&self.var_def_names) },
        }
    }
}

struct VarDefNames<'a> {
    this: HashMap<String, hir::VarDefIdx>,
    parent: Option<&'a Self>,
}

impl VarDefNames<'_> {
    fn get_var_def(&self, name: &str) -> Option<hir::VarDefIdx> {
        self.this
            .get(name)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.get_var_def(name)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use std::ops::Range as StdRange;

    fn check<const STMTS_LEN: usize, const ERRORS_LEN: usize>(
        input: &str,
        var_defs: Arena<hir::VarDef>,
        exprs: Arena<hir::Expr>,
        stmts: [hir::Stmt; STMTS_LEN],
        errors: [(StdRange<u32>, LowerErrorKind); ERRORS_LEN],
    ) {
        let expected_errors = IntoIterator::into_iter(errors)
            .map(|(range, kind)| LowerError {
                range: TextRange::new(range.start.into(), range.end.into()),
                kind,
            })
            .collect::<Vec<_>>();

        let parse = parser::parse(&lexer::lex(input));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, actual_errors, _) = lower(&root);

        assert_eq!(program, hir::Program { var_defs, exprs, stmts: stmts.to_vec() });
        assert_eq!(actual_errors, expected_errors);
    }

    #[test]
    fn lower_var_def() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let bar = exprs.alloc(hir::Expr::IntLiteral { value: 92 });
        let var_def = var_defs.alloc(hir::VarDef { value: bar });

        check("let foo = 92", var_defs, exprs, [hir::Stmt::VarDef(var_def)], []);
    }

    #[test]
    fn lower_bin_expr() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(hir::Expr::IntLiteral { value: 10 });
        let five = exprs.alloc(hir::Expr::IntLiteral { value: 5 });
        let ten_minus_five =
            exprs.alloc(hir::Expr::Bin { lhs: ten, rhs: five, op: Some(hir::BinOp::Sub) });
        let one = exprs.alloc(hir::Expr::IntLiteral { value: 1 });
        let ten_minus_five_plus_one = exprs.alloc(hir::Expr::Bin {
            lhs: ten_minus_five,
            rhs: one,
            op: Some(hir::BinOp::Add),
        });

        check("10 - 5 + 1", Arena::new(), exprs, [hir::Stmt::Expr(ten_minus_five_plus_one)], []);
    }

    #[test]
    fn lower_paren_expr() {
        let mut exprs = Arena::new();
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral { value: 92 });

        check("((((92))))", Arena::new(), exprs, [hir::Stmt::Expr(ninety_two)], []);
    }

    #[test]
    fn lower_defined_var_ref() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let zero = exprs.alloc(hir::Expr::IntLiteral { value: 0 });
        let idx_def = var_defs.alloc(hir::VarDef { value: zero });
        let idx = exprs.alloc(hir::Expr::VarRef { var_def: idx_def });

        check(
            "let idx = 0\nidx",
            var_defs,
            exprs,
            [hir::Stmt::VarDef(idx_def), hir::Stmt::Expr(idx)],
            [],
        );
    }

    #[test]
    fn lower_undefined_var_ref() {
        let mut exprs = Arena::new();
        let missing = exprs.alloc(hir::Expr::Missing);

        check(
            "user",
            Arena::new(),
            exprs,
            [hir::Stmt::Expr(missing)],
            [(0..4, LowerErrorKind::UndefinedVar { name: "user".to_string() })],
        );
    }

    #[test]
    fn lower_int_literal() {
        let mut exprs = Arena::new();
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral { value: 92 });

        check("92", Arena::new(), exprs, [hir::Stmt::Expr(ninety_two)], []);
    }

    #[test]
    fn lower_string_literal() {
        let mut exprs = Arena::new();
        let hello = exprs.alloc(hir::Expr::StringLiteral { value: "hello".to_string() });

        check("\"hello\"", Arena::new(), exprs, [hir::Stmt::Expr(hello)], []);
    }

    #[test]
    fn lower_multiple_stmts() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let ten = exprs.alloc(hir::Expr::IntLiteral { value: 10 });
        let a_def = var_defs.alloc(hir::VarDef { value: ten });
        let a = exprs.alloc(hir::Expr::VarRef { var_def: a_def });
        let four = exprs.alloc(hir::Expr::IntLiteral { value: 4 });
        let a_minus_four =
            exprs.alloc(hir::Expr::Bin { lhs: a, rhs: four, op: Some(hir::BinOp::Sub) });

        check(
            "let a = 10\na - 4",
            var_defs,
            exprs,
            [hir::Stmt::VarDef(a_def), hir::Stmt::Expr(a_minus_four)],
            [],
        );
    }

    #[test]
    fn lower_var_def_without_value() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let missing = exprs.alloc(hir::Expr::Missing);
        let foo = var_defs.alloc(hir::VarDef { value: missing });

        check("let foo =", var_defs, exprs, [hir::Stmt::VarDef(foo)], []);
    }

    #[test]
    fn lower_var_def_without_name() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let ten = exprs.alloc(hir::Expr::IntLiteral { value: 10 });
        let var_def = var_defs.alloc(hir::VarDef { value: ten });

        check("let = 10", var_defs, exprs, [hir::Stmt::VarDef(var_def)], []);
    }

    #[test]
    fn lower_too_big_int_literal() {
        let mut exprs = Arena::new();
        let missing = exprs.alloc(hir::Expr::Missing);

        check("9999999999999999", Arena::new(), exprs, [hir::Stmt::Expr(missing)], []);
    }

    #[test]
    fn lower_with_preexisting_var_defs() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let hello = exprs.alloc(hir::Expr::StringLiteral { value: "hello".to_string() });
        let a_def = var_defs.alloc(hir::VarDef { value: hello });

        let parse = parser::parse(&lexer::lex("let a = \"hello\""));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, errors, var_def_names) = lower(&root);

        assert_eq!(
            program,
            hir::Program {
                var_defs: var_defs.clone(),
                exprs,
                stmts: vec![hir::Stmt::VarDef(a_def)]
            }
        );
        assert!(errors.is_empty());

        let mut exprs = Arena::new();
        let a = exprs.alloc(hir::Expr::VarRef { var_def: a_def });

        let parse = parser::parse(&lexer::lex("a"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, errors, _) = lower_with_var_defs(&root, var_defs.clone(), var_def_names);

        assert_eq!(program, hir::Program { var_defs, exprs, stmts: vec![hir::Stmt::Expr(a)] });
        assert!(errors.is_empty());
    }

    #[test]
    fn lower_block() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let one_hundred = exprs.alloc(hir::Expr::IntLiteral { value: 100 });
        let ten = exprs.alloc(hir::Expr::IntLiteral { value: 10 });
        let one_hundred_minus_ten =
            exprs.alloc(hir::Expr::Bin { lhs: one_hundred, rhs: ten, op: Some(hir::BinOp::Sub) });
        let foo_def = var_defs.alloc(hir::VarDef { value: one_hundred_minus_ten });
        let foo = exprs.alloc(hir::Expr::VarRef { var_def: foo_def });
        let two = exprs.alloc(hir::Expr::IntLiteral { value: 2 });
        let foo_plus_two =
            exprs.alloc(hir::Expr::Bin { lhs: foo, rhs: two, op: Some(hir::BinOp::Add) });
        let block = exprs.alloc(hir::Expr::Block {
            stmts: vec![hir::Stmt::VarDef(foo_def), hir::Stmt::Expr(foo_plus_two)],
        });

        check("{ let foo = 100 - 10\nfoo + 2 }", var_defs, exprs, [hir::Stmt::Expr(block)], []);
    }

    #[test]
    fn lower_block_with_same_var_names_inside_as_outside() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let zero = exprs.alloc(hir::Expr::IntLiteral { value: 0 });
        let outer_count_def = var_defs.alloc(hir::VarDef { value: zero });
        let string = exprs.alloc(hir::Expr::StringLiteral { value: "hello there".to_string() });
        let inner_count_def = var_defs.alloc(hir::VarDef { value: string });
        let inner_count = exprs.alloc(hir::Expr::VarRef { var_def: inner_count_def });
        let block = exprs.alloc(hir::Expr::Block {
            stmts: vec![hir::Stmt::VarDef(inner_count_def), hir::Stmt::Expr(inner_count)],
        });
        let outer_count = exprs.alloc(hir::Expr::VarRef { var_def: outer_count_def });

        check(
            r#"
                let count = 0
                {
                    let count = "hello there"
                    count
                }
                count
            "#,
            var_defs,
            exprs,
            [
                hir::Stmt::VarDef(outer_count_def),
                hir::Stmt::Expr(block),
                hir::Stmt::Expr(outer_count),
            ],
            [],
        );
    }

    #[test]
    fn lower_block_that_refers_to_outside_vars() {
        let mut var_defs = Arena::new();
        let mut exprs = Arena::new();

        let eight = exprs.alloc(hir::Expr::IntLiteral { value: 8 });
        let a_def = var_defs.alloc(hir::VarDef { value: eight });
        let sixteen = exprs.alloc(hir::Expr::IntLiteral { value: 16 });
        let b_def = var_defs.alloc(hir::VarDef { value: sixteen });
        let a = exprs.alloc(hir::Expr::VarRef { var_def: a_def });
        let b = exprs.alloc(hir::Expr::VarRef { var_def: b_def });
        let a_times_b = exprs.alloc(hir::Expr::Bin { lhs: a, rhs: b, op: Some(hir::BinOp::Mul) });
        let block = exprs.alloc(hir::Expr::Block {
            stmts: vec![hir::Stmt::VarDef(b_def), hir::Stmt::Expr(a_times_b)],
        });

        check(
            r#"
                let a = 8
                {
                    let b = 16
                    a * b
                }
            "#,
            var_defs,
            exprs,
            [hir::Stmt::VarDef(a_def), hir::Stmt::Expr(block)],
            [],
        );
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
        let ten_hir = exprs.alloc(hir::Expr::IntLiteral { value: 10 });
        let five_hir = exprs.alloc(hir::Expr::IntLiteral { value: 5 });
        let bin_expr_hir =
            exprs.alloc(hir::Expr::Bin { lhs: ten_hir, rhs: five_hir, op: Some(hir::BinOp::Sub) });

        let (program, source_map, errors, _) = lower(&root);

        assert_eq!(
            program,
            hir::Program {
                var_defs: Arena::new(),
                exprs,
                stmts: vec![hir::Stmt::Expr(bin_expr_hir)]
            }
        );

        assert_eq!(source_map.expr_map[bin_expr_hir], bin_expr_ast);
        assert_eq!(source_map.expr_map[ten_hir], ten_ast);
        assert_eq!(source_map.expr_map[five_hir], five_ast);
        assert_eq!(source_map.expr_map_back[&bin_expr_ast], bin_expr_hir);
        assert_eq!(source_map.expr_map_back[&ten_ast], ten_hir);
        assert_eq!(source_map.expr_map_back[&five_ast], five_hir);

        assert!(errors.is_empty());
    }
}
