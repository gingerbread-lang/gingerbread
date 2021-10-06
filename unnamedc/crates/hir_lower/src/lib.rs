use arena::{Arena, ArenaMap};
use ast::AstToken;
use std::collections::HashMap;
use text_size::TextRange;

pub fn lower(
    ast: &ast::Root,
) -> (hir::Program, SourceMap, Vec<LowerError>, HashMap<String, hir::LocalDefIdx>) {
    lower_with_local_defs(ast, Arena::new(), HashMap::new())
}

pub fn lower_with_local_defs(
    ast: &ast::Root,
    local_defs: Arena<hir::LocalDef>,
    local_def_names: HashMap<String, hir::LocalDefIdx>,
) -> (hir::Program, SourceMap, Vec<LowerError>, HashMap<String, hir::LocalDefIdx>) {
    let mut lower_store = LowerStore { local_defs, ..LowerStore::default() };
    let mut lower_ctx = LowerCtx {
        store: &mut lower_store,
        local_names: LocalNames { this: local_def_names, parent: None },
    };
    let mut stmts = Vec::new();

    for stmt in ast.stmts() {
        stmts.push(lower_ctx.lower_stmt(stmt));
    }

    let local_def_names = lower_ctx.local_names.this;
    (
        hir::Program { local_defs: lower_store.local_defs, exprs: lower_store.exprs, stmts },
        lower_store.source_map,
        lower_store.errors,
        local_def_names,
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
    local_names: LocalNames<'a>,
}

#[derive(Default)]
struct LowerStore {
    local_defs: Arena<hir::LocalDef>,
    exprs: Arena<hir::Expr>,
    source_map: SourceMap,
    errors: Vec<LowerError>,
}

impl LowerCtx<'_> {
    fn lower_stmt(&mut self, ast: ast::Stmt) -> hir::Stmt {
        match ast {
            ast::Stmt::LocalDef(ast) => hir::Stmt::LocalDef(self.lower_local_def(ast)),
            ast::Stmt::FncDef(_) => todo!(),
            ast::Stmt::Expr(ast) => hir::Stmt::Expr(self.lower_expr(Some(ast))),
        }
    }

    fn lower_local_def(&mut self, ast: ast::LocalDef) -> hir::LocalDefIdx {
        let value = self.lower_expr(ast.value());
        let idx = self.store.local_defs.alloc(hir::LocalDef { value });

        if let Some(name) = ast.name() {
            self.local_names.this.insert(name.text().to_string(), idx);
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
                hir::Expr::Block(ast.stmts().map(|ast| child.lower_stmt(ast)).collect())
            }

            ast::Expr::Paren(ast) => return self.lower_expr(ast.inner()),

            ast::Expr::VarRef(ast) => ast.name().map_or(hir::Expr::Missing, |ast| {
                let name = ast.text();

                match self.local_names.get_def(name) {
                    Some(local_def) => hir::Expr::VarRef(hir::VarDefIdx::Local(local_def)),
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
                .map_or(hir::Expr::Missing, hir::Expr::IntLiteral),

            ast::Expr::StringLiteral(ast) => ast
                .value()
                .map(|ast| {
                    let text = ast.text();
                    hir::Expr::StringLiteral(text[1..text.len() - 1].to_string())
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
            local_names: LocalNames { this: HashMap::new(), parent: Some(&self.local_names) },
        }
    }
}

struct LocalNames<'a> {
    this: HashMap<String, hir::LocalDefIdx>,
    parent: Option<&'a Self>,
}

impl LocalNames<'_> {
    fn get_def(&self, name: &str) -> Option<hir::LocalDefIdx> {
        self.this.get(name).copied().or_else(|| self.parent.and_then(|parent| parent.get_def(name)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use std::ops::Range as StdRange;

    fn check<const STMTS_LEN: usize, const ERRORS_LEN: usize>(
        input: &str,
        local_defs: Arena<hir::LocalDef>,
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

        assert_eq!(program, hir::Program { local_defs, exprs, stmts: stmts.to_vec() });
        assert_eq!(actual_errors, expected_errors);
    }

    #[test]
    fn lower_local_def() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let bar = exprs.alloc(hir::Expr::IntLiteral(92));
        let local_def = local_defs.alloc(hir::LocalDef { value: bar });

        check("let foo = 92", local_defs, exprs, [hir::Stmt::LocalDef(local_def)], []);
    }

    #[test]
    fn lower_bin_expr() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(hir::Expr::IntLiteral(10));
        let five = exprs.alloc(hir::Expr::IntLiteral(5));
        let ten_minus_five =
            exprs.alloc(hir::Expr::Bin { lhs: ten, rhs: five, op: Some(hir::BinOp::Sub) });
        let one = exprs.alloc(hir::Expr::IntLiteral(1));
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
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral(92));

        check("((((92))))", Arena::new(), exprs, [hir::Stmt::Expr(ninety_two)], []);
    }

    #[test]
    fn lower_defined_var_ref() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let zero = exprs.alloc(hir::Expr::IntLiteral(0));
        let idx_def = local_defs.alloc(hir::LocalDef { value: zero });
        let idx = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(idx_def)));

        check(
            "let idx = 0\nidx",
            local_defs,
            exprs,
            [hir::Stmt::LocalDef(idx_def), hir::Stmt::Expr(idx)],
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
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral(92));

        check("92", Arena::new(), exprs, [hir::Stmt::Expr(ninety_two)], []);
    }

    #[test]
    fn lower_string_literal() {
        let mut exprs = Arena::new();
        let hello = exprs.alloc(hir::Expr::StringLiteral("hello".to_string()));

        check("\"hello\"", Arena::new(), exprs, [hir::Stmt::Expr(hello)], []);
    }

    #[test]
    fn lower_multiple_stmts() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let ten = exprs.alloc(hir::Expr::IntLiteral(10));
        let a_def = local_defs.alloc(hir::LocalDef { value: ten });
        let a = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(a_def)));
        let four = exprs.alloc(hir::Expr::IntLiteral(4));
        let a_minus_four =
            exprs.alloc(hir::Expr::Bin { lhs: a, rhs: four, op: Some(hir::BinOp::Sub) });

        check(
            "let a = 10\na - 4",
            local_defs,
            exprs,
            [hir::Stmt::LocalDef(a_def), hir::Stmt::Expr(a_minus_four)],
            [],
        );
    }

    #[test]
    fn lower_local_def_without_value() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let missing = exprs.alloc(hir::Expr::Missing);
        let foo = local_defs.alloc(hir::LocalDef { value: missing });

        check("let foo =", local_defs, exprs, [hir::Stmt::LocalDef(foo)], []);
    }

    #[test]
    fn lower_local_def_without_name() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let ten = exprs.alloc(hir::Expr::IntLiteral(10));
        let local_def = local_defs.alloc(hir::LocalDef { value: ten });

        check("let = 10", local_defs, exprs, [hir::Stmt::LocalDef(local_def)], []);
    }

    #[test]
    fn lower_too_big_int_literal() {
        let mut exprs = Arena::new();
        let missing = exprs.alloc(hir::Expr::Missing);

        check("9999999999999999", Arena::new(), exprs, [hir::Stmt::Expr(missing)], []);
    }

    #[test]
    fn lower_with_preexisting_local_defs() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let hello = exprs.alloc(hir::Expr::StringLiteral("hello".to_string()));
        let a_def = local_defs.alloc(hir::LocalDef { value: hello });

        let parse = parser::parse(&lexer::lex("let a = \"hello\""));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, errors, local_def_names) = lower(&root);

        assert_eq!(
            program,
            hir::Program {
                local_defs: local_defs.clone(),
                exprs,
                stmts: vec![hir::Stmt::LocalDef(a_def)]
            }
        );
        assert!(errors.is_empty());

        let mut exprs = Arena::new();
        let a = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(a_def)));

        let parse = parser::parse(&lexer::lex("a"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, errors, _) =
            lower_with_local_defs(&root, local_defs.clone(), local_def_names);

        assert_eq!(program, hir::Program { local_defs, exprs, stmts: vec![hir::Stmt::Expr(a)] });
        assert!(errors.is_empty());
    }

    #[test]
    fn lower_block() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let one_hundred = exprs.alloc(hir::Expr::IntLiteral(100));
        let ten = exprs.alloc(hir::Expr::IntLiteral(10));
        let one_hundred_minus_ten =
            exprs.alloc(hir::Expr::Bin { lhs: one_hundred, rhs: ten, op: Some(hir::BinOp::Sub) });
        let foo_def = local_defs.alloc(hir::LocalDef { value: one_hundred_minus_ten });
        let foo = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(foo_def)));
        let two = exprs.alloc(hir::Expr::IntLiteral(2));
        let foo_plus_two =
            exprs.alloc(hir::Expr::Bin { lhs: foo, rhs: two, op: Some(hir::BinOp::Add) });
        let block = exprs.alloc(hir::Expr::Block(vec![
            hir::Stmt::LocalDef(foo_def),
            hir::Stmt::Expr(foo_plus_two),
        ]));

        check("{ let foo = 100 - 10\nfoo + 2 }", local_defs, exprs, [hir::Stmt::Expr(block)], []);
    }

    #[test]
    fn lower_block_with_same_var_names_inside_as_outside() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let zero = exprs.alloc(hir::Expr::IntLiteral(0));
        let outer_count_def = local_defs.alloc(hir::LocalDef { value: zero });
        let string = exprs.alloc(hir::Expr::StringLiteral("hello there".to_string()));
        let inner_count_def = local_defs.alloc(hir::LocalDef { value: string });
        let inner_count = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(inner_count_def)));
        let block = exprs.alloc(hir::Expr::Block(vec![
            hir::Stmt::LocalDef(inner_count_def),
            hir::Stmt::Expr(inner_count),
        ]));
        let outer_count = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(outer_count_def)));

        check(
            r#"
                let count = 0
                {
                    let count = "hello there"
                    count
                }
                count
            "#,
            local_defs,
            exprs,
            [
                hir::Stmt::LocalDef(outer_count_def),
                hir::Stmt::Expr(block),
                hir::Stmt::Expr(outer_count),
            ],
            [],
        );
    }

    #[test]
    fn lower_block_that_refers_to_outside_vars() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let eight = exprs.alloc(hir::Expr::IntLiteral(8));
        let a_def = local_defs.alloc(hir::LocalDef { value: eight });
        let sixteen = exprs.alloc(hir::Expr::IntLiteral(16));
        let b_def = local_defs.alloc(hir::LocalDef { value: sixteen });
        let a = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(a_def)));
        let b = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(b_def)));
        let a_times_b = exprs.alloc(hir::Expr::Bin { lhs: a, rhs: b, op: Some(hir::BinOp::Mul) });
        let block = exprs
            .alloc(hir::Expr::Block(vec![hir::Stmt::LocalDef(b_def), hir::Stmt::Expr(a_times_b)]));

        check(
            r#"
                let a = 8
                {
                    let b = 16
                    a * b
                }
            "#,
            local_defs,
            exprs,
            [hir::Stmt::LocalDef(a_def), hir::Stmt::Expr(block)],
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
        let ten_hir = exprs.alloc(hir::Expr::IntLiteral(10));
        let five_hir = exprs.alloc(hir::Expr::IntLiteral(5));
        let bin_expr_hir =
            exprs.alloc(hir::Expr::Bin { lhs: ten_hir, rhs: five_hir, op: Some(hir::BinOp::Sub) });

        let (program, source_map, errors, _) = lower(&root);

        assert_eq!(
            program,
            hir::Program {
                local_defs: Arena::new(),
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
