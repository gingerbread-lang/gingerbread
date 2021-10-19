use arena::{Arena, ArenaMap, IdxRange};
use ast::AstToken;
use std::collections::HashMap;
use text_size::TextRange;

pub fn lower(
    ast: &ast::Root,
) -> (hir::Program, SourceMap, Vec<LowerError>, HashMap<String, hir::VarDefIdx>) {
    lower_with_local_defs(ast, Arena::new(), HashMap::new())
}

pub fn lower_with_local_defs(
    ast: &ast::Root,
    local_defs: Arena<hir::LocalDef>,
    var_names: HashMap<String, hir::VarDefIdx>,
) -> (hir::Program, SourceMap, Vec<LowerError>, HashMap<String, hir::VarDefIdx>) {
    let mut lower_store = LowerStore { local_defs, ..LowerStore::default() };
    let mut lower_ctx =
        LowerCtx { store: &mut lower_store, var_names: VarNames { this: var_names, parent: None } };
    let mut stmts = Vec::new();

    for stmt in ast.stmts() {
        stmts.push(lower_ctx.lower_stmt(stmt));
    }

    let var_def_names = lower_ctx.var_names.this;
    (
        hir::Program {
            local_defs: lower_store.local_defs,
            fnc_defs: lower_store.fnc_defs,
            params: lower_store.params,
            exprs: lower_store.exprs,
            stmts,
        },
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
    UndefinedTy { name: String },
}

struct LowerCtx<'a> {
    store: &'a mut LowerStore,
    var_names: VarNames<'a>,
}

#[derive(Default)]
struct LowerStore {
    local_defs: Arena<hir::LocalDef>,
    fnc_defs: Arena<hir::FncDef>,
    params: Arena<hir::Param>,
    exprs: Arena<hir::Expr>,
    source_map: SourceMap,
    errors: Vec<LowerError>,
}

impl LowerCtx<'_> {
    fn lower_stmt(&mut self, ast: ast::Stmt) -> hir::Stmt {
        match ast {
            ast::Stmt::LocalDef(ast) => hir::Stmt::LocalDef(self.lower_local_def(ast)),
            ast::Stmt::FncDef(ast) => hir::Stmt::FncDef(self.lower_fnc_def(ast)),
            ast::Stmt::Expr(ast) => hir::Stmt::Expr(self.lower_expr(Some(ast))),
        }
    }

    fn lower_local_def(&mut self, ast: ast::LocalDef) -> hir::LocalDefIdx {
        let value = self.lower_expr(ast.value());
        let idx = self.store.local_defs.alloc(hir::LocalDef { value });

        if let Some(name) = ast.name() {
            self.var_names.this.insert(name.text().to_string(), hir::VarDefIdx::Local(idx));
        }

        idx
    }

    fn lower_fnc_def(&mut self, ast: ast::FncDef) -> hir::FncDefIdx {
        let mut child = self.new_child();

        let mut first_and_last_param = None;

        if let Some(param_list) = ast.param_list() {
            for param in param_list.params() {
                let ty = child.lower_ty(param.ty());
                let idx = child.store.params.alloc(hir::Param { ty });

                first_and_last_param = match first_and_last_param {
                    None => Some((idx, None)),
                    Some((first, None)) => Some((first, Some(idx))),
                    Some((first, Some(_))) => Some((first, Some(idx))),
                };

                if let Some(name) = param.name() {
                    child
                        .var_names
                        .this
                        .insert(name.text().to_string(), hir::VarDefIdx::Param(idx));
                }
            }
        }

        let params = match first_and_last_param {
            Some((first, Some(last))) => IdxRange::new_inclusive(first..=last),
            Some((first, None)) => IdxRange::new_inclusive(first..=first),
            None => IdxRange::default(),
        };

        let ret_ty = match ast.ret_ty() {
            Some(ret_ty) => child.lower_ty(ret_ty.ty()),
            None => hir::Ty::Unit,
        };

        let body = child.lower_expr(ast.body());

        self.store.fnc_defs.alloc(hir::FncDef { params, ret_ty, body })
    }

    fn lower_ty(&mut self, ast: Option<ast::Ty>) -> hir::Ty {
        if let Some(ast) = ast {
            if let Some(name) = ast.name() {
                let text = name.text();

                if text == "s32" {
                    return hir::Ty::S32;
                }

                self.store.errors.push(LowerError {
                    range: name.range(),
                    kind: LowerErrorKind::UndefinedTy { name: text.to_string() },
                });
            }
        }

        hir::Ty::Missing
    }

    fn lower_expr(&mut self, ast: Option<ast::Expr>) -> hir::ExprIdx {
        let ast = match ast {
            Some(ast) => ast,
            None => return self.store.exprs.alloc(hir::Expr::Missing),
        };

        let expr = match ast.clone() {
            ast::Expr::Bin(ast) => self.lower_bin_expr(ast),
            ast::Expr::Block(ast) => self.lower_block(ast),
            ast::Expr::Paren(ast) => return self.lower_expr(ast.inner()),
            ast::Expr::VarRef(ast) => self.lower_var_ref(ast),
            ast::Expr::IntLiteral(ast) => self.lower_int_literal(ast),
            ast::Expr::StringLiteral(ast) => self.lower_string_literal(ast),
        };

        let expr = self.store.exprs.alloc(expr);
        self.store.source_map.expr_map.insert(expr, ast.clone());
        self.store.source_map.expr_map_back.insert(ast, expr);

        expr
    }

    fn lower_bin_expr(&mut self, ast: ast::BinExpr) -> hir::Expr {
        hir::Expr::Bin {
            lhs: self.lower_expr(ast.lhs()),
            rhs: self.lower_expr(ast.rhs()),
            op: ast.op().map(|op| self.lower_op(op)),
        }
    }

    fn lower_block(&mut self, ast: ast::Block) -> hir::Expr {
        let mut child = self.new_child();
        hir::Expr::Block(ast.stmts().map(|ast| child.lower_stmt(ast)).collect())
    }

    fn lower_var_ref(&mut self, ast: ast::VarRef) -> hir::Expr {
        ast.name().map_or(hir::Expr::Missing, |ast| {
            let name = ast.text();

            match self.var_names.get(name) {
                Some(var_def) => hir::Expr::VarRef(var_def),
                None => {
                    self.store.errors.push(LowerError {
                        range: ast.range(),
                        kind: LowerErrorKind::UndefinedVar { name: name.to_string() },
                    });

                    hir::Expr::Missing
                }
            }
        })
    }

    fn lower_int_literal(&self, ast: ast::IntLiteral) -> hir::Expr {
        ast.value()
            .and_then(|ast| ast.text().parse().ok())
            .map_or(hir::Expr::Missing, hir::Expr::IntLiteral)
    }

    fn lower_string_literal(&self, ast: ast::StringLiteral) -> hir::Expr {
        ast.value().map_or(hir::Expr::Missing, |ast| {
            let text = ast.text();
            hir::Expr::StringLiteral(text[1..text.len() - 1].to_string())
        })
    }

    fn lower_op(&self, ast: ast::Op) -> hir::BinOp {
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
            var_names: VarNames { this: HashMap::new(), parent: Some(&self.var_names) },
        }
    }
}

struct VarNames<'a> {
    this: HashMap<String, hir::VarDefIdx>,
    parent: Option<&'a Self>,
}

impl VarNames<'_> {
    fn get(&self, name: &str) -> Option<hir::VarDefIdx> {
        self.this.get(name).copied().or_else(|| self.parent.and_then(|parent| parent.get(name)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use std::ops::Range as StdRange;

    fn check<const ERRORS_LEN: usize>(
        input: &str,
        expected_program: hir::Program,
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
        let (actual_program, _, actual_errors, _) = lower(&root);

        pretty_assertions::assert_eq!(actual_program, expected_program);
        pretty_assertions::assert_eq!(actual_errors, expected_errors);
    }

    #[test]
    fn lower_local_def() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let bar = exprs.alloc(hir::Expr::IntLiteral(92));
        let local_def = local_defs.alloc(hir::LocalDef { value: bar });

        check(
            "let foo = 92",
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::LocalDef(local_def)],
                ..Default::default()
            },
            [],
        );
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

        check(
            "10 - 5 + 1",
            hir::Program {
                exprs,
                stmts: vec![hir::Stmt::Expr(ten_minus_five_plus_one)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_paren_expr() {
        let mut exprs = Arena::new();
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral(92));

        check(
            "((((92))))",
            hir::Program { exprs, stmts: vec![hir::Stmt::Expr(ninety_two)], ..Default::default() },
            [],
        );
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
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::LocalDef(idx_def), hir::Stmt::Expr(idx)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_undefined_var_ref() {
        let mut exprs = Arena::new();
        let missing = exprs.alloc(hir::Expr::Missing);

        check(
            "user",
            hir::Program { exprs, stmts: vec![hir::Stmt::Expr(missing)], ..Default::default() },
            [(0..4, LowerErrorKind::UndefinedVar { name: "user".to_string() })],
        );
    }

    #[test]
    fn lower_int_literal() {
        let mut exprs = Arena::new();
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral(92));

        check(
            "92",
            hir::Program { exprs, stmts: vec![hir::Stmt::Expr(ninety_two)], ..Default::default() },
            [],
        );
    }

    #[test]
    fn lower_string_literal() {
        let mut exprs = Arena::new();
        let hello = exprs.alloc(hir::Expr::StringLiteral("hello".to_string()));

        check(
            "\"hello\"",
            hir::Program { exprs, stmts: vec![hir::Stmt::Expr(hello)], ..Default::default() },
            [],
        );
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
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::LocalDef(a_def), hir::Stmt::Expr(a_minus_four)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_local_def_without_value() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let missing = exprs.alloc(hir::Expr::Missing);
        let foo = local_defs.alloc(hir::LocalDef { value: missing });

        check(
            "let foo =",
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::LocalDef(foo)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_local_def_without_name() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let ten = exprs.alloc(hir::Expr::IntLiteral(10));
        let local_def = local_defs.alloc(hir::LocalDef { value: ten });

        check(
            "let = 10",
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::LocalDef(local_def)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_too_big_int_literal() {
        let mut exprs = Arena::new();
        let missing = exprs.alloc(hir::Expr::Missing);

        check(
            "9999999999999999",
            hir::Program { exprs, stmts: vec![hir::Stmt::Expr(missing)], ..Default::default() },
            [],
        );
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
                stmts: vec![hir::Stmt::LocalDef(a_def)],
                ..Default::default()
            }
        );
        assert!(errors.is_empty());

        let mut exprs = Arena::new();
        let a = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(a_def)));

        let parse = parser::parse(&lexer::lex("a"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, errors, _) =
            lower_with_local_defs(&root, local_defs.clone(), local_def_names);

        assert_eq!(
            program,
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::Expr(a)],
                ..Default::default()
            }
        );
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

        check(
            "{ let foo = 100 - 10\nfoo + 2 }",
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::Expr(block)],
                ..Default::default()
            },
            [],
        );
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
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![
                    hir::Stmt::LocalDef(outer_count_def),
                    hir::Stmt::Expr(block),
                    hir::Stmt::Expr(outer_count),
                ],
                ..Default::default()
            },
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
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::LocalDef(a_def), hir::Stmt::Expr(block)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_fnc_def_with_no_params_or_ret_ty() {
        let mut fnc_defs = Arena::new();
        let mut exprs = Arena::new();

        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new()));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdxRange::default(),
            ret_ty: hir::Ty::Unit,
            body: empty_block,
        });

        check(
            "fnc f() -> {}",
            hir::Program {
                fnc_defs,
                exprs,
                stmts: vec![hir::Stmt::FncDef(fnc_def)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_fnc_def_with_param_but_no_ret_ty() {
        let mut fnc_defs = Arena::new();
        let mut params = Arena::new();
        let mut exprs = Arena::new();

        let x = params.alloc(hir::Param { ty: hir::Ty::S32 });
        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new()));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdxRange::new_inclusive(x..=x),
            ret_ty: hir::Ty::Unit,
            body: empty_block,
        });

        check(
            "fnc drop(x: s32) -> {}",
            hir::Program {
                fnc_defs,
                params,
                exprs,
                stmts: vec![hir::Stmt::FncDef(fnc_def)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_fnc_def_with_param_and_ret_ty() {
        let mut fnc_defs = Arena::new();
        let mut params = Arena::new();
        let mut exprs = Arena::new();

        let n_def = params.alloc(hir::Param { ty: hir::Ty::S32 });
        let n = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Param(n_def)));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdxRange::new_inclusive(n_def..=n_def),
            ret_ty: hir::Ty::S32,
            body: n,
        });

        check(
            "fnc id(n: s32): s32 -> n",
            hir::Program {
                fnc_defs,
                params,
                exprs,
                stmts: vec![hir::Stmt::FncDef(fnc_def)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn params_do_not_escape_fnc() {
        let mut fnc_defs = Arena::new();
        let mut params = Arena::new();
        let mut exprs = Arena::new();

        let x_def = params.alloc(hir::Param { ty: hir::Ty::S32 });
        let y_def = params.alloc(hir::Param { ty: hir::Ty::S32 });
        let x = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Param(x_def)));
        let y = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Param(y_def)));
        let x_plus_y = exprs.alloc(hir::Expr::Bin { lhs: x, rhs: y, op: Some(hir::BinOp::Add) });
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdxRange::new_inclusive(x_def..=y_def),
            ret_ty: hir::Ty::S32,
            body: x_plus_y,
        });
        let missing = exprs.alloc(hir::Expr::Missing);

        check(
            r#"
                fnc add(x: s32, y: s32): s32 -> x + y
                x
            "#,
            hir::Program {
                fnc_defs,
                params,
                exprs,
                stmts: vec![hir::Stmt::FncDef(fnc_def), hir::Stmt::Expr(missing)],
                ..Default::default()
            },
            [(71..72, LowerErrorKind::UndefinedVar { name: "x".to_string() })],
        );
    }

    #[test]
    fn lower_fnc_def_with_missing_param_ty() {
        let mut fnc_defs = Arena::new();
        let mut params = Arena::new();
        let mut exprs = Arena::new();

        let x_def = params.alloc(hir::Param { ty: hir::Ty::Missing });
        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new()));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdxRange::new_inclusive(x_def..=x_def),
            ret_ty: hir::Ty::Unit,
            body: empty_block,
        });

        check(
            "fnc f(x) -> {}",
            hir::Program {
                fnc_defs,
                params,
                exprs,
                stmts: vec![hir::Stmt::FncDef(fnc_def)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_undefined_ty() {
        let mut fnc_defs = Arena::new();
        let mut exprs = Arena::new();

        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new()));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdxRange::default(),
            ret_ty: hir::Ty::Missing,
            body: empty_block,
        });

        check(
            "fnc a(): foo -> {}",
            hir::Program {
                fnc_defs,
                exprs,
                stmts: vec![hir::Stmt::FncDef(fnc_def)],
                ..Default::default()
            },
            [(9..12, LowerErrorKind::UndefinedTy { name: "foo".to_string() })],
        );
    }

    #[test]
    fn source_map() {
        let parse = parser::parse(&lexer::lex("fnc f(): s32 -> 4\nlet a = 10\na - 5"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let mut stmts = root.stmts();

        let fnc_def_ast = match stmts.next().unwrap() {
            ast::Stmt::FncDef(fnc_def) => fnc_def,
            _ => unreachable!(),
        };
        let four_ast = fnc_def_ast.body().unwrap();

        let a_def_ast = match stmts.next().unwrap() {
            ast::Stmt::LocalDef(local_def) => local_def,
            _ => unreachable!(),
        };
        let ten_ast = a_def_ast.value().unwrap();

        let bin_expr_ast = match stmts.next().unwrap() {
            ast::Stmt::Expr(ast::Expr::Bin(bin_expr)) => bin_expr,
            _ => unreachable!(),
        };
        let a_ast = bin_expr_ast.lhs().unwrap();
        let five_ast = bin_expr_ast.rhs().unwrap();
        let bin_expr_ast = ast::Expr::Bin(bin_expr_ast);

        let mut fnc_defs = Arena::new();
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let four_hir = exprs.alloc(hir::Expr::IntLiteral(4));
        let f_def_hir = fnc_defs.alloc(hir::FncDef {
            params: IdxRange::default(),
            ret_ty: hir::Ty::S32,
            body: four_hir,
        });

        let ten_hir = exprs.alloc(hir::Expr::IntLiteral(10));
        let a_def_hir = local_defs.alloc(hir::LocalDef { value: ten_hir });

        let a_hir = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(a_def_hir)));
        let five_hir = exprs.alloc(hir::Expr::IntLiteral(5));
        let bin_expr_hir =
            exprs.alloc(hir::Expr::Bin { lhs: a_hir, rhs: five_hir, op: Some(hir::BinOp::Sub) });

        let (program, source_map, errors, _) = lower(&root);

        assert_eq!(
            program,
            hir::Program {
                fnc_defs,
                local_defs,
                exprs,
                stmts: vec![
                    hir::Stmt::FncDef(f_def_hir),
                    hir::Stmt::LocalDef(a_def_hir),
                    hir::Stmt::Expr(bin_expr_hir)
                ],
                ..Default::default()
            }
        );

        assert_eq!(source_map.expr_map[four_hir], four_ast);
        assert_eq!(source_map.expr_map[ten_hir], ten_ast);
        assert_eq!(source_map.expr_map[a_hir], a_ast);
        assert_eq!(source_map.expr_map[five_hir], five_ast);
        assert_eq!(source_map.expr_map[bin_expr_hir], bin_expr_ast);
        assert_eq!(source_map.expr_map_back[&four_ast], four_hir);
        assert_eq!(source_map.expr_map_back[&ten_ast], ten_hir);
        assert_eq!(source_map.expr_map_back[&a_ast], a_hir);
        assert_eq!(source_map.expr_map_back[&five_ast], five_hir);
        assert_eq!(source_map.expr_map_back[&bin_expr_ast], bin_expr_hir);

        assert!(errors.is_empty());
    }
}
