use arena::{Arena, ArenaMap, IdRange};
use ast::AstToken;
use std::collections::HashMap;
use text_size::TextRange;

pub fn lower(
    ast: &ast::Root,
) -> (
    hir::Program,
    SourceMap,
    Vec<LowerError>,
    HashMap<String, hir::FncDefId>,
    HashMap<String, hir::VarDefId>,
) {
    lower_with_in_scope(ast, Arena::new(), HashMap::new(), HashMap::new())
}

pub fn lower_with_in_scope(
    ast: &ast::Root,
    local_defs: Arena<hir::LocalDef>,
    mut fnc_names: HashMap<String, hir::FncDefId>,
    var_names: HashMap<String, hir::VarDefId>,
) -> (
    hir::Program,
    SourceMap,
    Vec<LowerError>,
    HashMap<String, hir::FncDefId>,
    HashMap<String, hir::VarDefId>,
) {
    let mut lower_store = LowerStore { local_defs, ..LowerStore::default() };
    let mut lower_ctx = LowerCtx {
        store: &mut lower_store,
        fnc_names: &mut fnc_names,
        var_names: VarNames { this: var_names, parent: None },
    };
    let mut defs = Vec::new();
    let mut stmts = Vec::new();

    for def in ast.defs() {
        defs.push(lower_ctx.lower_def(def));
    }

    for stmt in ast.stmts() {
        stmts.push(lower_ctx.lower_stmt(stmt));
    }

    let tail_expr = ast.tail_expr().map(|ast| lower_ctx.lower_expr(Some(ast)));

    let var_names = lower_ctx.var_names.this;
    (
        hir::Program {
            local_defs: lower_store.local_defs,
            fnc_defs: lower_store.fnc_defs,
            params: lower_store.params,
            exprs: lower_store.exprs,
            defs,
            stmts,
            tail_expr,
        },
        lower_store.source_map,
        lower_store.errors,
        fnc_names,
        var_names,
    )
}

#[derive(Debug, Default)]
pub struct SourceMap {
    pub expr_map: ArenaMap<hir::ExprId, ast::Expr>,
    pub expr_map_back: HashMap<ast::Expr, hir::ExprId>,
}

#[derive(Debug, PartialEq)]
pub struct LowerError {
    pub range: TextRange,
    pub kind: LowerErrorKind,
}

#[derive(Debug, PartialEq)]
pub enum LowerErrorKind {
    UndefinedVarOrFnc { name: String },
    UndefinedFnc { name: String },
    UndefinedTy { name: String },
}

struct LowerCtx<'a> {
    store: &'a mut LowerStore,
    fnc_names: &'a mut HashMap<String, hir::FncDefId>,
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
    fn lower_def(&mut self, ast: ast::Def) -> hir::Def {
        match ast {
            ast::Def::FncDef(ast) => hir::Def::FncDef(self.lower_fnc_def(ast)),
        }
    }

    fn lower_stmt(&mut self, ast: ast::Stmt) -> hir::Stmt {
        match ast {
            ast::Stmt::LocalDef(ast) => hir::Stmt::LocalDef(self.lower_local_def(ast)),
            ast::Stmt::ExprStmt(ast) => hir::Stmt::Expr(self.lower_expr(ast.expr())),
        }
    }

    fn lower_local_def(&mut self, ast: ast::LocalDef) -> hir::LocalDefId {
        let value = self.lower_expr(ast.value());
        let id = self.store.local_defs.alloc(hir::LocalDef { value });

        if let Some(name) = ast.name() {
            self.var_names.this.insert(name.text().to_string(), hir::VarDefId::Local(id));
        }

        id
    }

    fn lower_fnc_def(&mut self, ast: ast::FncDef) -> hir::FncDefId {
        let mut child = self.new_child();

        let mut params = IdRange::builder();

        if let Some(param_list) = ast.param_list() {
            for param in param_list.params() {
                let ty = child.lower_ty(param.ty());
                let id = child.store.params.alloc(hir::Param { ty });

                params.include(id);

                if let Some(name) = param.name() {
                    child.var_names.this.insert(name.text().to_string(), hir::VarDefId::Param(id));
                }
            }
        }

        let params = params.build();

        let ret_ty = match ast.ret_ty() {
            Some(ret_ty) => child.lower_ty(ret_ty.ty()),
            None => hir::Ty::Unit,
        };

        let body = child.lower_expr(ast.body());

        let id = self.store.fnc_defs.alloc(hir::FncDef { params, ret_ty, body });
        if let Some(name) = ast.name() {
            self.fnc_names.insert(name.text().to_string(), id);
        }

        id
    }

    fn lower_ty(&mut self, ast: Option<ast::Ty>) -> hir::Ty {
        if let Some(ast) = ast {
            if let Some(name) = ast.name() {
                let text = name.text();

                match text {
                    "s32" => return hir::Ty::S32,
                    "string" => return hir::Ty::String,
                    _ => {}
                }

                self.store.errors.push(LowerError {
                    range: name.range(),
                    kind: LowerErrorKind::UndefinedTy { name: text.to_string() },
                });
            }
        }

        hir::Ty::Unknown
    }

    fn lower_expr(&mut self, ast: Option<ast::Expr>) -> hir::ExprId {
        let ast = match ast {
            Some(ast) => ast,
            None => return self.store.exprs.alloc(hir::Expr::Missing),
        };

        let expr = match ast.clone() {
            ast::Expr::Bin(ast) => self.lower_bin_expr(ast),
            ast::Expr::Block(ast) => self.lower_block(ast),
            ast::Expr::FncCall(ast) => self.lower_fnc_call(ast),
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

        hir::Expr::Block(
            ast.stmts().map(|ast| child.lower_stmt(ast)).collect(),
            ast.tail_expr().map(|ast| child.lower_expr(Some(ast))),
        )
    }

    fn lower_fnc_call(&mut self, ast: ast::FncCall) -> hir::Expr {
        let name_ast = match ast.name() {
            Some(ast) => ast,
            None => return hir::Expr::Missing,
        };
        let name = name_ast.text();

        let arg_list = if let Some(ast) = ast.arg_list() {
            ast
        } else {
            if let Some(var_def) = self.var_names.get(name) {
                return hir::Expr::VarRef(var_def);
            } else if let Some(&def) = self.fnc_names.get(name) {
                return hir::Expr::FncCall { def, args: IdRange::default() };
            }

            self.store.errors.push(LowerError {
                range: name_ast.range(),
                kind: LowerErrorKind::UndefinedVarOrFnc { name: name.to_string() },
            });

            return hir::Expr::Missing;
        };

        let mut args = IdRange::builder();

        for arg in arg_list.args() {
            let id = self.lower_expr(arg.value());
            args.include(id);
        }

        match self.fnc_names.get(name) {
            Some(&def) => hir::Expr::FncCall { def, args: args.build() },
            None => {
                self.store.errors.push(LowerError {
                    range: name_ast.range(),
                    kind: LowerErrorKind::UndefinedFnc { name: name.to_string() },
                });

                hir::Expr::Missing
            }
        }
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
            fnc_names: self.fnc_names,
            var_names: VarNames { this: HashMap::new(), parent: Some(&self.var_names) },
        }
    }
}

struct VarNames<'a> {
    this: HashMap<String, hir::VarDefId>,
    parent: Option<&'a Self>,
}

impl VarNames<'_> {
    fn get(&self, name: &str) -> Option<hir::VarDefId> {
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
        let expected_errors = errors
            .into_iter()
            .map(|(range, kind)| LowerError {
                range: TextRange::new(range.start.into(), range.end.into()),
                kind,
            })
            .collect::<Vec<_>>();

        let parse = parser::parse_repl_line(&lexer::lex(input));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (actual_program, _, actual_errors, _, _) = lower(&root);

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
            "let foo = 92;",
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
            hir::Program { exprs, tail_expr: Some(ten_minus_five_plus_one), ..Default::default() },
            [],
        );
    }

    #[test]
    fn lower_defined_var_ref() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let zero = exprs.alloc(hir::Expr::IntLiteral(0));
        let idx_def = local_defs.alloc(hir::LocalDef { value: zero });
        let idx = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(idx_def)));

        check(
            "let idx = 0; idx",
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::LocalDef(idx_def)],
                tail_expr: Some(idx),
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_undefined_var_or_fnc() {
        let mut exprs = Arena::new();
        let missing = exprs.alloc(hir::Expr::Missing);

        check(
            "user",
            hir::Program { exprs, tail_expr: Some(missing), ..Default::default() },
            [(0..4, LowerErrorKind::UndefinedVarOrFnc { name: "user".to_string() })],
        );
    }

    #[test]
    fn lower_fnc_call() {
        let mut fnc_defs = Arena::new();
        let mut params = Arena::new();
        let mut exprs = Arena::new();

        let x_def = params.alloc(hir::Param { ty: hir::Ty::S32 });
        let y_def = params.alloc(hir::Param { ty: hir::Ty::S32 });
        let x = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Param(x_def)));
        let y = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Param(y_def)));
        let x_plus_y = exprs.alloc(hir::Expr::Bin { lhs: x, rhs: y, op: Some(hir::BinOp::Add) });
        let add_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::new(x_def..=y_def),
            ret_ty: hir::Ty::S32,
            body: x_plus_y,
        });
        let two = exprs.alloc(hir::Expr::IntLiteral(2));
        let five = exprs.alloc(hir::Expr::IntLiteral(5));
        let add_two_five =
            exprs.alloc(hir::Expr::FncCall { def: add_def, args: IdRange::new(two..=five) });

        check(
            r#"
                fnc add(x: s32, y: s32): s32 -> x + y;
                add 2, 5;
            "#,
            hir::Program {
                fnc_defs,
                params,
                exprs,
                defs: vec![hir::Def::FncDef(add_def)],
                stmts: vec![hir::Stmt::Expr(add_two_five)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_zero_arg_fnc_call() {
        let mut fnc_defs = Arena::new();
        let mut exprs = Arena::new();

        let zero_literal = exprs.alloc(hir::Expr::IntLiteral(0));
        let zero_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::default(),
            ret_ty: hir::Ty::S32,
            body: zero_literal,
        });
        let zero = exprs.alloc(hir::Expr::FncCall { def: zero_def, args: IdRange::default() });

        check(
            r#"
                fnc zero(): s32 -> 0;
                zero;
            "#,
            hir::Program {
                fnc_defs,
                exprs,
                defs: vec![hir::Def::FncDef(zero_def)],
                stmts: vec![hir::Stmt::Expr(zero)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn call_fnc_before_def() {
        let mut fnc_defs = Arena::new();
        let mut exprs = Arena::new();

        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new(), None));
        let unit_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::default(),
            ret_ty: hir::Ty::Unit,
            body: empty_block,
        });
        let unit = exprs.alloc(hir::Expr::FncCall { def: unit_def, args: IdRange::default() });

        check(
            r#"
                unit;
                fnc unit() -> {};
            "#,
            hir::Program {
                fnc_defs,
                exprs,
                defs: vec![hir::Def::FncDef(unit_def)],
                stmts: vec![hir::Stmt::Expr(unit)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_int_literal() {
        let mut exprs = Arena::new();
        let ninety_two = exprs.alloc(hir::Expr::IntLiteral(92));

        check("92", hir::Program { exprs, tail_expr: Some(ninety_two), ..Default::default() }, []);
    }

    #[test]
    fn lower_string_literal() {
        let mut exprs = Arena::new();
        let hello = exprs.alloc(hir::Expr::StringLiteral("hello".to_string()));

        check(
            "\"hello\"",
            hir::Program { exprs, tail_expr: Some(hello), ..Default::default() },
            [],
        );
    }

    #[test]
    fn lower_multiple_stmts() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let ten = exprs.alloc(hir::Expr::IntLiteral(10));
        let a_def = local_defs.alloc(hir::LocalDef { value: ten });
        let a = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(a_def)));
        let four = exprs.alloc(hir::Expr::IntLiteral(4));
        let a_minus_four =
            exprs.alloc(hir::Expr::Bin { lhs: a, rhs: four, op: Some(hir::BinOp::Sub) });

        check(
            "let a = 10; a - 4;",
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
            "let = 10;",
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
            hir::Program { exprs, tail_expr: Some(missing), ..Default::default() },
            [],
        );
    }

    #[test]
    fn lower_with_preexisting_local_defs() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let hello = exprs.alloc(hir::Expr::StringLiteral("hello".to_string()));
        let a_def = local_defs.alloc(hir::LocalDef { value: hello });

        let parse = parser::parse_repl_line(&lexer::lex("let a = \"hello\";"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, errors, fnc_names, var_names) = lower(&root);

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
        let a = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(a_def)));

        let parse = parser::parse_repl_line(&lexer::lex("a"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, errors, _, _) =
            lower_with_in_scope(&root, local_defs.clone(), fnc_names, var_names);

        assert_eq!(
            program,
            hir::Program { local_defs, exprs, tail_expr: Some(a), ..Default::default() }
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
        let foo = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(foo_def)));
        let two = exprs.alloc(hir::Expr::IntLiteral(2));
        let foo_plus_two =
            exprs.alloc(hir::Expr::Bin { lhs: foo, rhs: two, op: Some(hir::BinOp::Add) });
        let block =
            exprs.alloc(hir::Expr::Block(vec![hir::Stmt::LocalDef(foo_def)], Some(foo_plus_two)));

        check(
            "{ let foo = 100 - 10; foo + 2 }",
            hir::Program { local_defs, exprs, tail_expr: Some(block), ..Default::default() },
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
        let inner_count = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(inner_count_def)));
        let block = exprs
            .alloc(hir::Expr::Block(vec![hir::Stmt::LocalDef(inner_count_def)], Some(inner_count)));
        let outer_count = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(outer_count_def)));

        check(
            r#"
                let count = 0;
                {
                    let count = "hello there";
                    count
                };
                count;
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
        let a = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(a_def)));
        let b = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(b_def)));
        let a_times_b = exprs.alloc(hir::Expr::Bin { lhs: a, rhs: b, op: Some(hir::BinOp::Mul) });
        let block =
            exprs.alloc(hir::Expr::Block(vec![hir::Stmt::LocalDef(b_def)], Some(a_times_b)));

        check(
            r#"
                let a = 8;
                {
                    let b = 16;
                    a * b
                }
            "#,
            hir::Program {
                local_defs,
                exprs,
                stmts: vec![hir::Stmt::LocalDef(a_def)],
                tail_expr: Some(block),
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_fnc_def_with_no_params_or_ret_ty() {
        let mut fnc_defs = Arena::new();
        let mut exprs = Arena::new();

        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new(), None));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::default(),
            ret_ty: hir::Ty::Unit,
            body: empty_block,
        });

        check(
            "fnc f() -> {};",
            hir::Program {
                fnc_defs,
                exprs,
                defs: vec![hir::Def::FncDef(fnc_def)],
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
        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new(), None));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::new(x..=x),
            ret_ty: hir::Ty::Unit,
            body: empty_block,
        });

        check(
            "fnc drop(x: s32) -> {};",
            hir::Program {
                fnc_defs,
                params,
                exprs,
                defs: vec![hir::Def::FncDef(fnc_def)],
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
        let n = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Param(n_def)));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::new(n_def..=n_def),
            ret_ty: hir::Ty::S32,
            body: n,
        });

        check(
            "fnc id(n: s32): s32 -> n;",
            hir::Program {
                fnc_defs,
                params,
                exprs,
                defs: vec![hir::Def::FncDef(fnc_def)],
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
        let x = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Param(x_def)));
        let y = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Param(y_def)));
        let x_plus_y = exprs.alloc(hir::Expr::Bin { lhs: x, rhs: y, op: Some(hir::BinOp::Add) });
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::new(x_def..=y_def),
            ret_ty: hir::Ty::S32,
            body: x_plus_y,
        });
        let missing = exprs.alloc(hir::Expr::Missing);

        check(
            r#"
                fnc add(x: s32, y: s32): s32 -> x + y;
                x
            "#,
            hir::Program {
                fnc_defs,
                params,
                exprs,
                defs: vec![hir::Def::FncDef(fnc_def)],
                tail_expr: Some(missing),
                ..Default::default()
            },
            [(72..73, LowerErrorKind::UndefinedVarOrFnc { name: "x".to_string() })],
        );
    }

    #[test]
    fn lower_fnc_def_with_missing_param_ty() {
        let mut fnc_defs = Arena::new();
        let mut params = Arena::new();
        let mut exprs = Arena::new();

        let x_def = params.alloc(hir::Param { ty: hir::Ty::Unknown });
        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new(), None));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::new(x_def..=x_def),
            ret_ty: hir::Ty::Unit,
            body: empty_block,
        });

        check(
            "fnc f(x) -> {};",
            hir::Program {
                fnc_defs,
                params,
                exprs,
                defs: vec![hir::Def::FncDef(fnc_def)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn lower_undefined_ty() {
        let mut fnc_defs = Arena::new();
        let mut exprs = Arena::new();

        let empty_block = exprs.alloc(hir::Expr::Block(Vec::new(), None));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::default(),
            ret_ty: hir::Ty::Unknown,
            body: empty_block,
        });

        check(
            "fnc a(): foo -> {};",
            hir::Program {
                fnc_defs,
                exprs,
                defs: vec![hir::Def::FncDef(fnc_def)],
                ..Default::default()
            },
            [(9..12, LowerErrorKind::UndefinedTy { name: "foo".to_string() })],
        );
    }

    #[test]
    fn lower_string_ty() {
        let mut fnc_defs = Arena::new();
        let mut exprs = Arena::new();

        let string = exprs.alloc(hir::Expr::StringLiteral("🦀".to_string()));
        let fnc_def = fnc_defs.alloc(hir::FncDef {
            params: IdRange::default(),
            ret_ty: hir::Ty::String,
            body: string,
        });

        check(
            r#"fnc crab(): string -> "🦀";"#,
            hir::Program {
                fnc_defs,
                exprs,
                defs: vec![hir::Def::FncDef(fnc_def)],
                ..Default::default()
            },
            [],
        );
    }

    #[test]
    fn source_map() {
        let parse = parser::parse_repl_line(&lexer::lex("fnc f(): s32 -> 4; let a = 10; a - 5;"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let mut defs = root.defs();
        let mut stmts = root.stmts();

        let ast::Def::FncDef(fnc_def_ast) = defs.next().unwrap();
        let four_ast = fnc_def_ast.body().unwrap();

        let a_def_ast = match stmts.next().unwrap() {
            ast::Stmt::LocalDef(local_def) => local_def,
            _ => unreachable!(),
        };
        let ten_ast = a_def_ast.value().unwrap();

        let expr_stmt_ast = match stmts.next().unwrap() {
            ast::Stmt::ExprStmt(expr_stmt) => expr_stmt,
            _ => unreachable!(),
        };
        let bin_expr_ast = match expr_stmt_ast.expr() {
            Some(ast::Expr::Bin(bin_expr)) => bin_expr,
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
            params: IdRange::default(),
            ret_ty: hir::Ty::S32,
            body: four_hir,
        });

        let ten_hir = exprs.alloc(hir::Expr::IntLiteral(10));
        let a_def_hir = local_defs.alloc(hir::LocalDef { value: ten_hir });

        let a_hir = exprs.alloc(hir::Expr::VarRef(hir::VarDefId::Local(a_def_hir)));
        let five_hir = exprs.alloc(hir::Expr::IntLiteral(5));
        let bin_expr_hir =
            exprs.alloc(hir::Expr::Bin { lhs: a_hir, rhs: five_hir, op: Some(hir::BinOp::Sub) });

        let (program, source_map, errors, _, _) = lower(&root);

        assert_eq!(
            program,
            hir::Program {
                fnc_defs,
                local_defs,
                exprs,
                defs: vec![hir::Def::FncDef(f_def_hir)],
                stmts: vec![hir::Stmt::LocalDef(a_def_hir), hir::Stmt::Expr(bin_expr_hir)],
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
