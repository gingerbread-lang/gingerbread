use arena::{Arena, ArenaMap};
use std::mem;

#[derive(Default)]
pub struct Evaluator {
    locals: ArenaMap<hir::LocalDefId, Val>,
    params: ArenaMap<hir::ParamId, Val>,
}

impl Evaluator {
    pub fn eval(&mut self, program: hir::Program) -> Val {
        let mut eval_ctx = EvalCtx {
            locals: mem::take(&mut self.locals),
            params: mem::take(&mut self.params),
            local_defs: &program.local_defs,
            fnc_defs: &program.fnc_defs,
            exprs: &program.exprs,
        };

        for stmt in program.stmts {
            eval_ctx.eval_stmt(stmt);
        }

        let result = program.tail_expr.map_or(Val::Nil, |expr| eval_ctx.eval_expr(expr));

        self.locals = eval_ctx.locals;

        result
    }
}

#[derive(Debug)]
struct EvalCtx<'program> {
    locals: ArenaMap<hir::LocalDefId, Val>,
    params: ArenaMap<hir::ParamId, Val>,
    local_defs: &'program Arena<hir::LocalDef>,
    fnc_defs: &'program Arena<hir::FncDef>,
    exprs: &'program Arena<hir::Expr>,
}

impl EvalCtx<'_> {
    fn eval_stmt(&mut self, stmt: hir::Stmt) {
        match stmt {
            hir::Stmt::LocalDef(local_def) => self.eval_local_def(local_def),
            hir::Stmt::Expr(expr) => {
                self.eval_expr(expr);
            }
        }
    }

    fn eval_local_def(&mut self, local_def: hir::LocalDefId) {
        let value = self.eval_expr(self.local_defs[local_def].value);
        self.locals.insert(local_def, value);
    }

    fn eval_expr(&mut self, expr: hir::ExprId) -> Val {
        match &self.exprs[expr] {
            hir::Expr::Missing => Val::Nil,
            hir::Expr::Bin { lhs, rhs, op } => self.eval_bin_expr(*op, *lhs, *rhs),
            hir::Expr::FncCall { def, args } => {
                let fnc_def = &self.fnc_defs[*def];

                for (param, &arg) in fnc_def.params.clone().zip(args) {
                    let arg = self.eval_expr(arg);
                    self.params.insert(param, arg);
                }

                self.eval_expr(fnc_def.body)
            }
            hir::Expr::Block { stmts, tail_expr } => {
                for stmt in stmts {
                    self.eval_stmt(*stmt);
                }

                match tail_expr {
                    Some(tail_expr) => self.eval_expr(*tail_expr),
                    None => Val::Nil,
                }
            }
            hir::Expr::VarRef(hir::VarDefId::Local(local_def)) => self.locals[*local_def].clone(),
            hir::Expr::VarRef(hir::VarDefId::Param(param)) => self.params[*param].clone(),
            hir::Expr::IntLiteral(value) => Val::Int(*value),
            hir::Expr::StringLiteral(value) => Val::String(value.clone()),
        }
    }

    fn eval_bin_expr(&mut self, op: Option<hir::BinOp>, lhs: hir::ExprId, rhs: hir::ExprId) -> Val {
        let op = match op {
            Some(op) => op,
            None => return Val::Nil,
        };

        let (lhs, rhs) = match (self.eval_expr(lhs), self.eval_expr(rhs)) {
            (Val::Int(lhs), Val::Int(rhs)) => (lhs, rhs),
            _ => return Val::Nil,
        };

        match op {
            hir::BinOp::Add => lhs.checked_add(rhs),
            hir::BinOp::Sub => lhs.checked_sub(rhs),
            hir::BinOp::Mul => lhs.checked_mul(rhs),
            hir::BinOp::Div => lhs.checked_div(rhs),
        }
        .map_or(Val::Nil, Val::Int)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Nil,
    Int(u32),
    String(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;

    fn check(input: &str, val: Val) {
        let parse = parser::parse_repl_line(&lexer::lex(input));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, errors, _, _) = hir_lower::lower(&root);

        assert_eq!(Evaluator::default().eval(program), val);
        assert!(errors.is_empty());
    }

    #[test]
    fn eval_int_literal() {
        check("92", Val::Int(92));
    }

    #[test]
    fn eval_string_literal() {
        check("\"foo\"", Val::String("foo".to_string()));
    }

    #[test]
    fn eval_bin_expr() {
        check("10 * 5", Val::Int(50));
    }

    #[test]
    fn eval_local_def() {
        check("let a = 5;", Val::Nil);
    }

    #[test]
    fn eval_local_def_and_ref() {
        check("let a = 10; a", Val::Int(10));
    }

    #[test]
    fn eval_empty_block() {
        check("{}", Val::Nil);
    }

    #[test]
    fn eval_block_ending_in_stmt() {
        check("{ let foo = 10; }", Val::Nil);
    }

    #[test]
    fn eval_block_ending_in_expr() {
        check("{ 10 - 5 }", Val::Int(5));
    }

    #[test]
    fn eval_block_with_nested_scope() {
        check("{ let n = 5; n+2 }", Val::Int(7));
    }

    #[test]
    fn locals_have_lexical_scope() {
        check(
            r#"
                let foo = "foo";
                let bar = {
                    let foo = 10;
                    foo
                };
                foo
            "#,
            Val::String("foo".to_string()),
        );
    }

    #[test]
    fn add_with_overflow() {
        check("3000000000 + 3000000000", Val::Nil);
    }

    #[test]
    fn subtract_from_zero() {
        check("0 - 10", Val::Nil);
    }

    #[test]
    fn subtract_with_overflow() {
        check("10 - 20", Val::Nil);
    }

    #[test]
    fn multiply_with_overflow() {
        check("1000000000 * 5", Val::Nil);
    }

    #[test]
    fn divide_by_zero() {
        check("0 / 0", Val::Nil);
    }

    #[test]
    fn preserve_locals_across_eval_calls() {
        let mut evaluator = Evaluator::default();

        let parse = parser::parse_repl_line(&lexer::lex("let foo = 100;"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, _, fnc_names, var_names) = hir_lower::lower(&root);
        assert_eq!(evaluator.eval(program.clone()), Val::Nil);

        let parse = parser::parse_repl_line(&lexer::lex("foo"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, ..) = hir_lower::lower_with_in_scope(&root, program, fnc_names, var_names);
        assert_eq!(evaluator.eval(program), Val::Int(100));
    }

    #[test]
    fn eval_fnc_call_with_zero_args() {
        check("fnc magic_number -> 3735928559; magic_number", Val::Int(0xdeadbeef));
    }

    #[test]
    fn eval_fnc_call_with_one_arg() {
        check(
            r#"
                fnc id(x: s32): s32 -> x;
                id 10
            "#,
            Val::Int(10),
        );
    }

    #[test]
    fn eval_fnc_call_with_multiple_args() {
        check(
            r#"
                fnc add(x: s32, y: s32, z: s32): s32 -> x + y + z;
                add 29, 31, 32
            "#,
            Val::Int(92),
        );
    }

    #[test]
    fn eval_nested_fnc_call() {
        check(
            r#"
                fnc mul(n: s32, m: s32): s32 -> n * m;
                mul {mul 2, 6}, 8
            "#,
            Val::Int(96),
        );
    }

    #[test]
    fn eval_fnc_call_with_multiple_contained_exprs_in_last_arg() {
        check(
            r#"
                fnc second(elem1: s32, elem2: s32): s32 -> elem2;
                second 1, 4 * 5
            "#,
            Val::Int(20),
        );
    }

    #[test]
    fn preserve_fnc_defs_across_eval_calls() {
        let mut evaluator = Evaluator::default();

        let parse = parser::parse_source_file(&lexer::lex(
            "fnc div(dividend: s32, divisor: s32): s32 -> dividend / divisor;",
        ));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, _, fnc_names, var_names) = hir_lower::lower(&root);
        assert_eq!(evaluator.eval(program.clone()), Val::Nil);

        let parse = parser::parse_repl_line(&lexer::lex("div 10, 2"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, ..) = hir_lower::lower_with_in_scope(&root, program, fnc_names, var_names);
        assert_eq!(evaluator.eval(program), Val::Int(5));
    }
}
