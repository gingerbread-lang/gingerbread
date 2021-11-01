use arena::{Arena, ArenaMap};
use std::mem;

#[derive(Default)]
pub struct Evaluator {
    locals: ArenaMap<hir::LocalDefIdx, Val>,
    params: ArenaMap<hir::ParamIdx, Val>,
}

impl Evaluator {
    pub fn eval(&mut self, mut program: hir::Program) -> Val {
        let local_defs = &program.local_defs;
        let exprs = &program.exprs;
        let last_stmt = program.stmts.pop();
        let mut eval_ctx = EvalCtx {
            locals: mem::take(&mut self.locals),
            params: mem::take(&mut self.params),
            local_defs,
            exprs,
        };

        for stmt in program.stmts {
            eval_ctx.eval_stmt(stmt);
        }

        let result = last_stmt.map_or(Val::Nil, |stmt| eval_ctx.eval_stmt(stmt));

        self.locals = eval_ctx.locals;

        result
    }
}

struct EvalCtx<'program> {
    locals: ArenaMap<hir::LocalDefIdx, Val>,
    params: ArenaMap<hir::ParamIdx, Val>,
    local_defs: &'program Arena<hir::LocalDef>,
    exprs: &'program Arena<hir::Expr>,
}

impl EvalCtx<'_> {
    fn eval_stmt(&mut self, stmt: hir::Stmt) -> Val {
        match stmt {
            hir::Stmt::LocalDef(local_def) => self.eval_local_def(local_def),
            hir::Stmt::Expr(expr) => self.eval_expr(expr),
        }
    }

    fn eval_local_def(&mut self, local_def: hir::LocalDefIdx) -> Val {
        let value = self.eval_expr(self.local_defs[local_def].value);
        self.locals.insert(local_def, value);

        Val::Nil
    }

    fn eval_expr(&mut self, expr: hir::ExprIdx) -> Val {
        match &self.exprs[expr] {
            hir::Expr::Missing => Val::Nil,
            hir::Expr::Bin { lhs, rhs, op } => self.eval_bin_expr(*op, *lhs, *rhs),
            hir::Expr::Block(stmts) => match stmts.split_last() {
                Some((last, rest)) => {
                    for stmt in rest {
                        self.eval_stmt(*stmt);
                    }

                    self.eval_stmt(*last)
                }

                None => Val::Nil,
            },
            hir::Expr::VarRef(hir::VarDefIdx::Local(local_def)) => self.locals[*local_def].clone(),
            hir::Expr::VarRef(hir::VarDefIdx::Param(param)) => self.params[*param].clone(),
            hir::Expr::IntLiteral(value) => Val::Int(*value),
            hir::Expr::StringLiteral(value) => Val::String(value.clone()),
        }
    }

    fn eval_bin_expr(
        &mut self,
        op: Option<hir::BinOp>,
        lhs: hir::ExprIdx,
        rhs: hir::ExprIdx,
    ) -> Val {
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
        let (program, _, errors, _) = hir_lower::lower(&root);

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
        check("let a = 5", Val::Nil);
    }

    #[test]
    fn eval_local_def_and_ref() {
        check("let a = 10\na", Val::Int(10));
    }

    #[test]
    fn eval_empty_block() {
        check("{}", Val::Nil);
    }

    #[test]
    fn eval_block_ending_in_stmt() {
        check("{ let foo = 10 }", Val::Nil);
    }

    #[test]
    fn eval_block_ending_in_expr() {
        check("{ 10 - 5 }", Val::Int(5));
    }

    #[test]
    fn eval_block_with_nested_scope() {
        check("{ let n = 5 \n n+2 }", Val::Int(7));
    }

    #[test]
    fn locals_have_lexical_scope() {
        check(
            r#"
                let foo = "foo"
                let bar = {
                    let foo = 10
                    foo
                }
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

        let parse = parser::parse_repl_line(&lexer::lex("let foo = 100"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, _, local_def_names) = hir_lower::lower(&root);
        assert_eq!(evaluator.eval(program.clone()), Val::Nil);

        let parse = parser::parse_repl_line(&lexer::lex("foo"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _, _, _) =
            hir_lower::lower_with_local_defs(&root, program.local_defs, local_def_names);
        assert_eq!(evaluator.eval(program), Val::Int(100));
    }
}
