use la_arena::Arena;
use std::collections::HashMap;

#[derive(Default)]
pub struct Evaluator {
    vars: HashMap<String, Val>,
}

impl Evaluator {
    pub fn eval(&mut self, mut program: hir::Program) -> Val {
        let exprs = &program.exprs;
        let last_stmt = program.stmts.pop();

        for stmt in program.stmts {
            self.eval_stmt(stmt, exprs);
        }

        last_stmt.map_or(Val::Nil, |stmt| self.eval_stmt(stmt, exprs))
    }

    fn eval_stmt(&mut self, stmt: hir::Stmt, exprs: &Arena<hir::Expr>) -> Val {
        match stmt {
            hir::Stmt::VarDef(var_def) => self.eval_var_def(var_def, exprs),
            hir::Stmt::Expr(expr) => self.eval_expr(expr, exprs),
        }
    }

    fn eval_var_def(&mut self, var_def: hir::VarDef, exprs: &Arena<hir::Expr>) -> Val {
        if let hir::Name(Some(name)) = var_def.name {
            let value = self.eval_expr(var_def.value, exprs);
            self.vars.insert(name, value);
        }

        Val::Nil
    }

    fn eval_expr(&mut self, expr: hir::ExprIdx, exprs: &Arena<hir::Expr>) -> Val {
        match &exprs[expr] {
            hir::Expr::Missing => Val::Nil,
            hir::Expr::Bin { lhs, rhs, op } => self.eval_bin_expr(*op, *lhs, *rhs, exprs),
            hir::Expr::VarRef { name } => {
                name.0.as_ref().and_then(|name| self.vars.get(name)).cloned().unwrap_or(Val::Nil)
            }
            hir::Expr::IntLiteral { value } => value.map_or(Val::Nil, Val::Int),
            hir::Expr::StringLiteral { value } => value.clone().map_or(Val::Nil, Val::String),
        }
    }

    fn eval_bin_expr(
        &mut self,
        op: Option<hir::BinOp>,
        lhs: hir::ExprIdx,
        rhs: hir::ExprIdx,
        exprs: &Arena<hir::Expr>,
    ) -> Val {
        let op = if let Some(op) = op {
            op
        } else {
            return Val::Nil;
        };

        let (lhs, rhs) = match (self.eval_expr(lhs, exprs), self.eval_expr(rhs, exprs)) {
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
        let parse = parser::parse(lexer::lex(input));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _) = hir_lower::lower(&root);

        assert_eq!(Evaluator::default().eval(program), val);
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
    fn eval_var_def() {
        check("let a = 5", Val::Nil);
    }

    #[test]
    fn eval_undefined_var_ref() {
        check("foo", Val::Nil);
    }

    #[test]
    fn eval_var_def_and_var_ref() {
        check("let a = 10\na", Val::Int(10));
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
    fn preserve_variables_across_eval_calls() {
        let mut evaluator = Evaluator::default();

        let parse = parser::parse(lexer::lex("let foo = 100"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _) = hir_lower::lower(&root);
        assert_eq!(evaluator.eval(program), Val::Nil);

        let parse = parser::parse(lexer::lex("foo"));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (program, _) = hir_lower::lower(&root);
        assert_eq!(evaluator.eval(program), Val::Int(100));
    }
}
