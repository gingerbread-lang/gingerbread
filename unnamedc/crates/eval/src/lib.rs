use std::collections::HashMap;

pub fn eval(mut program: hir::Program) -> Val {
    let mut evaluator = Evaluator::default();

    let last_stmt = program.stmts.pop();

    for stmt in program.stmts {
        evaluator.eval_stmt(stmt);
    }

    last_stmt.map_or(Val::Nil, |stmt| evaluator.eval_stmt(stmt))
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Val {
    Nil,
    Int(u32),
}

#[derive(Default)]
struct Evaluator {
    vars: HashMap<String, Val>,
}

impl Evaluator {
    fn eval_stmt(&mut self, stmt: hir::Stmt) -> Val {
        match stmt {
            hir::Stmt::VarDef(var_def) => self.eval_var_def(var_def),
            hir::Stmt::Expr(expr) => self.eval_expr(expr),
        }
    }

    fn eval_var_def(&mut self, var_def: hir::VarDef) -> Val {
        if let hir::Name(Some(name)) = var_def.name {
            let value = self.eval_expr(var_def.value);
            self.vars.insert(name, value);
        }

        Val::Nil
    }

    fn eval_expr(&mut self, expr: hir::Expr) -> Val {
        match expr {
            hir::Expr::Missing => Val::Nil,
            hir::Expr::Bin { lhs, rhs, op } => self.eval_bin_expr(op, *lhs, *rhs),
            hir::Expr::VarRef { name } => {
                name.0.and_then(|name| self.vars.get(&name)).copied().unwrap_or(Val::Nil)
            }
            hir::Expr::IntLiteral { value } => value.map_or(Val::Nil, Val::Int),
        }
    }

    fn eval_bin_expr(&mut self, op: Option<hir::BinOp>, lhs: hir::Expr, rhs: hir::Expr) -> Val {
        let op = if let Some(op) = op {
            op
        } else {
            return Val::Nil;
        };

        let (lhs, rhs) = match (self.eval_expr(lhs), self.eval_expr(rhs)) {
            (Val::Int(lhs), Val::Int(rhs)) => (lhs, rhs),
            _ => return Val::Nil,
        };

        let output = match op {
            hir::BinOp::Add => lhs + rhs,
            hir::BinOp::Sub => lhs - rhs,
            hir::BinOp::Mul => lhs * rhs,
            hir::BinOp::Div => lhs / rhs,
        };

        Val::Int(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;

    fn check(input: &str, val: Val) {
        let parse = parser::parse(lexer::lex(input));
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let program = hir_lower::lower(&root);

        assert_eq!(eval(program), val);
    }

    #[test]
    fn eval_int_literal() {
        check("92", Val::Int(92));
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
}
