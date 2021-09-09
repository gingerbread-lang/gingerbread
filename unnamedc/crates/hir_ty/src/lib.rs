use la_arena::Arena;
use std::collections::HashMap;

pub fn infer(program: &hir::Program) -> InferResult {
    infer_with_var_tys(program, HashMap::new())
}

pub fn infer_with_var_tys(program: &hir::Program, var_tys: HashMap<String, Ty>) -> InferResult {
    let mut infer_ctx = InferCtx {
        result: InferResult { expr_tys: HashMap::new(), var_tys, errors: Vec::new() },
        exprs: &program.exprs,
    };

    for stmt in &program.stmts {
        infer_ctx.infer_stmt(stmt);
    }

    infer_ctx.result
}

#[derive(Debug)]
pub struct InferResult {
    pub expr_tys: HashMap<hir::ExprIdx, Ty>,
    pub var_tys: HashMap<String, Ty>,
    pub errors: Vec<TyError>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ty {
    Unknown,
    Int,
    String,
}

#[derive(Debug, PartialEq)]
pub struct TyError {
    expr: hir::ExprIdx,
    kind: TyErrorKind,
}

#[derive(Debug, PartialEq)]
enum TyErrorKind {
    Mismatch { expected: Ty, found: Ty },
    UndefinedVar,
}

struct InferCtx<'a> {
    result: InferResult,
    exprs: &'a Arena<hir::Expr>,
}

impl InferCtx<'_> {
    fn infer_stmt(&mut self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::VarDef(var_def) => {
                let value_ty = self.infer_expr(var_def.value);

                if let hir::Name(Some(name)) = &var_def.name {
                    self.result.var_tys.insert(name.clone(), value_ty);
                }
            }
            hir::Stmt::Expr(expr) => {
                self.infer_expr(*expr);
            }
        }
    }

    fn infer_expr(&mut self, expr: hir::ExprIdx) -> Ty {
        let ty = match self.exprs[expr] {
            hir::Expr::Bin { lhs, rhs, .. } => {
                let lhs_ty = self.infer_expr(lhs);
                let rhs_ty = self.infer_expr(rhs);

                for (expr, ty) in [(lhs, lhs_ty), (rhs, rhs_ty)] {
                    if ty != Ty::Int {
                        self.result.errors.push(TyError {
                            expr,
                            kind: TyErrorKind::Mismatch { expected: Ty::Int, found: ty },
                        });
                    }
                }

                Ty::Int
            }

            hir::Expr::VarRef { ref name } => match name {
                hir::Name(Some(name)) => match self.result.var_tys.get(name.as_str()) {
                    Some(ty) => *ty,
                    None => {
                        self.result.errors.push(TyError { expr, kind: TyErrorKind::UndefinedVar });
                        Ty::Unknown
                    }
                },
                hir::Name(None) => Ty::Unknown,
            },

            hir::Expr::IntLiteral { .. } => Ty::Int,

            hir::Expr::StringLiteral { .. } => Ty::String,

            _ => todo!(),
        };

        self.result.expr_tys.insert(expr, ty);

        ty
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn infer_int_literal() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(hir::Expr::IntLiteral { value: Some(10) });

        let program = hir::Program { exprs, stmts: vec![hir::Stmt::Expr(ten)] };
        let result = infer(&program);

        assert_eq!(result.expr_tys[&ten], Ty::Int);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_string_literal() {
        let mut exprs = Arena::new();
        let hello = exprs.alloc(hir::Expr::StringLiteral { value: Some("hello".to_string()) });

        let program = hir::Program { exprs, stmts: vec![hir::Stmt::Expr(hello)] };
        let result = infer(&program);

        assert_eq!(result.expr_tys[&hello], Ty::String);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_bin_expr_on_ints() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(hir::Expr::IntLiteral { value: Some(10) });
        let twenty = exprs.alloc(hir::Expr::IntLiteral { value: Some(20) });
        let ten_times_twenty =
            exprs.alloc(hir::Expr::Bin { lhs: ten, rhs: twenty, op: Some(hir::BinOp::Mul) });

        let program = hir::Program { exprs, stmts: vec![hir::Stmt::Expr(ten_times_twenty)] };
        let result = infer(&program);

        assert_eq!(result.expr_tys[&ten], Ty::Int);
        assert_eq!(result.expr_tys[&twenty], Ty::Int);
        assert_eq!(result.expr_tys[&ten_times_twenty], Ty::Int);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_bin_expr_on_string_and_int() {
        let mut exprs = Arena::new();
        let string = exprs.alloc(hir::Expr::StringLiteral { value: Some("100".to_string()) });
        let int = exprs.alloc(hir::Expr::IntLiteral { value: Some(7) });
        let bin_expr =
            exprs.alloc(hir::Expr::Bin { lhs: string, rhs: int, op: Some(hir::BinOp::Sub) });

        let program = hir::Program { exprs, stmts: vec![hir::Stmt::Expr(bin_expr)] };
        let result = infer(&program);

        assert_eq!(result.expr_tys[&string], Ty::String);
        assert_eq!(result.expr_tys[&int], Ty::Int);
        assert_eq!(result.expr_tys[&bin_expr], Ty::Int);
        assert_eq!(
            result.errors,
            [TyError {
                expr: string,
                kind: TyErrorKind::Mismatch { expected: Ty::Int, found: Ty::String }
            }]
        );
    }

    #[test]
    fn infer_undefined_var_ref() {
        let mut exprs = Arena::new();
        let a = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("a".to_string())) });

        let program = hir::Program { exprs, stmts: vec![hir::Stmt::Expr(a)] };
        let result = infer(&program);

        assert_eq!(result.expr_tys[&a], Ty::Unknown);
        assert_eq!(result.errors, [TyError { expr: a, kind: TyErrorKind::UndefinedVar }]);
    }

    #[test]
    fn infer_var_def() {
        let mut exprs = Arena::new();
        let two = exprs.alloc(hir::Expr::IntLiteral { value: Some(2) });

        let program = hir::Program {
            exprs,
            stmts: vec![hir::Stmt::VarDef(hir::VarDef {
                name: hir::Name(Some("foo".to_string())),
                value: two,
            })],
        };
        let result = infer(&program);

        assert_eq!(result.expr_tys[&two], Ty::Int);
        assert_eq!(result.var_tys["foo"], Ty::Int);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_chain_of_var_refs_and_defs() {
        let mut exprs = Arena::new();
        let string = exprs.alloc(hir::Expr::StringLiteral { value: Some("test".to_string()) });
        let a = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("a".to_string())) });
        let b = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("b".to_string())) });
        let c = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("c".to_string())) });

        let program = hir::Program {
            exprs,
            stmts: vec![
                hir::Stmt::VarDef(hir::VarDef {
                    name: hir::Name(Some("a".to_string())),
                    value: string,
                }),
                hir::Stmt::VarDef(hir::VarDef { name: hir::Name(Some("b".to_string())), value: a }),
                hir::Stmt::VarDef(hir::VarDef { name: hir::Name(Some("c".to_string())), value: b }),
                hir::Stmt::Expr(c),
            ],
        };
        let result = infer(&program);

        assert_eq!(result.expr_tys[&string], Ty::String);
        assert_eq!(result.var_tys["a"], Ty::String);
        assert_eq!(result.expr_tys[&a], Ty::String);
        assert_eq!(result.var_tys["b"], Ty::String);
        assert_eq!(result.expr_tys[&b], Ty::String);
        assert_eq!(result.var_tys["c"], Ty::String);
        assert_eq!(result.expr_tys[&c], Ty::String);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_with_preserved_var_tys() {
        let preserved_var_tys = {
            let mut exprs = Arena::new();
            let six = exprs.alloc(hir::Expr::IntLiteral { value: Some(6) });

            let program = hir::Program {
                exprs,
                stmts: vec![hir::Stmt::VarDef(hir::VarDef {
                    name: hir::Name(Some("idx".to_string())),
                    value: six,
                })],
            };
            let result = infer(&program);

            assert_eq!(result.expr_tys[&six], Ty::Int);
            assert_eq!(result.var_tys["idx"], Ty::Int);
            assert_eq!(result.errors, []);

            result.var_tys
        };

        let mut exprs = Arena::new();
        let idx = exprs.alloc(hir::Expr::VarRef { name: hir::Name(Some("idx".to_string())) });

        let program = hir::Program { exprs, stmts: vec![hir::Stmt::Expr(idx)] };
        let result = infer_with_var_tys(&program, preserved_var_tys);

        assert_eq!(result.expr_tys[&idx], Ty::Int);
        assert_eq!(result.var_tys["idx"], Ty::Int);
        assert_eq!(result.errors, []);
    }
}
