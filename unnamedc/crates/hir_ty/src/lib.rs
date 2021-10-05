use la_arena::{Arena, ArenaMap};

pub fn infer(program: &hir::Program) -> InferResult {
    infer_in_scope(program, InScope::default())
}

pub fn infer_in_scope(program: &hir::Program, in_scope: InScope) -> InferResult {
    let mut infer_ctx = InferCtx {
        result: InferResult {
            expr_tys: ArenaMap::default(),
            local_tys: in_scope.local_tys,
            errors: Vec::new(),
        },
        local_defs: &program.local_defs,
        exprs: &program.exprs,
    };

    for stmt in &program.stmts {
        infer_ctx.infer_stmt(stmt);
    }

    infer_ctx.result
}

#[derive(Debug)]
pub struct InferResult {
    local_tys: ArenaMap<hir::LocalDefIdx, Ty>,
    expr_tys: ArenaMap<hir::ExprIdx, Ty>,
    errors: Vec<TyError>,
}

impl InferResult {
    pub fn in_scope(self) -> (InScope, Vec<TyError>) {
        (InScope { local_tys: self.local_tys }, self.errors)
    }
}

#[derive(Debug, Clone, Default)]
pub struct InScope {
    local_tys: ArenaMap<hir::LocalDefIdx, Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ty {
    Unknown,
    Int,
    String,
    Unit,
}

#[derive(Debug, PartialEq)]
pub struct TyError {
    pub expr: hir::ExprIdx,
    pub kind: TyErrorKind,
}

#[derive(Debug, PartialEq)]
pub enum TyErrorKind {
    Mismatch { expected: Ty, found: Ty },
}

struct InferCtx<'a> {
    result: InferResult,
    local_defs: &'a Arena<hir::LocalDef>,
    exprs: &'a Arena<hir::Expr>,
}

impl InferCtx<'_> {
    fn infer_stmt(&mut self, stmt: &hir::Stmt) -> Ty {
        match stmt {
            hir::Stmt::LocalDef(local_def) => {
                let value_ty = self.infer_expr(self.local_defs[*local_def].value);
                self.result.local_tys.insert(*local_def, value_ty);

                Ty::Unit
            }
            hir::Stmt::Expr(expr) => self.infer_expr(*expr),
        }
    }

    fn infer_expr(&mut self, expr: hir::ExprIdx) -> Ty {
        let ty = match self.exprs[expr] {
            hir::Expr::Missing => Ty::Unknown,

            hir::Expr::Bin { lhs, rhs, .. } => {
                let lhs_ty = self.infer_expr(lhs);
                let rhs_ty = self.infer_expr(rhs);

                let is_lhs_missing = self.exprs[lhs] == hir::Expr::Missing;
                let is_rhs_missing = self.exprs[rhs] == hir::Expr::Missing;
                let is_anything_missing = is_lhs_missing || is_rhs_missing;
                if !is_anything_missing {
                    for (expr, ty) in [(lhs, lhs_ty), (rhs, rhs_ty)] {
                        if ty != Ty::Int {
                            self.result.errors.push(TyError {
                                expr,
                                kind: TyErrorKind::Mismatch { expected: Ty::Int, found: ty },
                            });
                        }
                    }
                }

                Ty::Int
            }

            hir::Expr::Block(ref stmts) => match stmts.split_last() {
                Some((last, rest)) => {
                    for stmt in rest {
                        self.infer_stmt(stmt);
                    }

                    self.infer_stmt(last)
                }

                None => Ty::Unit,
            },

            hir::Expr::VarRef(hir::VarDefIdx::Local(local_def)) => self.result.local_tys[local_def],

            hir::Expr::IntLiteral(_) => Ty::Int,

            hir::Expr::StringLiteral(_) => Ty::String,
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
        let ten = exprs.alloc(hir::Expr::IntLiteral(10));

        let result = infer(&hir::Program {
            local_defs: Arena::new(),
            exprs,
            stmts: vec![hir::Stmt::Expr(ten)],
        });

        assert_eq!(result.expr_tys[ten], Ty::Int);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_string_literal() {
        let mut exprs = Arena::new();
        let hello = exprs.alloc(hir::Expr::StringLiteral("hello".to_string()));

        let result = infer(&hir::Program {
            local_defs: Arena::new(),
            exprs,
            stmts: vec![hir::Stmt::Expr(hello)],
        });

        assert_eq!(result.expr_tys[hello], Ty::String);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_bin_expr_on_ints() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(hir::Expr::IntLiteral(10));
        let twenty = exprs.alloc(hir::Expr::IntLiteral(20));
        let ten_times_twenty =
            exprs.alloc(hir::Expr::Bin { lhs: ten, rhs: twenty, op: Some(hir::BinOp::Mul) });

        let result = infer(&hir::Program {
            local_defs: Arena::new(),
            exprs,
            stmts: vec![hir::Stmt::Expr(ten_times_twenty)],
        });

        assert_eq!(result.expr_tys[ten], Ty::Int);
        assert_eq!(result.expr_tys[twenty], Ty::Int);
        assert_eq!(result.expr_tys[ten_times_twenty], Ty::Int);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_bin_expr_on_string_and_int() {
        let mut exprs = Arena::new();
        let string = exprs.alloc(hir::Expr::StringLiteral("100".to_string()));
        let int = exprs.alloc(hir::Expr::IntLiteral(7));
        let bin_expr =
            exprs.alloc(hir::Expr::Bin { lhs: string, rhs: int, op: Some(hir::BinOp::Sub) });

        let result = infer(&hir::Program {
            local_defs: Arena::new(),
            exprs,
            stmts: vec![hir::Stmt::Expr(bin_expr)],
        });

        assert_eq!(result.expr_tys[string], Ty::String);
        assert_eq!(result.expr_tys[int], Ty::Int);
        assert_eq!(result.expr_tys[bin_expr], Ty::Int);
        assert_eq!(
            result.errors,
            [TyError {
                expr: string,
                kind: TyErrorKind::Mismatch { expected: Ty::Int, found: Ty::String }
            }]
        );
    }

    #[test]
    fn infer_local_def() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let two = exprs.alloc(hir::Expr::IntLiteral(2));
        let local_def = local_defs.alloc(hir::LocalDef { value: two });

        let result =
            infer(&hir::Program { local_defs, exprs, stmts: vec![hir::Stmt::LocalDef(local_def)] });

        assert_eq!(result.expr_tys[two], Ty::Int);
        assert_eq!(result.local_tys[local_def], Ty::Int);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_chain_of_var_refs_and_defs() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let string = exprs.alloc(hir::Expr::StringLiteral("test".to_string()));
        let a_def = local_defs.alloc(hir::LocalDef { value: string });
        let a = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(a_def)));
        let b_def = local_defs.alloc(hir::LocalDef { value: a });
        let b = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(b_def)));
        let c_def = local_defs.alloc(hir::LocalDef { value: b });
        let c = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(c_def)));

        let result = infer(&hir::Program {
            local_defs,
            exprs,
            stmts: vec![
                hir::Stmt::LocalDef(a_def),
                hir::Stmt::LocalDef(b_def),
                hir::Stmt::LocalDef(c_def),
                hir::Stmt::Expr(c),
            ],
        });

        assert_eq!(result.expr_tys[string], Ty::String);
        assert_eq!(result.local_tys[a_def], Ty::String);
        assert_eq!(result.expr_tys[a], Ty::String);
        assert_eq!(result.local_tys[b_def], Ty::String);
        assert_eq!(result.expr_tys[b], Ty::String);
        assert_eq!(result.local_tys[c_def], Ty::String);
        assert_eq!(result.expr_tys[c], Ty::String);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_with_preserved_in_scope() {
        let (in_scope, local_defs, local_def) = {
            let mut local_defs = Arena::new();
            let mut exprs = Arena::new();

            let six = exprs.alloc(hir::Expr::IntLiteral(6));
            let local_def = local_defs.alloc(hir::LocalDef { value: six });

            let result = infer(&hir::Program {
                local_defs: local_defs.clone(),
                exprs,
                stmts: vec![hir::Stmt::LocalDef(local_def)],
            });

            assert_eq!(result.expr_tys[six], Ty::Int);
            assert_eq!(result.local_tys[local_def], Ty::Int);
            assert_eq!(result.errors, []);

            (result.in_scope().0, local_defs, local_def)
        };

        let mut exprs = Arena::new();
        let local_value = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(local_def)));

        let program = hir::Program { local_defs, exprs, stmts: vec![hir::Stmt::Expr(local_value)] };
        let result = infer_in_scope(&program, in_scope);

        assert_eq!(result.expr_tys[local_value], Ty::Int);
        assert_eq!(result.local_tys[local_def], Ty::Int);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_missing_expr() {
        let mut exprs = Arena::new();
        let missing = exprs.alloc(hir::Expr::Missing);

        let result = infer(&hir::Program {
            local_defs: Arena::new(),
            exprs,
            stmts: vec![hir::Stmt::Expr(missing)],
        });

        assert_eq!(result.expr_tys[missing], Ty::Unknown);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn only_error_on_missing_expr_use() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let missing = exprs.alloc(hir::Expr::Missing);
        let user_def = local_defs.alloc(hir::LocalDef { value: missing });
        let user = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(user_def)));
        let four = exprs.alloc(hir::Expr::IntLiteral(4));
        let user_plus_four =
            exprs.alloc(hir::Expr::Bin { lhs: user, rhs: four, op: Some(hir::BinOp::Add) });

        let result = infer(&hir::Program {
            local_defs,
            exprs,
            stmts: vec![hir::Stmt::LocalDef(user_def), hir::Stmt::Expr(user_plus_four)],
        });

        assert_eq!(result.expr_tys[missing], Ty::Unknown);
        assert_eq!(result.expr_tys[user], Ty::Unknown);
        assert_eq!(result.expr_tys[four], Ty::Int);
        assert_eq!(result.expr_tys[user_plus_four], Ty::Int);
        assert_eq!(result.local_tys[user_def], Ty::Unknown);

        // we only get an error about `user`’s type not being known
        // until we try to do an operation with it
        assert_eq!(
            result.errors,
            [TyError {
                expr: user,
                kind: TyErrorKind::Mismatch { expected: Ty::Int, found: Ty::Unknown }
            }]
        );
    }

    #[test]
    fn dont_error_on_missing_expr_in_bin_expr() {
        let mut exprs = Arena::new();
        let ten = exprs.alloc(hir::Expr::IntLiteral(10));
        let missing = exprs.alloc(hir::Expr::Missing);
        let ten_times_missing =
            exprs.alloc(hir::Expr::Bin { lhs: ten, rhs: missing, op: Some(hir::BinOp::Mul) });

        let result = infer(&hir::Program {
            local_defs: Arena::new(),
            exprs,
            stmts: vec![hir::Stmt::Expr(ten_times_missing)],
        });

        assert_eq!(result.expr_tys[ten], Ty::Int);
        assert_eq!(result.expr_tys[missing], Ty::Unknown);
        assert_eq!(result.expr_tys[ten_times_missing], Ty::Int);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_empty_block() {
        let mut exprs = Arena::new();
        let block = exprs.alloc(hir::Expr::Block(Vec::new()));

        let result = infer(&hir::Program {
            local_defs: Arena::new(),
            exprs,
            stmts: vec![hir::Stmt::Expr(block)],
        });

        assert_eq!(result.expr_tys[block], Ty::Unit);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_block_ending_in_local_def() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let string = exprs.alloc(hir::Expr::StringLiteral("🌈".to_string()));
        let local_def = local_defs.alloc(hir::LocalDef { value: string });
        let block = exprs.alloc(hir::Expr::Block(vec![hir::Stmt::LocalDef(local_def)]));

        let result =
            infer(&hir::Program { local_defs, exprs, stmts: vec![hir::Stmt::Expr(block)] });

        assert_eq!(result.expr_tys[string], Ty::String);
        assert_eq!(result.local_tys[local_def], Ty::String);
        assert_eq!(result.expr_tys[block], Ty::Unit);
        assert_eq!(result.errors, []);
    }

    #[test]
    fn infer_block_ending_in_expr() {
        let mut local_defs = Arena::new();
        let mut exprs = Arena::new();

        let seven = exprs.alloc(hir::Expr::IntLiteral(7));
        let num_def = local_defs.alloc(hir::LocalDef { value: seven });
        let num = exprs.alloc(hir::Expr::VarRef(hir::VarDefIdx::Local(num_def)));
        let block =
            exprs.alloc(hir::Expr::Block(vec![hir::Stmt::LocalDef(num_def), hir::Stmt::Expr(num)]));

        let result =
            infer(&hir::Program { local_defs, exprs, stmts: vec![hir::Stmt::Expr(block)] });

        assert_eq!(result.expr_tys[seven], Ty::Int);
        assert_eq!(result.local_tys[num_def], Ty::Int);
        assert_eq!(result.expr_tys[num], Ty::Int);
        assert_eq!(result.expr_tys[block], Ty::Int);
        assert_eq!(result.errors, []);
    }
}
