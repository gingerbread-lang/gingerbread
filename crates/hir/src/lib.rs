pub mod summary;

use arena::{Arena, Id, IdRange};

pub type ExprId = Id<Expr>;
pub type LocalDefId = Id<LocalDef>;
pub type FncDefId = Id<FncDef>;
pub type ParamId = Id<Param>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub local_defs: Arena<LocalDef>,
    pub fnc_defs: Arena<FncDef>,
    pub params: Arena<Param>,
    pub exprs: Arena<Expr>,
    pub defs: Vec<Def>,
    pub stmts: Vec<Stmt>,
    pub tail_expr: Option<ExprId>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Def {
    FncDef(FncDefId),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Stmt {
    LocalDef(LocalDefId),
    Expr(ExprId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FncDef {
    pub params: IdRange<Param>,
    pub ret_ty: Ty,
    pub body: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalDef {
    pub value: ExprId,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Param {
    pub ty: Ty,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ty {
    Unknown,
    Unit,
    S32,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Missing,
    Bin { lhs: ExprId, rhs: ExprId, op: Option<BinOp> },
    FncCall { def: FncDefId, args: Vec<ExprId> },
    Block { stmts: Vec<Stmt>, tail_expr: Option<ExprId> },
    VarRef(VarDefId),
    IntLiteral(u32),
    StringLiteral(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarDefId {
    Local(LocalDefId),
    Param(ParamId),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}
