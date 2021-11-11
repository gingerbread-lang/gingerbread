use arena::{Arena, Idx, IdxRange};

pub type ExprIdx = Idx<Expr>;
pub type LocalDefIdx = Idx<LocalDef>;
pub type FncDefIdx = Idx<FncDef>;
pub type ParamIdx = Idx<Param>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub local_defs: Arena<LocalDef>,
    pub fnc_defs: Arena<FncDef>,
    pub params: Arena<Param>,
    pub exprs: Arena<Expr>,
    pub defs: Vec<Def>,
    pub stmts: Vec<Stmt>,
    pub tail_expr: Option<ExprIdx>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Def {
    FncDef(FncDefIdx),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Stmt {
    LocalDef(LocalDefIdx),
    Expr(ExprIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FncDef {
    pub params: IdxRange<Param>,
    pub ret_ty: Ty,
    pub body: ExprIdx,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalDef {
    pub value: ExprIdx,
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
    Bin { lhs: ExprIdx, rhs: ExprIdx, op: Option<BinOp> },
    FncCall { def: FncDefIdx, args: IdxRange<Expr> },
    Block(Vec<Stmt>, Option<ExprIdx>),
    VarRef(VarDefIdx),
    IntLiteral(u32),
    StringLiteral(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarDefIdx {
    Local(LocalDefIdx),
    Param(ParamIdx),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Name(pub Option<String>);
