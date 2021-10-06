use arena::{Arena, Idx};

pub type ExprIdx = Idx<Expr>;
pub type LocalDefIdx = Idx<LocalDef>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub local_defs: Arena<LocalDef>,
    pub exprs: Arena<Expr>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Stmt {
    LocalDef(LocalDefIdx),
    Expr(ExprIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalDef {
    pub value: ExprIdx,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Missing,
    Bin { lhs: ExprIdx, rhs: ExprIdx, op: Option<BinOp> },
    Block(Vec<Stmt>),
    VarRef(VarDefIdx),
    IntLiteral(u32),
    StringLiteral(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarDefIdx {
    Local(LocalDefIdx),
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
