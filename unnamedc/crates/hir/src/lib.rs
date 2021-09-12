use la_arena::{Arena, Idx};

pub type ExprIdx = Idx<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub exprs: Arena<Expr>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDef(VarDef),
    Expr(ExprIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDef {
    pub name: Name,
    pub value: ExprIdx,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Missing,
    Bin { lhs: ExprIdx, rhs: ExprIdx, op: Option<BinOp> },
    VarRef { name: Name },
    IntLiteral { value: u32 },
    StringLiteral { value: String },
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
