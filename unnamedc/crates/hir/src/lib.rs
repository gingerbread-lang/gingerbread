use la_arena::{Arena, Idx};

pub type ExprIdx = Idx<Expr>;
pub type VarDefIdx = Idx<VarDef>;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub var_defs: Arena<VarDef>,
    pub exprs: Arena<Expr>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Stmt {
    VarDef(VarDefIdx),
    Expr(ExprIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDef {
    pub value: ExprIdx,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Missing,
    Bin { lhs: ExprIdx, rhs: ExprIdx, op: Option<BinOp> },
    Block { stmts: Vec<Stmt> },
    VarRef { var_def: VarDefIdx },
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
