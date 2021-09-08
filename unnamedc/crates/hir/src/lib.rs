#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDef(VarDef),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDef {
    pub name: Name,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Missing,
    Bin { lhs: Box<Expr>, rhs: Box<Expr>, op: Option<BinOp> },
    VarRef { name: Name },
    IntLiteral { value: u32 },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Name(pub Option<String>);
