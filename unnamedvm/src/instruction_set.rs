use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub(crate) enum Instruction {
    Exit,
    Push(Val),
    Pop(Reg),
    Store(Loc, Val),
    Add(Reg, Val),
    Sub(Reg, Val),
    Mul(Reg, Val),
    Div(Reg, Val),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub(crate) enum Loc {
    Ptr(usize),
    Reg(Reg),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct Reg(pub(crate) usize);

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub(crate) enum Val {
    Imm(usize),
    Reg(Reg),
    Ptr(Reg),
}
