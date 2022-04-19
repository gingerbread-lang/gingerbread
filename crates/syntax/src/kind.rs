use std::mem;
use token::TokenKind;

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u16)]
pub enum SyntaxKind {
    LetKw,
    FncKw,
    Ident,
    Int,
    String,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    Eq,
    Dot,
    Colon,
    Comma,
    Semicolon,
    Arrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Whitespace,
    Comment,
    Error,

    Root,
    Call,
    ArgList,
    Arg,
    Block,
    IntLiteral,
    StringLiteral,
    BinaryExpr,
    LocalDef,
    ExprStatement,
    Function,
    ParamList,
    Param,
    ReturnTy,
    Ty,

    __Last,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> SyntaxKind {
        unsafe { mem::transmute(token_kind as u16) }
    }
}
