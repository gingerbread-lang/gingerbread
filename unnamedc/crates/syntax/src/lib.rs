use rowan::Language;
use std::mem;
use token::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnnamedLang {}

impl Language for UnnamedLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { mem::transmute(raw) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<UnnamedLang>;

#[derive(Debug, PartialEq)]
#[repr(u16)]
pub enum SyntaxKind {
    LetKw,
    Ident,
    Int,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    Eq,
    LParen,
    RParen,
    Whitespace,
    Error,

    Root,
    VarRef,
    IntLiteral,
    BinExpr,
    ParenExpr,
    VarDef,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> SyntaxKind {
        unsafe { mem::transmute(token_kind) }
    }
}
