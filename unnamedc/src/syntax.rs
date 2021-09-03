use crate::lexer::TokenKind;
use rowan::Language;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum UnnamedLang {}

impl Language for UnnamedLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute(raw) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub(crate) type SyntaxNode = rowan::SyntaxNode<UnnamedLang>;

#[derive(Debug)]
#[repr(u16)]
pub(crate) enum SyntaxKind {
    Ident,
    Int,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    Whitespace,
    Error,

    Root,
    VarRef,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> SyntaxKind {
        unsafe { std::mem::transmute(token_kind) }
    }
}
