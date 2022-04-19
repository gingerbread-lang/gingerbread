use std::mem;
use token::TokenKind;

pub type SyntaxBuilder = eventree::SyntaxBuilder<SyntaxKind>;
pub type SyntaxElement = eventree::SyntaxElement<SyntaxKind>;
pub type SyntaxNode = eventree::SyntaxNode<SyntaxKind>;
pub type SyntaxToken = eventree::SyntaxToken<SyntaxKind>;
pub type SyntaxTree = eventree::SyntaxTree<SyntaxKind>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

unsafe impl eventree::SyntaxKind for SyntaxKind {
    const LAST: u16 = Self::__Last as u16;

    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        mem::transmute(raw)
    }
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> SyntaxKind {
        unsafe { mem::transmute(token_kind as u16) }
    }
}
