use std::mem;

pub type SyntaxBuilder = eventree::SyntaxBuilder<TreeConfig>;
pub type SyntaxElement = eventree::SyntaxElement<TreeConfig>;
pub type SyntaxNode = eventree::SyntaxNode<TreeConfig>;
pub type SyntaxToken = eventree::SyntaxToken<TreeConfig>;
pub type SyntaxTree = eventree::SyntaxTree<TreeConfig>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TreeConfig {}

impl eventree::TreeConfig for TreeConfig {
    type NodeKind = NodeKind;
    type TokenKind = TokenKind;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
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
    __Last,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeKind {
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
    Error,
    __Last,
}

unsafe impl eventree::SyntaxKind for TokenKind {
    const LAST: u16 = Self::__Last as u16;

    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        mem::transmute(raw as u8)
    }
}

unsafe impl eventree::SyntaxKind for NodeKind {
    const LAST: u16 = Self::__Last as u16;

    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        mem::transmute(raw as u8)
    }
}
