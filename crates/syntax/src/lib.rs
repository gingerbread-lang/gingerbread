use rowan::Language;
use std::mem;
use token::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum L {}

impl Language for L {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { mem::transmute(raw.0 as u8) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<L>;
pub type SyntaxToken = rowan::SyntaxToken<L>;
pub type SyntaxElement = rowan::SyntaxElement<L>;

#[derive(Default)]
pub struct SyntaxBuilder(rowan::GreenNodeBuilder<'static>);

impl SyntaxBuilder {
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.0.start_node(L::kind_to_raw(kind));
    }

    pub fn finish_node(&mut self) {
        self.0.finish_node();
    }

    pub fn token(&mut self, kind: SyntaxKind, text: &str) {
        self.0.token(L::kind_to_raw(kind), text);
    }

    pub fn finish(self) -> SyntaxNode {
        SyntaxNode::new_root(self.0.finish())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> SyntaxKind {
        unsafe { mem::transmute(token_kind) }
    }
}
