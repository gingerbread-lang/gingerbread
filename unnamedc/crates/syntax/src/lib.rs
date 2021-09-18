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
pub type SyntaxToken = rowan::SyntaxToken<UnnamedLang>;
pub type SyntaxElement = rowan::SyntaxElement<UnnamedLang>;

#[derive(Default)]
pub struct SyntaxBuilder(rowan::GreenNodeBuilder<'static>);

impl SyntaxBuilder {
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.0.start_node(UnnamedLang::kind_to_raw(kind));
    }

    pub fn finish_node(&mut self) {
        self.0.finish_node();
    }

    pub fn token(&mut self, kind: SyntaxKind, text: &str) {
        self.0.token(UnnamedLang::kind_to_raw(kind), text);
    }

    pub fn finish(self) -> SyntaxNode {
        SyntaxNode::new_root(self.0.finish())
    }
}

#[derive(Debug, PartialEq)]
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
    Colon,
    Comma,
    Arrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Whitespace,
    Error,

    Root,
    VarRef,
    Block,
    IntLiteral,
    StringLiteral,
    BinExpr,
    ParenExpr,
    VarDef,
    FncDef,
    Params,
    Param,
    RetTy,
    Ty,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> SyntaxKind {
        unsafe { mem::transmute(token_kind) }
    }
}
