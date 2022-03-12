use std::fmt;
use text_size::TextRange;

#[derive(Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: TextRange,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}@{}..{}",
            self.kind,
            u32::from(self.range.start()),
            u32::from(self.range.end())
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
}
