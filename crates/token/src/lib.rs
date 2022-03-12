use std::fmt;
use text_size::TextRange;

#[derive(PartialEq, Default)]
pub struct Tokens {
    pub kinds: Vec<TokenKind>,
    pub ranges: Vec<TextRange>,
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

impl Tokens {
    pub fn iter(&self) -> impl Iterator<Item = (TokenKind, TextRange)> + '_ {
        self.kinds.iter().copied().zip(self.ranges.iter().copied())
    }
}

impl fmt::Debug for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "[")?;

        for (kind, range) in self.kinds.iter().zip(&self.ranges) {
            writeln!(f, "{:?}@{}..{},", kind, u32::from(range.start()), u32::from(range.end()))?;
        }

        write!(f, "]")?;

        Ok(())
    }
}
