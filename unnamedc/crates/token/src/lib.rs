use std::fmt;
use text_size::TextRange;

#[derive(Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub text: &'a str,
    pub kind: TokenKind,
    pub range: TextRange,
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}@{}..{} {:?}",
            self.kind,
            u32::from(self.range.start()),
            u32::from(self.range.end()),
            self.text
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u16)]
pub enum TokenKind {
    LetKw,
    FunKw,
    Ident,
    Int,
    String,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    Eq,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Whitespace,
    Error,
}
