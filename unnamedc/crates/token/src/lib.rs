#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub text: &'a str,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u16)]
pub enum TokenKind {
    Ident,
    Int,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    LParen,
    RParen,
    Whitespace,
    Error,
}
