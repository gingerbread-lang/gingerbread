use logos::Logos;

pub(crate) fn lex(text: &str) -> impl Iterator<Item = Token<'_>> {
    Lexer { inner: TokenKind::lexer(text) }
}

struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        Some(Token { text, kind })
    }
}

#[derive(Debug)]
pub(crate) struct Token<'a> {
    text: &'a str,
    kind: TokenKind,
}

#[derive(Debug, Logos)]
enum TokenKind {
    #[error]
    Error,
}
