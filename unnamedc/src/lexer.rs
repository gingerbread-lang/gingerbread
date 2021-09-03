use logos::Logos;

pub fn lex(text: &str) -> impl Iterator<Item = Token<'_>> {
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
pub struct Token<'a> {
    text: &'a str,
    kind: TokenKind,
}

#[derive(Debug, PartialEq, Logos)]
enum TokenKind {
    #[regex("[a-zA-Z0-9_]+")]
    Ident,

    #[regex("[ \n]+")]
    Whitespace,

    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, expected_kind: TokenKind) {
        let mut tokens = lex(input);

        let token = tokens.next().unwrap();
        assert_eq!(token.kind, expected_kind);
        assert_eq!(token.text, input); // the token should span the entire input

        assert!(tokens.next().is_none()); // we should only get one token
    }

    #[test]
    fn lex_whitespace() {
        check("  \n ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_lowercase_alphabetic_ident() {
        check("abc", TokenKind::Ident);
    }

    #[test]
    fn lex_uppercase_alphabetic_ident() {
        check("ABC", TokenKind::Ident);
    }

    #[test]
    fn lex_mixed_case_alphabetic_ident() {
        check("abCdEFg", TokenKind::Ident);
    }

    #[test]
    fn lex_alphanumeric_ident() {
        check("abc123def", TokenKind::Ident);
    }

    #[test]
    fn lex_ident_with_underscores() {
        check("a_b_c", TokenKind::Ident);
    }

    #[test]
    fn lex_ident_starting_with_underscore() {
        check("__main__", TokenKind::Ident);
    }
}
