use logos::Logos;
use std::convert::TryInto;
use std::mem;
use std::ops::Range as StdRange;
use text_size::TextRange;
use token::Token;

pub fn lex(text: &str) -> Vec<Token<'_>> {
    Lexer { inner: LexerTokenKind::lexer(text) }.collect()
}

struct Lexer<'a> {
    inner: logos::Lexer<'a, LexerTokenKind>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();
        let range = {
            let StdRange { start, end } = self.inner.span();
            let start = start.try_into().unwrap();
            let end = end.try_into().unwrap();
            TextRange::new(start, end)
        };

        Some(Token { text, kind: unsafe { mem::transmute(kind) }, range })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
enum LexerTokenKind {
    #[token("let")]
    LetKw,

    #[token("fnc")]
    FncKw,

    #[regex("[a-zA-Z_]+[a-zA-Z0-9_]*")]
    Ident,

    #[regex("[0-9]+")]
    Int,

    #[regex("\"[^\"\n]*\"")]
    String,

    #[token("+")]
    Plus,

    #[token("-")]
    Hyphen,

    #[token("*")]
    Asterisk,

    #[token("/")]
    Slash,

    #[token("=")]
    Eq,

    #[token(".")]
    Dot,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token("->")]
    Arrow,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[regex("[ \n]+")]
    Whitespace,

    #[regex("#.*")]
    Comment,

    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::TokenKind;

    fn check(input: &str, expected_kind: TokenKind) {
        let tokens = lex(input);

        assert_eq!(tokens[0].kind, expected_kind);
        assert_eq!(tokens[0].text, input); // the token should span the entire input

        assert_eq!(tokens.len(), 1); // we should only get one token
    }

    #[test]
    fn lex_whitespace() {
        check("  \n ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_comment() {
        check("# ignore me", TokenKind::Comment);
    }

    #[test]
    fn comments_go_to_end_of_line() {
        assert_eq!(
            lex("# foo\n100"),
            [
                Token {
                    text: "# foo",
                    kind: TokenKind::Comment,
                    range: TextRange::new(0.into(), 5.into())
                },
                Token {
                    text: "\n",
                    kind: TokenKind::Whitespace,
                    range: TextRange::new(5.into(), 6.into())
                },
                Token {
                    text: "100",
                    kind: TokenKind::Int,
                    range: TextRange::new(6.into(), 9.into())
                },
            ]
        );
    }

    #[test]
    fn lex_let_keyword() {
        check("let", TokenKind::LetKw);
    }

    #[test]
    fn lex_fnc_keyword() {
        check("fnc", TokenKind::FncKw);
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

    #[test]
    fn lex_int() {
        check("123", TokenKind::Int);
    }

    #[test]
    fn dont_lex_ident_starting_with_int() {
        assert_eq!(
            lex("92foo"),
            [
                Token {
                    text: "92",
                    kind: TokenKind::Int,
                    range: TextRange::new(0.into(), 2.into())
                },
                Token {
                    text: "foo",
                    kind: TokenKind::Ident,
                    range: TextRange::new(2.into(), 5.into())
                }
            ]
        );
    }

    #[test]
    fn lex_string() {
        check("\"hello\"", TokenKind::String);
    }

    #[test]
    fn dont_lex_multiline_string() {
        assert_eq!(
            lex("\"foo\nbar\""),
            [
                Token {
                    text: "\"foo",
                    kind: TokenKind::Error,
                    range: TextRange::new(0.into(), 4.into())
                },
                Token {
                    text: "\n",
                    kind: TokenKind::Whitespace,
                    range: TextRange::new(4.into(), 5.into())
                },
                Token {
                    text: "bar",
                    kind: TokenKind::Ident,
                    range: TextRange::new(5.into(), 8.into())
                },
                Token {
                    text: "\"",
                    kind: TokenKind::Error,
                    range: TextRange::new(8.into(), 9.into())
                }
            ]
        );
    }

    #[test]
    fn lex_plus() {
        check("+", TokenKind::Plus);
    }

    #[test]
    fn lex_hyphen() {
        check("-", TokenKind::Hyphen);
    }

    #[test]
    fn lex_asterisk() {
        check("*", TokenKind::Asterisk);
    }

    #[test]
    fn lex_slash() {
        check("/", TokenKind::Slash);
    }

    #[test]
    fn lex_eq() {
        check("=", TokenKind::Eq);
    }

    #[test]
    fn lex_dot() {
        check(".", TokenKind::Dot);
    }

    #[test]
    fn lex_colon() {
        check(":", TokenKind::Colon);
    }

    #[test]
    fn lex_comma() {
        check(",", TokenKind::Comma);
    }

    #[test]
    fn lex_semicolon() {
        check(";", TokenKind::Semicolon);
    }

    #[test]
    fn lex_arrow() {
        check("->", TokenKind::Arrow);
    }

    #[test]
    fn lex_l_paren() {
        check("(", TokenKind::LParen);
    }

    #[test]
    fn lex_r_paren() {
        check(")", TokenKind::RParen);
    }

    #[test]
    fn lex_l_brace() {
        check("{", TokenKind::LBrace);
    }

    #[test]
    fn lex_r_brace() {
        check("}", TokenKind::RBrace);
    }
}
