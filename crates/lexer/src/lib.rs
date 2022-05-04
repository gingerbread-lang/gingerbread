use logos::Logos;
use std::convert::TryInto;
use std::mem;
use syntax::TokenKind;
use text_size::TextSize;
use token::Tokens;

pub fn lex(text: &str) -> Tokens {
    let mut kinds = Vec::new();
    let mut starts = Vec::new();

    let lexer = Lexer { top_level_lexer: LexerTokenKind::lexer(text), string_lexer: None };
    for (kind, start) in lexer {
        kinds.push(kind);
        starts.push(start);
    }

    starts.push((text.len() as u32).into());

    kinds.shrink_to_fit();
    starts.shrink_to_fit();

    Tokens::new(kinds, starts)
}

struct Lexer<'a> {
    top_level_lexer: logos::Lexer<'a, LexerTokenKind>,
    string_lexer: Option<(logos::Lexer<'a, StringTokenKind>, usize)>,
}

impl Iterator for Lexer<'_> {
    type Item = (TokenKind, TextSize);

    fn next(&mut self) -> Option<Self::Item> {
        let (kind, range) = match &mut self.string_lexer {
            Some((string_lexer, offset)) => match string_lexer.next() {
                Some(kind) => {
                    let kind = match kind {
                        StringTokenKind::Quote => LexerTokenKind::Quote,
                        StringTokenKind::Contents => LexerTokenKind::StringContents,
                        StringTokenKind::Error => unreachable!(),
                    };
                    let range = string_lexer.span();

                    (kind, (range.start + *offset)..(range.end + *offset))
                }
                None => {
                    self.string_lexer = None;
                    return self.next();
                }
            },

            None => {
                let kind = self.top_level_lexer.next()?;

                if kind == LexerTokenKind::__String {
                    self.string_lexer = Some((
                        StringTokenKind::lexer(self.top_level_lexer.slice()),
                        self.top_level_lexer.span().start,
                    ));
                    return self.next();
                }

                (kind, self.top_level_lexer.span())
            }
        };

        let start = {
            let start = range.start;
            let start: u32 = start.try_into().unwrap();
            start.into()
        };

        Some((unsafe { mem::transmute::<LexerTokenKind, TokenKind>(kind) }, start))
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

    Quote,

    StringContents,

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

    // the closing quote is optional;
    // unclosed quotes are handled in parsing for better error messages
    #[regex(r#""[^"\n]*"?"#)]
    __String,
}

#[derive(Debug, Logos)]
enum StringTokenKind {
    #[token("\"")]
    Quote,
    #[regex("[^\"]*")]
    Contents,
    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    use text_size::TextRange;

    fn check(input: &str, expected_kind: TokenKind) {
        let tokens = lex(input);

        assert_eq!(tokens.kind(0), expected_kind);
        assert_eq!(tokens.range(0), TextRange::new(0.into(), (input.len() as u32).into())); // the token should span the entire input

        // we should only get one token
        assert_eq!(tokens.len(), 1);
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
            Tokens::new(
                vec![TokenKind::Comment, TokenKind::Whitespace, TokenKind::Int],
                vec![0.into(), 5.into(), 6.into(), 9.into()]
            )
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
            Tokens::new(vec![TokenKind::Int, TokenKind::Ident], vec![0.into(), 2.into(), 5.into()])
        );
    }

    #[test]
    fn lex_string() {
        assert_eq!(
            lex("\"hello\""),
            Tokens::new(
                vec![TokenKind::Quote, TokenKind::StringContents, TokenKind::Quote],
                vec![0.into(), 1.into(), 6.into(), 7.into()]
            )
        );
    }

    #[test]
    fn lex_empty_string() {
        assert_eq!(
            lex("\"\""),
            Tokens::new(
                vec![TokenKind::Quote, TokenKind::Quote],
                vec![0.into(), 1.into(), 2.into()]
            )
        );
    }

    #[test]
    fn unclosed_string_go_to_end_of_line() {
        assert_eq!(
            lex("\
foo\"bar
baz"),
            Tokens::new(
                vec![
                    TokenKind::Ident,
                    TokenKind::Quote,
                    TokenKind::StringContents,
                    TokenKind::Whitespace,
                    TokenKind::Ident
                ],
                vec![0.into(), 3.into(), 4.into(), 7.into(), 8.into(), 11.into()]
            )
        );
    }

    #[test]
    fn dont_lex_multiline_string() {
        assert_eq!(
            lex("\"foo\nbar\""),
            Tokens::new(
                vec![
                    TokenKind::Quote,
                    TokenKind::StringContents,
                    TokenKind::Whitespace,
                    TokenKind::Ident,
                    TokenKind::Quote
                ],
                vec![0.into(), 1.into(), 4.into(), 5.into(), 8.into(), 9.into()]
            )
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
