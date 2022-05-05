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
                        StringTokenKind::Escape => LexerTokenKind::Escape,
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

    Escape,

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
    #[regex(r#""([^"\\\n]|\\.)*"?"#)]
    __String,
}

#[derive(Debug, Logos)]
enum StringTokenKind {
    #[token(r#"""#)]
    Quote,
    #[regex(r#"\\."#)]
    Escape,
    #[regex(r#"[^"\\]*"#)]
    Contents,
    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check(input: &str, expect: Expect) {
        let tokens = lex(input);
        expect.assert_debug_eq(&tokens);
    }

    #[test]
    fn lex_whitespace() {
        check(
            "  \n ",
            expect![[r#"
                Whitespace@0..4
            "#]],
        );
    }

    #[test]
    fn lex_comment() {
        check(
            "# ignore me",
            expect![[r#"
                Comment@0..11
            "#]],
        );
    }

    #[test]
    fn comments_go_to_end_of_line() {
        check(
            "# foo\n100",
            expect![[r#"
                Comment@0..5
                Whitespace@5..6
                Int@6..9
            "#]],
        );
    }

    #[test]
    fn lex_let_keyword() {
        check(
            "let",
            expect![[r#"
                LetKw@0..3
            "#]],
        );
    }

    #[test]
    fn lex_fnc_keyword() {
        check(
            "fnc",
            expect![[r#"
                FncKw@0..3
            "#]],
        );
    }

    #[test]
    fn lex_lowercase_alphabetic_ident() {
        check(
            "abc",
            expect![[r#"
                Ident@0..3
            "#]],
        );
    }

    #[test]
    fn lex_uppercase_alphabetic_ident() {
        check(
            "ABC",
            expect![[r#"
                Ident@0..3
            "#]],
        );
    }

    #[test]
    fn lex_mixed_case_alphabetic_ident() {
        check(
            "abCdEFg",
            expect![[r#"
                Ident@0..7
            "#]],
        );
    }

    #[test]
    fn lex_alphanumeric_ident() {
        check(
            "abc123def",
            expect![[r#"
                Ident@0..9
            "#]],
        );
    }

    #[test]
    fn lex_ident_with_underscores() {
        check(
            "a_b_c",
            expect![[r#"
                Ident@0..5
            "#]],
        );
    }

    #[test]
    fn lex_ident_starting_with_underscore() {
        check(
            "__main__",
            expect![[r#"
                Ident@0..8
            "#]],
        );
    }

    #[test]
    fn lex_int() {
        check(
            "123",
            expect![[r#"
                Int@0..3
            "#]],
        );
    }

    #[test]
    fn dont_lex_ident_starting_with_int() {
        check(
            "92foo",
            expect![[r#"
                Int@0..2
                Ident@2..5
            "#]],
        );
    }

    #[test]
    fn lex_string() {
        check(
            "\"hello\"",
            expect![[r#"
                Quote@0..1
                StringContents@1..6
                Quote@6..7
            "#]],
        );
    }

    #[test]
    fn lex_empty_string() {
        check(
            "\"\"",
            expect![[r#"
                Quote@0..1
                Quote@1..2
            "#]],
        );
    }

    #[test]
    fn unclosed_string_goes_to_end_of_line() {
        check(
            "\
foo\"bar
baz",
            expect![[r#"
                Ident@0..3
                Quote@3..4
                StringContents@4..7
                Whitespace@7..8
                Ident@8..11
            "#]],
        );
    }

    #[test]
    fn dont_lex_multiline_string() {
        check(
            "\"foo\nbar\"",
            expect![[r#"
                Quote@0..1
                StringContents@1..4
                Whitespace@4..5
                Ident@5..8
                Quote@8..9
            "#]],
        );
    }

    #[test]
    fn lex_escapes_in_string() {
        check(
            r#""\\section{Introduction}\n\"Why?\"""#,
            expect![[r#"
                Quote@0..1
                Escape@1..3
                StringContents@3..24
                Escape@24..26
                Escape@26..28
                StringContents@28..32
                Escape@32..34
                Quote@34..35
            "#]],
        );
    }

    #[test]
    fn lex_backslash_at_end_of_string() {
        check(
            r#""\\" test"#,
            expect![[r#"
                Quote@0..1
                Escape@1..3
                Quote@3..4
                Whitespace@4..5
                Ident@5..9
            "#]],
        );
    }

    #[test]
    fn lex_plus() {
        check(
            "+",
            expect![[r#"
                Plus@0..1
            "#]],
        );
    }

    #[test]
    fn lex_hyphen() {
        check(
            "-",
            expect![[r#"
                Hyphen@0..1
            "#]],
        );
    }

    #[test]
    fn lex_asterisk() {
        check(
            "*",
            expect![[r#"
                Asterisk@0..1
            "#]],
        );
    }

    #[test]
    fn lex_slash() {
        check(
            "/",
            expect![[r#"
                Slash@0..1
            "#]],
        );
    }

    #[test]
    fn lex_eq() {
        check(
            "=",
            expect![[r#"
                Eq@0..1
            "#]],
        );
    }

    #[test]
    fn lex_dot() {
        check(
            ".",
            expect![[r#"
                Dot@0..1
            "#]],
        );
    }

    #[test]
    fn lex_colon() {
        check(
            ":",
            expect![[r#"
                Colon@0..1
            "#]],
        );
    }

    #[test]
    fn lex_comma() {
        check(
            ",",
            expect![[r#"
                Comma@0..1
            "#]],
        );
    }

    #[test]
    fn lex_semicolon() {
        check(
            ";",
            expect![[r#"
                Semicolon@0..1
            "#]],
        );
    }

    #[test]
    fn lex_arrow() {
        check(
            "->",
            expect![[r#"
                Arrow@0..2
            "#]],
        );
    }

    #[test]
    fn lex_l_paren() {
        check(
            "(",
            expect![[r#"
                LParen@0..1
            "#]],
        );
    }

    #[test]
    fn lex_r_paren() {
        check(
            ")",
            expect![[r#"
                RParen@0..1
            "#]],
        );
    }

    #[test]
    fn lex_l_brace() {
        check(
            "{",
            expect![[r#"
                LBrace@0..1
            "#]],
        );
    }

    #[test]
    fn lex_r_brace() {
        check(
            "}",
            expect![[r#"
                RBrace@0..1
            "#]],
        );
    }
}
