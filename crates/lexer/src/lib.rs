use logos::Logos;
use std::mem;
use syntax::TokenKind;
use text_size::TextSize;
use token::Tokens;

pub fn lex(text: &str) -> Tokens {
    let mut kinds = Vec::new();
    let mut starts = Vec::new();

    let mut lexer = LexerTokenKind::lexer(text);
    while let Some(kind) = lexer.next() {
        let range = lexer.span();
        let start = (range.start as u32).into();

        let mut handler = |k, s| {
            kinds.push(k);
            starts.push(s);
        };

        match kind {
            LexerTokenKind::__InternalString => lex_string(lexer.slice(), start, handler),
            LexerTokenKind::__InternalComment => lex_comment(start, range.len(), handler),
            LexerTokenKind::__InternalDocComment => lex_doc_comment(start, range.len(), handler),
            _ => handler(unsafe { mem::transmute(kind) }, start),
        }
    }

    starts.push((text.len() as u32).into());

    kinds.shrink_to_fit();
    starts.shrink_to_fit();

    Tokens::new(kinds, starts)
}

fn lex_string(s: &str, offset: TextSize, mut f: impl FnMut(TokenKind, TextSize)) {
    #[derive(Clone, Copy)]
    enum Mode {
        StartContents,
        InContents,
        Escape,
    }

    let mut mode = Mode::InContents;
    let mut pos = offset;

    for c in s.chars() {
        match (mode, c) {
            (Mode::InContents | Mode::StartContents, '"') => {
                mode = Mode::StartContents;
                f(TokenKind::Quote, pos);
            }
            (Mode::InContents | Mode::StartContents, '\\') => {
                mode = Mode::Escape;
                f(TokenKind::Escape, pos);
            }
            (Mode::StartContents, _) => {
                mode = Mode::InContents;
                f(TokenKind::StringContents, pos);
            }
            (Mode::InContents, _) => {}
            (Mode::Escape, _) => mode = Mode::StartContents,
        }

        pos += TextSize::from(c.len_utf8() as u32);
    }
}

fn lex_comment(offset: TextSize, len: usize, mut f: impl FnMut(TokenKind, TextSize)) {
    f(TokenKind::CommentLeader, offset);

    if len > 1 {
        f(TokenKind::CommentContents, offset + TextSize::from(1));
    }
}

fn lex_doc_comment(offset: TextSize, len: usize, mut f: impl FnMut(TokenKind, TextSize)) {
    f(TokenKind::DocCommentLeader, offset);

    if len > 2 {
        f(TokenKind::DocCommentContents, offset + TextSize::from(2));
    }
}

#[derive(PartialEq, Logos)]
enum LexerTokenKind {
    #[token("let")]
    LetKw,

    #[token("fnc")]
    FncKw,

    #[token("rec")]
    RecKw,

    #[regex("[a-zA-Z_]+[a-zA-Z0-9_]*")]
    Ident,

    #[regex("[0-9]+")]
    Int,

    _Quote,

    _Escape,

    _StringContents,

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

    _CommentContents,

    _CommentLeader,

    _DocCommentContents,

    _DocCommentLeader,

    #[error]
    Error,

    // the closing quote is optional;
    // unclosed quotes are handled in parsing for better error messages
    #[regex(r#""([^"\\\n]|\\.)*"?"#)]
    __InternalString,

    #[regex("#.*")]
    __InternalComment,

    #[regex("##.*")]
    __InternalDocComment,
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
                CommentLeader@0..1
                CommentContents@1..11
            "#]],
        );
    }

    #[test]
    fn lex_empty_comment() {
        check(
            "#",
            expect![[r#"
                CommentLeader@0..1
            "#]],
        );
    }

    #[test]
    fn comments_go_to_end_of_line() {
        check(
            "# foo\n100",
            expect![[r#"
                CommentLeader@0..1
                CommentContents@1..5
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
    fn lex_rec_keyword() {
        check(
            "rec",
            expect![[r#"
                RecKw@0..3
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
    fn lex_weird_escapes() {
        check(
            r#""\a\1\"\\\_\|\=\å\👽""#,
            expect![[r#"
                Quote@0..1
                Escape@1..3
                Escape@3..5
                Escape@5..7
                Escape@7..9
                Escape@9..11
                Escape@11..13
                Escape@13..15
                Escape@15..18
                Escape@18..23
                Quote@23..24
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

    #[test]
    fn lex_doc_comment() {
        check(
            "## foo",
            expect![[r#"
                DocCommentLeader@0..2
                DocCommentContents@2..6
            "#]],
        );
    }

    #[test]
    fn lex_empty_doc_comment() {
        check(
            "##\n1",
            expect![[r#"
                DocCommentLeader@0..2
                Whitespace@2..3
                Int@3..4
            "#]],
        );
    }
}
