use std::collections::BTreeSet;
use std::fmt;
use text_size::TextRange;
use token::TokenKind;

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
    pub(crate) expected: BTreeSet<TokenKind>,
    pub(crate) found: Option<TokenKind>,
    pub(crate) range: TextRange,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        format_comma_separated(f, self.expected.iter().map(|kind| format_kind(*kind)))?;

        if let Some(found) = self.found {
            write!(f, " but found {}", format_kind(found))?;
        }

        Ok(())
    }
}

fn format_comma_separated<'a>(
    f: &'a mut fmt::Formatter<'_>,
    items: impl ExactSizeIterator<Item = &'a str>,
) -> fmt::Result {
    let len = items.len();

    for (idx, item) in items.enumerate() {
        if idx == 0 {
            write!(f, " {}", item)?;
        } else if idx == len - 1 {
            write!(f, " or {}", item)?;
        } else {
            write!(f, ", {}", item)?;
        }
    }

    Ok(())
}

fn format_kind(kind: TokenKind) -> &'static str {
    match kind {
        TokenKind::LetKw => "`let`",
        TokenKind::Ident => "identifier",
        TokenKind::Int => "integer literal",
        TokenKind::Plus => "`+`",
        TokenKind::Hyphen => "`-`",
        TokenKind::Asterisk => "`*`",
        TokenKind::Slash => "`/`",
        TokenKind::Eq => "`=`",
        TokenKind::LParen => "`(`",
        TokenKind::RParen => "`)`",
        TokenKind::Whitespace => "whitespace",
        TokenKind::Error => "an unrecognized token",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use text_size::TextSize;

    fn check<const NUM_EXPECTED: usize>(
        expected: [TokenKind; NUM_EXPECTED],
        found: Option<TokenKind>,
        from: u32,
        to: u32,
        formatted: Expect,
    ) {
        let error = ParseError {
            expected: IntoIterator::into_iter(expected).collect(),
            found,
            range: TextRange::new(TextSize::from(from), TextSize::from(to)),
        };

        formatted.assert_eq(&error.to_string());
    }

    #[test]
    fn did_find_expected_1() {
        check(
            [TokenKind::Ident],
            Some(TokenKind::Asterisk),
            10,
            20,
            expect![[r#"error at 10..20: expected identifier but found `*`"#]],
        );
    }

    #[test]
    fn did_not_find_expected_1() {
        check([TokenKind::LParen], None, 1, 10, expect![[r#"error at 1..10: expected `(`"#]]);
    }

    #[test]
    fn did_find_expected_2() {
        check(
            [TokenKind::Int, TokenKind::Ident],
            Some(TokenKind::Plus),
            92,
            100,
            expect![[r#"error at 92..100: expected identifier or integer literal but found `+`"#]],
        );
    }

    #[test]
    fn did_not_find_expected_multiple() {
        check(
            [TokenKind::Plus, TokenKind::Hyphen, TokenKind::Asterisk, TokenKind::Slash],
            Some(TokenKind::Error),
            5,
            6,
            expect![[
                r#"error at 5..6: expected `+`, `-`, `*` or `/` but found an unrecognized token"#
            ]],
        );
    }
}
