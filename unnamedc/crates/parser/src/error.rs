use std::collections::BTreeSet;
use std::fmt;
use text_size::TextRange;
use token::TokenKind;

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
    pub(crate) expected_syntaxes: BTreeSet<ExpectedSyntax>,
    pub(crate) found: Option<TokenKind>,
    pub(crate) range: TextRange,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected ",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        format_comma_separated(f, self.expected_syntaxes.iter())?;

        if let Some(found) = self.found {
            write!(f, " but found {}", format_kind(found))?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ExpectedSyntax {
    Named(&'static str),
    Unnamed(TokenKind),
}

impl fmt::Display for ExpectedSyntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Named(name) => write!(f, "{}", name),
            Self::Unnamed(kind) => write!(f, "{}", format_kind(*kind)),
        }
    }
}

fn format_comma_separated(
    f: &mut fmt::Formatter<'_>,
    items: impl ExactSizeIterator<Item = impl fmt::Display>,
) -> fmt::Result {
    let len = items.len();

    for (idx, item) in items.enumerate() {
        if idx == 0 {
            write!(f, "{}", item)?;
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
        TokenKind::String => "string literal",
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
    use std::ops::Range as StdRange;

    fn check<const NUM_EXPECTED: usize>(
        expected_syntaxes: [ExpectedSyntax; NUM_EXPECTED],
        found: Option<TokenKind>,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let error = ParseError {
            expected_syntaxes: IntoIterator::into_iter(expected_syntaxes).collect(),
            found,
            range: TextRange::new(range.start.into(), range.end.into()),
        };

        formatted.assert_eq(&error.to_string());
    }

    #[test]
    fn did_find_expected_1() {
        check(
            [ExpectedSyntax::Unnamed(TokenKind::Ident)],
            Some(TokenKind::Asterisk),
            10..20,
            expect![[r#"error at 10..20: expected identifier but found `*`"#]],
        );
    }

    #[test]
    fn did_not_find_expected_1() {
        check(
            [ExpectedSyntax::Unnamed(TokenKind::LParen)],
            None,
            1..10,
            expect![[r#"error at 1..10: expected `(`"#]],
        );
    }

    #[test]
    fn did_find_expected_2() {
        check(
            [ExpectedSyntax::Unnamed(TokenKind::Int), ExpectedSyntax::Unnamed(TokenKind::Ident)],
            Some(TokenKind::Plus),
            92..100,
            expect![[r#"error at 92..100: expected identifier or integer literal but found `+`"#]],
        );
    }

    #[test]
    fn did_not_find_expected_multiple() {
        check(
            [
                ExpectedSyntax::Unnamed(TokenKind::Plus),
                ExpectedSyntax::Unnamed(TokenKind::Hyphen),
                ExpectedSyntax::Unnamed(TokenKind::Asterisk),
                ExpectedSyntax::Unnamed(TokenKind::Slash),
            ],
            Some(TokenKind::Error),
            5..6,
            expect![[
                r#"error at 5..6: expected `+`, `-`, `*` or `/` but found an unrecognized token"#
            ]],
        );
    }

    #[test]
    fn multiple_expected_syntaxes() {
        check(
            [
                ExpectedSyntax::Named("statement"),
                ExpectedSyntax::Unnamed(TokenKind::Asterisk),
                ExpectedSyntax::Named("expression"),
            ],
            None,
            5..10,
            expect![[r#"error at 5..10: expected expression, statement or `*`"#]],
        );
    }
}
