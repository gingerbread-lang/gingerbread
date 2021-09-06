use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt;
use text_size::TextRange;
use token::TokenKind;

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
    expected_groups: Vec<ExpectedGroup>,
    found: Option<TokenKind>,
    range: TextRange,
}

impl ParseError {
    pub(crate) fn new(
        mut expected_groups: Vec<ExpectedGroup>,
        found: Option<TokenKind>,
        range: TextRange,
    ) -> Self {
        expected_groups.sort_unstable();
        Self { expected_groups, found, range }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected ",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        format_comma_separated(f, self.expected_groups.iter())?;

        if let Some(found) = self.found {
            write!(f, " but found {}", format_kind(found))?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ExpectedGroup {
    pub(crate) name: Option<&'static str>,
    pub(crate) kinds: BTreeSet<TokenKind>,
}

// we use a custom Ord implementation
// to ensure that ExpectedGroups with no name
// are sorted after those with a name

impl Ord for ExpectedGroup {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.name, other.name) {
            (Some(n1), Some(n2)) => n1.cmp(n2),
            (None, Some(_)) => Ordering::Greater,
            (Some(_), None) => Ordering::Less,
            (None, None) => self.kinds.cmp(&other.kinds),
        }
    }
}

impl PartialOrd for ExpectedGroup {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for ExpectedGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let formatted_kinds = self.kinds.iter().map(|kind| format_kind(*kind));

        if let Some(name) = self.name {
            write!(f, "{} (", name)?;
            format_comma_separated(f, formatted_kinds)?;
            write!(f, ")")?;
        } else {
            format_comma_separated(f, formatted_kinds)?;
        }

        Ok(())
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
        expected_groups: [(Option<&'static str>, &[TokenKind]); NUM_EXPECTED],
        found: Option<TokenKind>,
        from: u32,
        to: u32,
        formatted: Expect,
    ) {
        let error = ParseError::new(
            IntoIterator::into_iter(expected_groups)
                .map(|(name, kinds)| ExpectedGroup { name, kinds: kinds.iter().copied().collect() })
                .collect(),
            found,
            TextRange::new(TextSize::from(from), TextSize::from(to)),
        );

        formatted.assert_eq(&error.to_string());
    }

    #[test]
    fn did_find_expected_1() {
        check(
            [(None, &[TokenKind::Ident])],
            Some(TokenKind::Asterisk),
            10,
            20,
            expect![[r#"error at 10..20: expected identifier but found `*`"#]],
        );
    }

    #[test]
    fn did_not_find_expected_1() {
        check(
            [(None, &[TokenKind::LParen])],
            None,
            1,
            10,
            expect![[r#"error at 1..10: expected `(`"#]],
        );
    }

    #[test]
    fn did_find_expected_2() {
        check(
            [(None, &[TokenKind::Int, TokenKind::Ident])],
            Some(TokenKind::Plus),
            92,
            100,
            expect![[r#"error at 92..100: expected identifier or integer literal but found `+`"#]],
        );
    }

    #[test]
    fn did_not_find_expected_multiple() {
        check(
            [(None, &[TokenKind::Plus, TokenKind::Hyphen, TokenKind::Asterisk, TokenKind::Slash])],
            Some(TokenKind::Error),
            5,
            6,
            expect![[
                r#"error at 5..6: expected `+`, `-`, `*` or `/` but found an unrecognized token"#
            ]],
        );
    }

    #[test]
    fn multiple_expected_groups() {
        check(
            [
                (Some("statement"), &[TokenKind::LetKw]),
                (None, &[TokenKind::Asterisk]),
                (Some("expression"), &[TokenKind::Ident, TokenKind::Int, TokenKind::LParen]),
            ],
            None,
            5,
            10,
            expect![[
                r#"error at 5..10: expected expression (identifier, integer literal or `(`), statement (`let`) or `*`"#
            ]],
        );
    }
}
