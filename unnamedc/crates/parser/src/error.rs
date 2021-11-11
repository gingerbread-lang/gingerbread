use std::fmt;
use text_size::{TextRange, TextSize};
use token::TokenKind;

#[derive(Clone, PartialEq)]
pub struct ParseError {
    pub expected_syntax: ExpectedSyntax,
    pub kind: ParseErrorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    Missing { offset: TextSize },
    Unexpected { found: TokenKind, range: TextRange },
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error at ")?;
        match self.kind {
            ParseErrorKind::Missing { offset } => write!(f, "{}", u32::from(offset))?,
            ParseErrorKind::Unexpected { range, .. } => {
                write!(f, "{}..{}", u32::from(range.start()), u32::from(range.end()))?
            }
        };
        write!(f, ": ")?;

        let format_expected_syntax = |f: &mut fmt::Formatter<'_>| match self.expected_syntax {
            ExpectedSyntax::Named(name) => write!(f, "{}", name),
            ExpectedSyntax::Unnamed(kind) => write!(f, "{:?}", kind),
        };

        match self.kind {
            ParseErrorKind::Missing { .. } => {
                write!(f, "missing ")?;
                format_expected_syntax(f)?;
            }
            ParseErrorKind::Unexpected { found, .. } => {
                write!(f, "expected ")?;
                format_expected_syntax(f)?;
                write!(f, " but found {:?}", found)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExpectedSyntax {
    Named(&'static str),
    Unnamed(TokenKind),
}
