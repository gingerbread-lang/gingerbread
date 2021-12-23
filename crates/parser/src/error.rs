use std::fmt;
use text_size::{TextRange, TextSize};
use token::TokenKind;

#[derive(Clone, Copy, PartialEq)]
pub struct SyntaxError {
    pub expected_syntax: ExpectedSyntax,
    pub kind: SyntaxErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SyntaxErrorKind {
    Missing { offset: TextSize },
    Unexpected { found: TokenKind, range: TextRange },
}

impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error at ")?;
        match self.kind {
            SyntaxErrorKind::Missing { offset } => write!(f, "{}", u32::from(offset))?,
            SyntaxErrorKind::Unexpected { range, .. } => {
                write!(f, "{}..{}", u32::from(range.start()), u32::from(range.end()))?
            }
        };
        write!(f, ": ")?;

        let format_expected_syntax = |f: &mut fmt::Formatter<'_>| match self.expected_syntax {
            ExpectedSyntax::Named(name) => write!(f, "{}", name),
            ExpectedSyntax::Unnamed(kind) => write!(f, "{:?}", kind),
        };

        match self.kind {
            SyntaxErrorKind::Missing { .. } => {
                write!(f, "missing ")?;
                format_expected_syntax(f)?;
            }
            SyntaxErrorKind::Unexpected { found, .. } => {
                write!(f, "expected ")?;
                format_expected_syntax(f)?;
                write!(f, " but found {:?}", found)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExpectedSyntax {
    Named(&'static str),
    Unnamed(TokenKind),
}
