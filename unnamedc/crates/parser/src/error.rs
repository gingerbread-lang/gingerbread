use std::collections::BTreeSet;
use std::fmt;
use text_size::TextRange;
use token::TokenKind;

#[derive(Clone, PartialEq)]
pub struct ParseError {
    pub range: TextRange,
    pub data: ParseErrorData,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseErrorData {
    pub expected_syntaxes: BTreeSet<ExpectedSyntax>,
    pub found: Option<TokenKind>,
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected ",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        for (idx, expected_syntax) in self.data.expected_syntaxes.iter().enumerate() {
            if idx != 0 {
                write!(f, ", ")?;
            }

            match expected_syntax {
                ExpectedSyntax::Named(name) => write!(f, "{}", name)?,
                ExpectedSyntax::Unnamed(kind) => write!(f, "{:?}", kind)?,
            }
        }

        if let Some(found) = self.data.found {
            write!(f, " but found {:?}", found)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExpectedSyntax {
    Named(&'static str),
    Unnamed(TokenKind),
}
