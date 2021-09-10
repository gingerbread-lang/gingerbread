use crate::{AstNode, AstToken, IntLiteral, Root};
use text_size::TextRange;

pub fn validate(root: &Root) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    for node in root.syntax().descendants() {
        if let Some(int_literal) = IntLiteral::cast(node) {
            if let Some(value) = int_literal.value() {
                let text = value.text();

                if text.parse::<u32>().is_err() {
                    errors.push(ValidationError {
                        kind: ValidationErrorKind::IntLiteralTooBig,
                        range: value.range(),
                    });
                }
            }
        }
    }

    errors
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValidatedRoot(Root);

impl ValidatedRoot {
    pub fn root(&self) -> &Root {
        &self.0
    }
}

#[derive(Debug, PartialEq)]
pub struct ValidationError {
    pub kind: ValidationErrorKind,
    pub range: TextRange,
}

#[derive(Debug, PartialEq)]
pub enum ValidationErrorKind {
    IntLiteralTooBig,
}

#[cfg(test)]
mod validation_tests {
    use super::*;
    use std::ops::Range as StdRange;

    fn check<const LEN: usize>(
        input: &str,
        expected_errors: [(ValidationErrorKind, StdRange<u32>); LEN],
    ) {
        let errors: Vec<_> = IntoIterator::into_iter(expected_errors)
            .map(|(kind, range)| ValidationError {
                kind,
                range: TextRange::new(range.start.into(), range.end.into()),
            })
            .collect();

        let syntax = parser::parse(&lexer::lex(input)).syntax_node();
        let root = Root::cast(syntax).unwrap();

        assert_eq!(validate(&root), errors);
    }

    #[test]
    fn validate_correct_code() {
        check("let a = 92\na - 5 * 10", []);
    }

    #[test]
    fn validate_u32_max_int_literal() {
        check("4294967295", []);
    }

    #[test]
    fn validate_too_big_int_literal() {
        check("4294967296", [(ValidationErrorKind::IntLiteralTooBig, 0..10)]);
    }

    #[test]
    fn validate_multiple_too_big_int_literals() {
        check(
            "let b = 5000000000 let a = 9999999999999999999 + b",
            [
                (ValidationErrorKind::IntLiteralTooBig, 8..18),
                (ValidationErrorKind::IntLiteralTooBig, 27..46),
            ],
        );
    }
}
