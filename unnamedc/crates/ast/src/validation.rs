use crate::{AstNode, AstToken, IntLiteral, Root};
use std::fmt;
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

pub struct ValidatedRoot(Root);

impl ValidatedRoot {
    pub fn root(&self) -> &Root {
        &self.0
    }
}

#[derive(Debug, PartialEq)]
pub struct ValidationError {
    kind: ValidationErrorKind,
    range: TextRange,
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: {}",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
            match self.kind {
                ValidationErrorKind::IntLiteralTooBig => "integer literal too large",
            }
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum ValidationErrorKind {
    IntLiteralTooBig,
}

#[cfg(test)]
mod error_display_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check(kind: ValidationErrorKind, from: u32, to: u32, formatted: Expect) {
        let error = ValidationError { kind, range: TextRange::new(from.into(), to.into()) };
        formatted.assert_eq(&error.to_string());
    }

    #[test]
    fn int_literal_too_big() {
        check(
            ValidationErrorKind::IntLiteralTooBig,
            0,
            20,
            expect![[r#"error at 0..20: integer literal too large"#]],
        );
    }
}

#[cfg(test)]
mod validation_tests {
    use super::*;

    fn check<const LEN: usize>(
        input: &str,
        expected_errors: [(ValidationErrorKind, u32, u32); LEN],
    ) {
        let errors: Vec<_> = IntoIterator::into_iter(expected_errors)
            .map(|(kind, from, to)| ValidationError {
                kind,
                range: TextRange::new(from.into(), to.into()),
            })
            .collect();

        let syntax = parser::parse(lexer::lex(input)).syntax_node();
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
        check("4294967296", [(ValidationErrorKind::IntLiteralTooBig, 0, 10)]);
    }
}
