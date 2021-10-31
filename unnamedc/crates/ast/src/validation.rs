use crate::{AstNode, AstToken, IntLiteral, SourceFile};
use text_size::TextRange;

pub fn validate(source_file: &SourceFile) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    for node in source_file.syntax().descendants() {
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
        let errors: Vec<_> = expected_errors
            .into_iter()
            .map(|(kind, range)| ValidationError {
                kind,
                range: TextRange::new(range.start.into(), range.end.into()),
            })
            .collect();

        let syntax = parser::parse(&lexer::lex(input)).syntax_node();
        let source_file = SourceFile::cast(syntax).unwrap();

        assert_eq!(validate(&source_file), errors);
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
