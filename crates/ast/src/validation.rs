use crate::{AstNode, AstToken, FncDef, IntLiteral};
use text_size::TextRange;

pub fn validate(ast: &impl AstNode) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    for node in ast.syntax().descendants() {
        if let Some(int_literal) = IntLiteral::cast(node.clone()) {
            if let Some(value) = int_literal.value() {
                let text = value.text();

                if text.parse::<u32>().is_err() {
                    errors.push(ValidationError {
                        kind: ValidationErrorKind::IntLiteralTooBig,
                        range: value.range(),
                    });
                }
            }
        } else if let Some(fnc_def) = FncDef::cast(node) {
            if let Some(param_list) = fnc_def.param_list() {
                if param_list.params().next().is_none() {
                    errors.push(ValidationError {
                        kind: ValidationErrorKind::UnneededParens,
                        range: param_list.range(),
                    });
                }
            }
        }
    }

    errors
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ValidationError {
    pub kind: ValidationErrorKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValidationErrorKind {
    IntLiteralTooBig,
    UnneededParens,
}

#[cfg(test)]
mod validation_tests {
    use super::*;
    use crate::Root;
    use std::ops::Range as StdRange;

    fn check_source_file<const LEN: usize>(
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

        let syntax = parser::parse_source_file(&lexer::lex(input)).syntax_node();
        let root = Root::cast(syntax).unwrap();

        assert_eq!(validate(&root), errors);
    }

    fn check_repl_line<const LEN: usize>(
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

        let syntax = parser::parse_repl_line(&lexer::lex(input)).syntax_node();
        let root = Root::cast(syntax).unwrap();

        assert_eq!(validate(&root), errors);
    }

    #[test]
    fn validate_correct_code() {
        check_repl_line("let a = 92; a - 5 * 10", []);
    }

    #[test]
    fn validate_u32_max_int_literal() {
        check_repl_line("4294967295", []);
    }

    #[test]
    fn validate_too_big_int_literal() {
        check_repl_line("4294967296", [(ValidationErrorKind::IntLiteralTooBig, 0..10)]);
    }

    #[test]
    fn validate_multiple_too_big_int_literals() {
        check_source_file(
            "
                fnc main -> {
                  let b = 5000000000;
                  let a = 9999999999999999999 + b;
                };
            ",
            [
                (ValidationErrorKind::IntLiteralTooBig, 57..67),
                (ValidationErrorKind::IntLiteralTooBig, 95..114),
            ],
        );
    }

    #[test]
    fn validate_unneeded_parens_on_fnc_def() {
        check_source_file("fnc foo ( ) -> {};", [(ValidationErrorKind::UnneededParens, 8..11)]);
    }
}
