use crate::{AstNode, AstToken, Function, IntLiteral};
use text_size::TextRange;

pub fn validate(ast: &impl AstNode) -> Vec<ValidationDiagnostic> {
    let mut diagnostics = Vec::new();

    for node in ast.syntax().descendants() {
        if let Some(int_literal) = IntLiteral::cast(node.clone()) {
            if let Some(value) = int_literal.value() {
                let text = value.text();

                if text.parse::<u32>().is_err() {
                    diagnostics.push(ValidationDiagnostic {
                        kind: ValidationDiagnosticKind::IntLiteralTooBig,
                        range: value.range(),
                    });
                }
            }
        } else if let Some(function) = Function::cast(node) {
            if let Some(param_list) = function.param_list() {
                if param_list.params().next().is_none() {
                    diagnostics.push(ValidationDiagnostic {
                        kind: ValidationDiagnosticKind::UnneededParens,
                        range: param_list.range(),
                    });
                }
            }
        }
    }

    diagnostics
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ValidationDiagnostic {
    pub kind: ValidationDiagnosticKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValidationDiagnosticKind {
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
        diagnostics: [(ValidationDiagnosticKind, StdRange<u32>); LEN],
    ) {
        let diagnostics: Vec<_> = diagnostics
            .into_iter()
            .map(|(kind, range)| ValidationDiagnostic {
                kind,
                range: TextRange::new(range.start.into(), range.end.into()),
            })
            .collect();

        let syntax = parser::parse_source_file(&lexer::lex(input)).syntax_node();
        let root = Root::cast(syntax).unwrap();

        assert_eq!(validate(&root), diagnostics);
    }

    fn check_repl_line<const LEN: usize>(
        input: &str,
        diagnostics: [(ValidationDiagnosticKind, StdRange<u32>); LEN],
    ) {
        let diagnostics: Vec<_> = diagnostics
            .into_iter()
            .map(|(kind, range)| ValidationDiagnostic {
                kind,
                range: TextRange::new(range.start.into(), range.end.into()),
            })
            .collect();

        let syntax = parser::parse_repl_line(&lexer::lex(input)).syntax_node();
        let root = Root::cast(syntax).unwrap();

        assert_eq!(validate(&root), diagnostics);
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
        check_repl_line("4294967296", [(ValidationDiagnosticKind::IntLiteralTooBig, 0..10)]);
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
                (ValidationDiagnosticKind::IntLiteralTooBig, 57..67),
                (ValidationDiagnosticKind::IntLiteralTooBig, 95..114),
            ],
        );
    }

    #[test]
    fn validate_unneeded_parens_on_function() {
        check_source_file(
            "fnc foo ( ) -> {};",
            [(ValidationDiagnosticKind::UnneededParens, 8..11)],
        );
    }
}
