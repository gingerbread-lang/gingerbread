use crate::{AstNode, Function};
use syntax::SyntaxTree;
use text_size::TextRange;

pub fn validate(ast: impl AstNode, tree: &SyntaxTree) -> Vec<ValidationDiagnostic> {
    let mut diagnostics = Vec::new();

    for node in ast.syntax().descendant_nodes(tree) {
        if let Some(function) = Function::cast(node, tree) {
            if let Some(param_list) = function.param_list(tree) {
                if param_list.params(tree).next().is_none() {
                    diagnostics.push(ValidationDiagnostic {
                        kind: ValidationDiagnosticKind::UnneededParens,
                        range: param_list.range(tree),
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

        let tree = parser::parse_source_file(&lexer::lex(input)).into_syntax_tree();
        let root = Root::cast(tree.root(), &tree).unwrap();

        assert_eq!(validate(root, &tree), diagnostics);
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

        let tree = parser::parse_repl_line(&lexer::lex(input)).into_syntax_tree();
        let root = Root::cast(tree.root(), &tree).unwrap();

        assert_eq!(validate(root, &tree), diagnostics);
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
    fn validate_unneeded_parens_on_function() {
        check_source_file(
            "fnc foo ( ) -> {};",
            [(ValidationDiagnosticKind::UnneededParens, 8..11)],
        );
    }
}
