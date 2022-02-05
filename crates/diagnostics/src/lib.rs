use ast::validation::{ValidationDiagnostic, ValidationDiagnosticKind};
use hir::{IndexingDiagnostic, IndexingDiagnosticKind, LoweringDiagnostic, LoweringDiagnosticKind};
use line_index::{ColNr, LineIndex, LineNr};
use parser::{ExpectedSyntax, SyntaxError, SyntaxErrorKind};
use std::convert::TryInto;
use text_size::{TextRange, TextSize};
use token::TokenKind;

pub struct Diagnostic(Repr);

enum Repr {
    Syntax(SyntaxError),
    Validation(ValidationDiagnostic),
    Indexing(IndexingDiagnostic),
    Lowering(LoweringDiagnostic),
}

pub enum Severity {
    Warning,
    Error,
}

impl Diagnostic {
    pub fn from_syntax(error: SyntaxError) -> Self {
        Self(Repr::Syntax(error))
    }

    pub fn from_validation(diagnostic: ValidationDiagnostic) -> Self {
        Self(Repr::Validation(diagnostic))
    }

    pub fn from_indexing(diagnostic: IndexingDiagnostic) -> Self {
        Self(Repr::Indexing(diagnostic))
    }

    pub fn from_lowering(diagnostic: LoweringDiagnostic) -> Self {
        Self(Repr::Lowering(diagnostic))
    }

    pub fn display(&self, input: &str, line_index: &LineIndex) -> Vec<String> {
        let range = self.range();

        let (start_line, start_col) = line_index.line_col(range.start());

        // we subtract 1 since end_line_column is inclusive,
        // unlike TextRange which is always exclusive
        let (end_line, end_col) = line_index.line_col(range.end() - TextSize::from(1));

        let severity = match self.severity() {
            Severity::Warning => "warning",
            Severity::Error => "error",
        };

        let mut lines = vec![format!(
            "{} at {}:{}: {}",
            severity,
            start_line.0 + 1,
            start_col.0 + 1,
            self.message()
        )];

        input_snippet(input, start_line, start_col, end_line, end_col, range, &mut lines);

        lines
    }

    pub fn range(&self) -> TextRange {
        match self.0 {
            Repr::Syntax(SyntaxError { kind: SyntaxErrorKind::Missing { offset }, .. }) => {
                TextRange::new(offset, offset + TextSize::from(1))
            }
            Repr::Syntax(SyntaxError {
                kind: SyntaxErrorKind::Unexpected { range, .. }, ..
            }) => range,
            Repr::Validation(ValidationDiagnostic { range, .. }) => range,
            Repr::Indexing(IndexingDiagnostic { range, .. }) => range,
            Repr::Lowering(LoweringDiagnostic { range, .. }) => range,
        }
    }

    pub fn severity(&self) -> Severity {
        match &self.0 {
            Repr::Syntax(_) => Severity::Error,
            Repr::Validation(_) => Severity::Warning,
            Repr::Indexing(_) => Severity::Error,
            Repr::Lowering(_) => Severity::Error,
        }
    }

    pub fn message(&self) -> String {
        match &self.0 {
            Repr::Syntax(e) => syntax_error_message(e),
            Repr::Validation(d) => validation_diagnostic_message(d),
            Repr::Indexing(d) => indexing_diagnostic_message(d),
            Repr::Lowering(d) => lowering_diagnostic_message(d),
        }
    }
}

fn input_snippet(
    input: &str,
    start_line: LineNr,
    start_col: ColNr,
    end_line: LineNr,
    end_col: ColNr,
    range: TextRange,
    lines: &mut Vec<String>,
) {
    const PADDING: &str = "  ";
    const POINTER_UP: &str = "^";
    const POINTER_DOWN: &str = "v";

    let file_lines: Vec<_> = input.lines().collect();

    let is_single_line = start_line == end_line;
    if is_single_line {
        lines.push(format!("{}{}", PADDING, file_lines[start_line.0 as usize]));

        lines.push(format!(
            "{}{}{}",
            PADDING,
            " ".repeat(start_col.0 as usize),
            POINTER_UP.repeat(range.len().try_into().unwrap())
        ));

        return;
    }

    let first_line = file_lines[start_line.0 as usize];
    lines.push(format!(
        "{}{}{}",
        PADDING,
        " ".repeat(start_col.0 as usize),
        POINTER_DOWN.repeat(first_line.len() - start_col.0 as usize)
    ));
    lines.push(format!("{}{}", PADDING, first_line));

    for line in &file_lines[start_line.0 as usize + 1..end_line.0 as usize] {
        lines.push(format!("{}{}", PADDING, line));
    }

    let last_line = file_lines[end_line.0 as usize];
    lines.push(format!("{}{}", PADDING, last_line));
    lines.push(format!("{}{}", PADDING, POINTER_UP.repeat(end_col.0 as usize + 1)));
}

fn syntax_error_message(e: &SyntaxError) -> String {
    let write_expected_syntax = |buf: &mut String| match e.expected_syntax {
        ExpectedSyntax::Named(name) => buf.push_str(name),
        ExpectedSyntax::Unnamed(kind) => buf.push_str(format_kind(kind)),
    };

    let mut message = String::new();

    match e.kind {
        SyntaxErrorKind::Missing { .. } => {
            message.push_str("missing ");
            write_expected_syntax(&mut message);
        }
        SyntaxErrorKind::Unexpected { found, .. } => {
            message.push_str("expected ");
            write_expected_syntax(&mut message);
            message.push_str(&format!(" but found {}", format_kind(found)));
        }
    }

    message
}

fn validation_diagnostic_message(d: &ValidationDiagnostic) -> String {
    match d.kind {
        ValidationDiagnosticKind::UnneededParens => "unneeded parentheses".to_string(),
    }
}

fn indexing_diagnostic_message(d: &IndexingDiagnostic) -> String {
    match &d.kind {
        IndexingDiagnosticKind::FunctionAlreadyDefined { name } => {
            format!("function `{}` already defined", name)
        }
    }
}

fn lowering_diagnostic_message(d: &LoweringDiagnostic) -> String {
    match &d.kind {
        LoweringDiagnosticKind::OutOfRangeIntLiteral => "integer literal out of range".to_string(),
        LoweringDiagnosticKind::UndefinedLocal { name } => format!("undefined variable `{}`", name),
        LoweringDiagnosticKind::MismatchedArgCount { name, expected, got } => {
            format!("`{}` expected {} arguments, but got {}", name, expected, got)
        }
        LoweringDiagnosticKind::CalledLocal { name } => {
            format!("tried to call `{}`, which is a variable, not a function", name)
        }
    }
}

fn format_kind(kind: TokenKind) -> &'static str {
    match kind {
        TokenKind::LetKw => "`let`",
        TokenKind::FncKw => "`fnc`",
        TokenKind::Ident => "identifier",
        TokenKind::Int => "integer literal",
        TokenKind::String => "string literal",
        TokenKind::Plus => "`+`",
        TokenKind::Hyphen => "`-`",
        TokenKind::Asterisk => "`*`",
        TokenKind::Slash => "`/`",
        TokenKind::Eq => "`=`",
        TokenKind::Dot => "`.`",
        TokenKind::Colon => "`:`",
        TokenKind::Comma => "`,`",
        TokenKind::Semicolon => "`;`",
        TokenKind::Arrow => "`->`",
        TokenKind::LParen => "`(`",
        TokenKind::RParen => "`)`",
        TokenKind::LBrace => "`{`",
        TokenKind::RBrace => "`}`",
        TokenKind::Whitespace => "whitespace",
        TokenKind::Comment => "comment",
        TokenKind::Error => "an unrecognized token",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use parser::{ExpectedSyntax, SyntaxErrorKind};
    use std::ops::Range as StdRange;

    fn check_syntax(
        input: &str,
        expected_syntax: ExpectedSyntax,
        kind: SyntaxErrorKind,
        formatted: Expect,
    ) {
        let diagnostic = Diagnostic::from_syntax(SyntaxError { expected_syntax, kind });

        formatted.assert_eq(&format!(
            "{}\n",
            diagnostic.display(input, &LineIndex::new(input)).join("\n")
        ));
    }

    fn check_validation(
        input: &str,
        kind: ValidationDiagnosticKind,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let diagnostic = Diagnostic::from_validation(ValidationDiagnostic {
            kind,
            range: TextRange::new(range.start.into(), range.end.into()),
        });

        formatted.assert_eq(&format!(
            "{}\n",
            diagnostic.display(input, &LineIndex::new(input)).join("\n")
        ));
    }

    fn check_indexing(
        input: &str,
        kind: IndexingDiagnosticKind,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let diagnostic = Diagnostic::from_indexing(IndexingDiagnostic {
            kind,
            range: TextRange::new(range.start.into(), range.end.into()),
        });

        formatted.assert_eq(&format!(
            "{}\n",
            diagnostic.display(input, &LineIndex::new(input)).join("\n")
        ));
    }

    fn check_lowering(
        input: &str,
        kind: LoweringDiagnosticKind,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let diagnostic = Diagnostic::from_lowering(LoweringDiagnostic {
            kind,
            range: TextRange::new(range.start.into(), range.end.into()),
        });

        formatted.assert_eq(&format!(
            "{}\n",
            diagnostic.display(input, &LineIndex::new(input)).join("\n")
        ));
    }

    #[test]
    fn syntax_unexpected() {
        check_syntax(
            "let *",
            ExpectedSyntax::Unnamed(TokenKind::Ident),
            SyntaxErrorKind::Unexpected {
                found: TokenKind::Asterisk,
                range: TextRange::new(4.into(), 5.into()),
            },
            expect![[r#"
                error at 1:5: expected identifier but found `*`
                  let *
                      ^
            "#]],
        );
    }

    #[test]
    fn syntax_missing() {
        check_syntax(
            "let = 10;",
            ExpectedSyntax::Named("variable name"),
            SyntaxErrorKind::Missing { offset: 3.into() },
            expect![[r#"
                error at 1:4: missing variable name
                  let = 10;
                     ^
            "#]],
        );
    }

    #[test]
    fn syntax_missing_at_end_of_line() {
        check_syntax(
            "let a =\nlet b = a;",
            ExpectedSyntax::Named("expression"),
            SyntaxErrorKind::Missing { offset: 7.into() },
            expect![[r#"
                error at 1:8: missing expression
                  let a =
                         ^
            "#]],
        );
    }

    #[test]
    fn validation_unneeded_parens() {
        check_validation(
            "fnc five(): s32 -> 5;",
            ValidationDiagnosticKind::UnneededParens,
            8..10,
            expect![[r#"
                warning at 1:9: unneeded parentheses
                  fnc five(): s32 -> 5;
                          ^^
            "#]],
        );
    }

    #[test]
    fn validation_unneeded_parens_multiline() {
        check_validation(
            "fnc main(\n) -> {};",
            ValidationDiagnosticKind::UnneededParens,
            8..11,
            expect![[r#"
                warning at 1:9: unneeded parentheses
                          v
                  fnc main(
                  ) -> {};
                  ^
            "#]],
        );
    }

    #[test]
    fn indexing_function_already_defined() {
        check_indexing(
            "fnc do_thing -> {};",
            IndexingDiagnosticKind::FunctionAlreadyDefined { name: "do_thing".to_string() },
            0..19,
            expect![[r#"
                error at 1:1: function `do_thing` already defined
                  fnc do_thing -> {};
                  ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn lowering_out_of_range_int_literal() {
        check_lowering(
            "1000000000000000;",
            LoweringDiagnosticKind::OutOfRangeIntLiteral,
            0..16,
            expect![[r#"
                error at 1:1: integer literal out of range
                  1000000000000000;
                  ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn lowering_undefined_local() {
        check_lowering(
            "foo + 1;",
            LoweringDiagnosticKind::UndefinedLocal { name: "foo".to_string() },
            0..3,
            expect![[r#"
                error at 1:1: undefined variable `foo`
                  foo + 1;
                  ^^^
            "#]],
        );
    }

    #[test]
    fn lowering_mismatched_arg_count() {
        check_lowering(
            "add 1, 2, 3",
            LoweringDiagnosticKind::MismatchedArgCount {
                name: "add".to_string(),
                expected: 2,
                got: 3,
            },
            0..3,
            expect![[r#"
                error at 1:1: `add` expected 2 arguments, but got 3
                  add 1, 2, 3
                  ^^^
            "#]],
        );
    }

    #[test]
    fn lowering_called_local() {
        check_lowering(
            "frobnicate a, b",
            LoweringDiagnosticKind::CalledLocal { name: "frobnicate".to_string() },
            0..10,
            expect![[r#"
                error at 1:1: tried to call `frobnicate`, which is a variable, not a function
                  frobnicate a, b
                  ^^^^^^^^^^
            "#]],
        );
    }
}
