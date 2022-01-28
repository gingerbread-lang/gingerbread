use ast::validation::{ValidationError, ValidationErrorKind};
use ast::AstNode;
use hir_lower::{LowerError, LowerErrorKind, SourceMap};
use hir_ty::{TyError, TyErrorKind};
use line_index::{ColNr, LineIndex, LineNr};
use parser::{ExpectedSyntax, SyntaxError, SyntaxErrorKind};
use std::convert::TryInto;
use text_size::{TextRange, TextSize};
use token::TokenKind;

pub struct Error(ErrorRepr);

enum ErrorRepr {
    Syntax(SyntaxError),
    Validation(ValidationError),
    Lower(LowerError),
    Ty { kind: TyErrorKind, range: TextRange },
}

impl Error {
    pub fn from_syntax_error(error: SyntaxError) -> Self {
        Self(ErrorRepr::Syntax(error))
    }

    pub fn from_validation_error(error: ValidationError) -> Self {
        Self(ErrorRepr::Validation(error))
    }

    pub fn from_lower_error(error: LowerError) -> Self {
        Self(ErrorRepr::Lower(error))
    }

    pub fn from_ty_error(error: TyError, source_map: &SourceMap) -> Self {
        Self(ErrorRepr::Ty { kind: error.kind, range: source_map.expr_map[error.expr].range() })
    }

    pub fn display(&self, input: &str, line_index: &LineIndex) -> Vec<String> {
        let range = self.range();

        let (start_line, start_col) = line_index.line_col(range.start());

        // we subtract 1 since end_line_column is inclusive,
        // unlike TextRange which is always exclusive
        let (end_line, end_col) = line_index.line_col(range.end() - TextSize::from(1));

        let mut lines = vec![format!(
            "{} at {}:{}: {}",
            self.kind(),
            start_line.0 + 1,
            start_col.0 + 1,
            self.message()
        )];

        input_snippet(input, start_line, start_col, end_line, end_col, range, &mut lines);

        lines
    }

    pub fn range(&self) -> TextRange {
        match self.0 {
            ErrorRepr::Syntax(SyntaxError {
                kind: SyntaxErrorKind::Missing { offset }, ..
            }) => TextRange::new(offset, offset + TextSize::from(1)),
            ErrorRepr::Syntax(SyntaxError {
                kind: SyntaxErrorKind::Unexpected { range, .. },
                ..
            }) => range,
            ErrorRepr::Validation(ValidationError { range, .. }) => range,
            ErrorRepr::Lower(LowerError { range, .. }) => range,
            ErrorRepr::Ty { range, .. } => range,
        }
    }

    pub fn kind(&self) -> &'static str {
        match &self.0 {
            ErrorRepr::Syntax(_) | ErrorRepr::Validation(_) => "syntax error",

            ErrorRepr::Lower(error) => match error.kind {
                LowerErrorKind::UndefinedVariableOrFunction { .. } => {
                    "undefined variable or zero-parameter function"
                }
                LowerErrorKind::UndefinedFunction { .. } => "undefined function",
                LowerErrorKind::UndefinedTy { .. } => "undefined type",
            },

            ErrorRepr::Ty { kind, .. } => match kind {
                TyErrorKind::Mismatch { .. } => "type mismatch",
                TyErrorKind::MismatchedArgCount { .. } => "mismatched argument count",
            },
        }
    }

    pub fn message(&self) -> String {
        match &self.0 {
            ErrorRepr::Syntax(error) => syntax_error_message(error),
            ErrorRepr::Validation(error) => validation_error_message(error),
            ErrorRepr::Lower(error) => lower_error_message(error),
            ErrorRepr::Ty { kind, .. } => ty_error_message(kind),
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

fn syntax_error_message(syntax_error: &SyntaxError) -> String {
    let write_expected_syntax = |buf: &mut String| match syntax_error.expected_syntax {
        ExpectedSyntax::Named(name) => buf.push_str(name),
        ExpectedSyntax::Unnamed(kind) => buf.push_str(format_kind(kind)),
    };

    let mut message = String::new();

    match syntax_error.kind {
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

fn validation_error_message(validation_error: &ValidationError) -> String {
    match validation_error.kind {
        ValidationErrorKind::IntLiteralTooBig => "integer literal too large".to_string(),
        ValidationErrorKind::UnneededParens => "unneeded parentheses".to_string(),
    }
}

fn lower_error_message(lower_error: &LowerError) -> String {
    match lower_error.kind {
        LowerErrorKind::UndefinedVariableOrFunction { ref name } => {
            format!("`{}` has not been defined", name)
        }
        LowerErrorKind::UndefinedFunction { ref name } => {
            format!("`{}` has not been defined", name)
        }
        LowerErrorKind::UndefinedTy { ref name } => {
            format!("`{}` has not been defined", name)
        }
    }
}

fn ty_error_message(ty_error_kind: &TyErrorKind) -> String {
    match ty_error_kind {
        TyErrorKind::Mismatch { expected, found } => {
            format!("expected {} but found {}", format_ty(*expected), format_ty(*found))
        }
        TyErrorKind::MismatchedArgCount { expected, found } => {
            format!("expected {} but found {}", expected, found)
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

fn format_ty(ty: hir::Ty) -> &'static str {
    match ty {
        hir::Ty::Unknown => "an unknown type",
        hir::Ty::S32 => "s32",
        hir::Ty::String => "string",
        hir::Ty::Unit => "unit",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use parser::{ExpectedSyntax, SyntaxErrorKind};
    use std::ops::Range as StdRange;

    fn check_syntax_error(
        input: &str,
        expected_syntax: ExpectedSyntax,
        kind: SyntaxErrorKind,
        formatted: Expect,
    ) {
        let error = Error::from_syntax_error(SyntaxError { expected_syntax, kind });
        formatted
            .assert_eq(&format!("{}\n", error.display(input, &LineIndex::new(input)).join("\n")));
    }

    fn check_validation_error(
        input: &str,
        kind: ValidationErrorKind,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let error = Error::from_validation_error(ValidationError {
            kind,
            range: TextRange::new(range.start.into(), range.end.into()),
        });

        formatted
            .assert_eq(&format!("{}\n", error.display(input, &LineIndex::new(input)).join("\n")));
    }

    fn check_lower_error(
        input: &str,
        kind: LowerErrorKind,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let error = Error::from_lower_error(LowerError {
            kind,
            range: TextRange::new(range.start.into(), range.end.into()),
        });

        formatted
            .assert_eq(&format!("{}\n", error.display(input, &LineIndex::new(input)).join("\n")));
    }

    fn check_ty_error(input: &str, kind: TyErrorKind, range: StdRange<u32>, formatted: Expect) {
        let error = Error(ErrorRepr::Ty {
            kind,
            range: TextRange::new(range.start.into(), range.end.into()),
        });

        formatted
            .assert_eq(&format!("{}\n", error.display(input, &LineIndex::new(input)).join("\n")));
    }

    #[test]
    fn syntax_error_unexpected() {
        check_syntax_error(
            "let *",
            ExpectedSyntax::Unnamed(TokenKind::Ident),
            SyntaxErrorKind::Unexpected {
                found: TokenKind::Asterisk,
                range: TextRange::new(4.into(), 5.into()),
            },
            expect![[r#"
                syntax error at 1:5: expected identifier but found `*`
                  let *
                      ^
            "#]],
        );
    }

    #[test]
    fn syntax_error_missing() {
        check_syntax_error(
            "let idx",
            ExpectedSyntax::Unnamed(TokenKind::Eq),
            SyntaxErrorKind::Missing { offset: 7.into() },
            expect![[r#"
                syntax error at 1:8: missing `=`
                  let idx
                         ^
            "#]],
        );
    }

    #[test]
    fn syntax_error_missing_at_end_of_line() {
        check_syntax_error(
            "let a =\nlet b = a",
            ExpectedSyntax::Named("expression"),
            SyntaxErrorKind::Missing { offset: 7.into() },
            expect![[r#"
                syntax error at 1:8: missing expression
                  let a =
                         ^
            "#]],
        );
    }

    #[test]
    fn validation_error_int_literal_too_big() {
        check_validation_error(
            "let a = 9999999999999999999",
            ValidationErrorKind::IntLiteralTooBig,
            8..27,
            expect![[r#"
                syntax error at 1:9: integer literal too large
                  let a = 9999999999999999999
                          ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn validation_error_unneeded_parens() {
        check_validation_error(
            "fnc five(): s32 -> 5;",
            ValidationErrorKind::UnneededParens,
            8..10,
            expect![[r#"
                syntax error at 1:9: unneeded parentheses
                  fnc five(): s32 -> 5;
                          ^^
            "#]],
        );
    }

    #[test]
    fn lower_error_undefined_variable_or_function() {
        check_lower_error(
            "let the_value = 10\nteh_value",
            LowerErrorKind::UndefinedVariableOrFunction { name: "teh_value".to_string() },
            19..28,
            expect![[r#"
                undefined variable or zero-parameter function at 2:1: `teh_value` has not been defined
                  teh_value
                  ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn lower_error_undefined_function() {
        check_lower_error(
            "frobnicate 10, 20",
            LowerErrorKind::UndefinedFunction { name: "frobnicate".to_string() },
            0..10,
            expect![[r#"
                undefined function at 1:1: `frobnicate` has not been defined
                  frobnicate 10, 20
                  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn lower_error_undefined_ty() {
        check_lower_error(
            "fnc f(param: abc)",
            LowerErrorKind::UndefinedTy { name: "abc".to_string() },
            13..16,
            expect![[r#"
                undefined type at 1:14: `abc` has not been defined
                  fnc f(param: abc)
                               ^^^
            "#]],
        );
    }

    #[test]
    fn ty_error_mismatch() {
        check_ty_error(
            "10 * \"a\"",
            TyErrorKind::Mismatch { expected: hir::Ty::S32, found: hir::Ty::String },
            5..8,
            expect![[r#"
                type mismatch at 1:6: expected s32 but found string
                  10 * "a"
                       ^^^
            "#]],
        );
    }

    #[test]
    fn ty_error_multiline_mismatch() {
        check_ty_error(
            "foo - (\n\"bar\"\n)",
            TyErrorKind::Mismatch { expected: hir::Ty::S32, found: hir::Ty::String },
            6..15,
            expect![[r#"
                type mismatch at 1:7: expected s32 but found string
                        v
                  foo - (
                  "bar"
                  )
                  ^
            "#]],
        );
    }

    #[test]
    fn ty_error_mismatched_arg_count() {
        check_ty_error(
            "add 5",
            TyErrorKind::MismatchedArgCount { expected: 2, found: 1 },
            0..5,
            expect![[r#"
                mismatched argument count at 1:1: expected 2 but found 1
                  add 5
                  ^^^^^
            "#]],
        );
    }
}
