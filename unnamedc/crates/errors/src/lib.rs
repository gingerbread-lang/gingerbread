use ast::validation::{ValidationError, ValidationErrorKind};
use ast::AstNode;
use hir_lower::{LowerError, LowerErrorKind, SourceMap};
use hir_ty::{Ty, TyError, TyErrorKind};
use parser::error::{ExpectedSyntax, ParseError, ParseErrorKind};
use std::convert::{TryFrom, TryInto};
use std::fmt;
use text_size::{TextRange, TextSize};
use token::TokenKind;

pub struct Error(ErrorRepr);

enum ErrorRepr {
    Parse(ParseError),
    Validation(ValidationError),
    Lower(LowerError),
    Ty { kind: TyErrorKind, range: TextRange },
}

impl Error {
    pub fn from_parse_error(error: ParseError) -> Self {
        Self(ErrorRepr::Parse(error))
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

    pub fn display(&self, input: &str) -> Vec<String> {
        let range = self.range();

        let start_line_column = text_size_to_line_column(range.start(), input);

        // we subtract 1 since end_line_column is inclusive,
        // unlike TextRange which is always exclusive
        let end_line_column = text_size_to_line_column(range.end() - TextSize::from(1), input);

        let mut lines = vec![self.header(&start_line_column)];

        input_snippet(input, start_line_column, end_line_column, range, &mut lines);

        lines
    }

    pub fn range(&self) -> TextRange {
        match self.0 {
            ErrorRepr::Parse(ParseError { kind: ParseErrorKind::Missing { offset }, .. }) => {
                TextRange::new(offset, offset + TextSize::from(1))
            }
            ErrorRepr::Parse(ParseError {
                kind: ParseErrorKind::Unexpected { range, .. }, ..
            }) => range,
            ErrorRepr::Validation(ValidationError { range, .. }) => range,
            ErrorRepr::Lower(LowerError { range, .. }) => range,
            ErrorRepr::Ty { range, .. } => range,
        }
    }

    fn header(&self, start_line_column: &LineColumn) -> String {
        match &self.0 {
            ErrorRepr::Parse(error) => parse_error_header(error, start_line_column),
            ErrorRepr::Validation(error) => validation_error_header(error, start_line_column),
            ErrorRepr::Lower(error) => lower_error_header(error, start_line_column),
            ErrorRepr::Ty { kind, .. } => ty_error_header(kind, start_line_column),
        }
    }
}

fn input_snippet(
    input: &str,
    start_line_column: LineColumn,
    end_line_column: LineColumn,
    range: TextRange,
    lines: &mut Vec<String>,
) {
    const PADDING: &str = "  ";
    const POINTER_UP: &str = "^";
    const POINTER_DOWN: &str = "v";

    let file_lines: Vec<_> = input.lines().collect();

    let is_single_line = start_line_column.line == end_line_column.line;
    if is_single_line {
        lines.push(format!("{}{}", PADDING, file_lines[start_line_column.line]));

        lines.push(format!(
            "{}{}{}",
            PADDING,
            " ".repeat(start_line_column.column),
            POINTER_UP.repeat(range.len().try_into().unwrap())
        ));

        return;
    }

    let first_line = file_lines[start_line_column.line];
    lines.push(format!(
        "{}{}{}",
        PADDING,
        " ".repeat(start_line_column.column),
        POINTER_DOWN.repeat(first_line.len() - start_line_column.column)
    ));
    lines.push(format!("{}{}", PADDING, first_line));

    for line in &file_lines[start_line_column.line + 1..end_line_column.line] {
        lines.push(format!("{}{}", PADDING, line));
    }

    let last_line = file_lines[end_line_column.line];
    lines.push(format!("{}{}", PADDING, last_line));
    lines.push(format!("{}{}", PADDING, POINTER_UP.repeat(end_line_column.column + 1)));
}

fn parse_error_header(parse_error: &ParseError, start_line_column: &LineColumn) -> String {
    let mut header = format!("syntax error at {}: ", start_line_column);

    let write_expected_syntaxes = |buf: &mut String| match parse_error.expected_syntax {
        ExpectedSyntax::Named(name) => buf.push_str(name),
        ExpectedSyntax::Unnamed(kind) => buf.push_str(format_kind(kind)),
    };

    match parse_error.kind {
        ParseErrorKind::Missing { .. } => {
            header.push_str("missing ");
            write_expected_syntaxes(&mut header);
        }
        ParseErrorKind::Unexpected { found, .. } => {
            header.push_str("expected ");
            write_expected_syntaxes(&mut header);
            header.push_str(&format!(" but found {}", format_kind(found)));
        }
    }

    header
}

fn validation_error_header(
    validation_error: &ValidationError,
    start_line_column: &LineColumn,
) -> String {
    let mut header = format!("syntax error at {}: ", start_line_column);

    match validation_error.kind {
        ValidationErrorKind::IntLiteralTooBig => header.push_str("integer literal too large"),
    }

    header
}

fn lower_error_header(lower_error: &LowerError, start_line_column: &LineColumn) -> String {
    match lower_error.kind {
        LowerErrorKind::UndefinedVar { ref name } => {
            format!("undefined variable at {}: `{}` has not been defined", start_line_column, name)
        }
    }
}

fn ty_error_header(ty_error_kind: &TyErrorKind, start_line_column: &LineColumn) -> String {
    match ty_error_kind {
        TyErrorKind::Mismatch { expected, found } => format!(
            "type mismatch at {}: expected {} but found {}",
            start_line_column,
            format_ty(*expected),
            format_ty(*found)
        ),
    }
}

fn text_size_to_line_column(text_size: TextSize, input: &str) -> LineColumn {
    // the collect into a Vec followed by an immediate .into_iter() call
    // is needed so we have an ExactSizeIterator to call .rev() on
    #[allow(clippy::needless_collect)]
    let line_idxs: Vec<_> = input.match_indices('\n').map(|(idx, _)| idx).collect();

    let (line, line_start_idx) = line_idxs
        .into_iter()
        .enumerate()
        .rev()
        .find(|(_, line_start_idx)| *line_start_idx < text_size.try_into().unwrap())
        .map(|(idx, line_start_idx)| (idx + 1, line_start_idx + 1))
        .unwrap_or((0, 0));

    let column = usize::try_from(text_size).unwrap() - line_start_idx;

    LineColumn { line, column }
}

#[derive(Debug)]
struct LineColumn {
    line: usize,
    column: usize,
}

impl fmt::Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
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
        TokenKind::Arrow => "`->`",
        TokenKind::LParen => "`(`",
        TokenKind::RParen => "`)`",
        TokenKind::LBrace => "`{`",
        TokenKind::RBrace => "`}`",
        TokenKind::Whitespace => "whitespace",
        TokenKind::Error => "an unrecognized token",
    }
}

fn format_ty(ty: Ty) -> &'static str {
    match ty {
        Ty::Unknown => "an unknown type",
        Ty::Int => "integer",
        Ty::String => "string",
        Ty::Unit => "unit",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use hir_ty::Ty;
    use parser::error::{ExpectedSyntax, ParseErrorKind};
    use std::ops::Range as StdRange;

    fn check_parse_error(
        input: &str,
        expected_syntax: ExpectedSyntax,
        kind: ParseErrorKind,
        formatted: Expect,
    ) {
        let error = Error::from_parse_error(ParseError { expected_syntax, kind });
        formatted.assert_eq(&format!("{}\n", error.display(input).join("\n")));
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

        formatted.assert_eq(&format!("{}\n", error.display(input).join("\n")));
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

        formatted.assert_eq(&format!("{}\n", error.display(input).join("\n")));
    }

    fn check_ty_error(input: &str, kind: TyErrorKind, range: StdRange<u32>, formatted: Expect) {
        let error = Error(ErrorRepr::Ty {
            kind,
            range: TextRange::new(range.start.into(), range.end.into()),
        });

        formatted.assert_eq(&format!("{}\n", error.display(input).join("\n")));
    }

    #[test]
    fn parse_error_unexpected() {
        check_parse_error(
            "let *",
            ExpectedSyntax::Unnamed(TokenKind::Ident),
            ParseErrorKind::Unexpected {
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
    fn parse_error_missing() {
        check_parse_error(
            "let idx",
            ExpectedSyntax::Unnamed(TokenKind::Eq),
            ParseErrorKind::Missing { offset: 7.into() },
            expect![[r#"
                syntax error at 1:8: missing `=`
                  let idx
                         ^
            "#]],
        );
    }

    #[test]
    fn parse_error_missing_at_end_of_line() {
        check_parse_error(
            "let a =\nlet b = a",
            ExpectedSyntax::Named("expression"),
            ParseErrorKind::Missing { offset: 7.into() },
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
    fn lower_error_undefined_var() {
        check_lower_error(
            "let the_value = 10\nteh_value",
            LowerErrorKind::UndefinedVar { name: "teh_value".to_string() },
            19..28,
            expect![[r#"
                undefined variable at 2:1: `teh_value` has not been defined
                  teh_value
                  ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn ty_error_mismatch() {
        check_ty_error(
            "10 * \"a\"",
            TyErrorKind::Mismatch { expected: Ty::Int, found: Ty::String },
            5..8,
            expect![[r#"
                type mismatch at 1:6: expected integer but found string
                  10 * "a"
                       ^^^
            "#]],
        );
    }

    #[test]
    fn ty_error_multiline_mismatch() {
        check_ty_error(
            "foo - (\n\"bar\"\n)",
            TyErrorKind::Mismatch { expected: Ty::Int, found: Ty::String },
            6..15,
            expect![[r#"
                type mismatch at 1:7: expected integer but found string
                        v
                  foo - (
                  "bar"
                  )
                  ^
            "#]],
        );
    }
}
