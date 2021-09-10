use ast::validation::{ValidationError, ValidationErrorKind};
use ast::AstNode;
use hir_lower::SourceMap;
use hir_ty::{Ty, TyError, TyErrorKind};
use parser::error::{ExpectedSyntax, ParseError, ParseErrorData};
use std::convert::{TryFrom, TryInto};
use std::fmt;
use text_size::{TextRange, TextSize};
use token::TokenKind;

pub struct Error {
    pub range: TextRange,
    pub kind: ErrorKind,
}

pub enum ErrorKind {
    Parse(ParseErrorData),
    Validation(ValidationErrorKind),
    Ty(TyErrorKind),
}

impl Error {
    pub fn from_parse_error(error: ParseError) -> Self {
        Self { range: error.range, kind: ErrorKind::Parse(error.data) }
    }

    pub fn from_validation_error(error: ValidationError) -> Self {
        Self { range: error.range, kind: ErrorKind::Validation(error.kind) }
    }

    pub fn from_ty_error(error: TyError, source_map: &SourceMap) -> Self {
        Self { range: source_map.expr_map[error.expr].range(), kind: ErrorKind::Ty(error.kind) }
    }

    pub fn display(&self, original_file: &str) -> Vec<String> {
        const PADDING: &str = "  ";
        const POINTER_UP: &str = "^";
        const POINTER_DOWN: &str = "v";

        let start_line_column = text_size_to_line_column(self.range.start(), original_file);

        // we subtract 1 since end_line_column is inclusive,
        // unlike TextRange which is always exclusive
        let end_line_column =
            text_size_to_line_column(self.range.end() - TextSize::from(1), original_file);

        let header = match &self.kind {
            ErrorKind::Parse(parse_error_data) => {
                let mut header = format!("syntax error at {}: expected", start_line_column);

                for (idx, expected_syntax) in parse_error_data.expected_syntaxes.iter().enumerate()
                {
                    if idx == 0 {
                        header.push(' ');
                    } else if idx == parse_error_data.expected_syntaxes.len() - 1 {
                        header.push_str(" or ");
                    } else {
                        header.push_str(", ");
                    }

                    match expected_syntax {
                        ExpectedSyntax::Named(name) => header.push_str(name),
                        ExpectedSyntax::Unnamed(kind) => header.push_str(format_kind(*kind)),
                    }
                }

                if let Some(found) = parse_error_data.found {
                    header.push_str(&format!(" but found {}", format_kind(found)));
                }

                header
            }

            ErrorKind::Validation(validation_error_kind) => {
                let mut header = format!("syntax error at {}: ", start_line_column);

                match validation_error_kind {
                    ValidationErrorKind::IntLiteralTooBig => {
                        header.push_str("integer literal too large")
                    }
                }

                header
            }

            ErrorKind::Ty(ty_error_kind) => match ty_error_kind {
                TyErrorKind::Mismatch { expected, found } => format!(
                    "type mismatch at {}: expected {} but found {}",
                    start_line_column,
                    format_ty(*expected),
                    format_ty(*found)
                ),

                TyErrorKind::UndefinedVar { name } => format!(
                    "undefined variable at {}: `{}` has not been defined",
                    start_line_column, name
                ),
            },
        };

        let mut lines = vec![header];

        let file_lines: Vec<_> = original_file.lines().collect();

        let is_single_line = start_line_column.line == end_line_column.line;
        if is_single_line {
            lines.push(format!("{}{}", PADDING, file_lines[start_line_column.line]));

            lines.push(format!(
                "{}{}{}",
                PADDING,
                " ".repeat(start_line_column.column),
                POINTER_UP.repeat(self.range.len().try_into().unwrap())
            ));
        } else {
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

        lines
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
        TokenKind::Ident => "identifier",
        TokenKind::Int => "integer literal",
        TokenKind::String => "string literal",
        TokenKind::Plus => "`+`",
        TokenKind::Hyphen => "`-`",
        TokenKind::Asterisk => "`*`",
        TokenKind::Slash => "`/`",
        TokenKind::Eq => "`=`",
        TokenKind::LParen => "`(`",
        TokenKind::RParen => "`)`",
        TokenKind::Whitespace => "whitespace",
        TokenKind::Error => "an unrecognized token",
    }
}

fn format_ty(ty: Ty) -> &'static str {
    match ty {
        Ty::Unknown => "an unknown type",
        Ty::Int => "integer",
        Ty::String => "string",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use hir_ty::Ty;
    use parser::error::ExpectedSyntax;
    use std::ops::Range as StdRange;

    fn check_parse_error<const NUM_EXPECTED: usize>(
        original_file: &str,
        expected_syntaxes: [ExpectedSyntax; NUM_EXPECTED],
        found: Option<TokenKind>,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let error = Error {
            range: TextRange::new(range.start.into(), range.end.into()),
            kind: ErrorKind::Parse(ParseErrorData {
                expected_syntaxes: IntoIterator::into_iter(expected_syntaxes).collect(),
                found,
            }),
        };

        formatted.assert_eq(&format!("{}\n", error.display(original_file).join("\n")));
    }

    fn check_validation_error(
        original_file: &str,
        kind: ValidationErrorKind,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let error = Error {
            range: TextRange::new(range.start.into(), range.end.into()),
            kind: ErrorKind::Validation(kind),
        };

        formatted.assert_eq(&format!("{}\n", error.display(original_file).join("\n")));
    }

    fn check_ty_error(
        original_file: &str,
        kind: TyErrorKind,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let error = Error {
            range: TextRange::new(range.start.into(), range.end.into()),
            kind: ErrorKind::Ty(kind),
        };

        formatted.assert_eq(&format!("{}\n", error.display(original_file).join("\n")));
    }

    #[test]
    fn parse_error_did_find_expected_1() {
        check_parse_error(
            "let *",
            [ExpectedSyntax::Unnamed(TokenKind::Ident)],
            Some(TokenKind::Asterisk),
            4..5,
            expect![[r#"
                syntax error at 1:5: expected identifier but found `*`
                  let *
                      ^
            "#]],
        );
    }

    #[test]
    fn parse_error_did_not_find_expected_1() {
        check_parse_error(
            "let idx",
            [ExpectedSyntax::Unnamed(TokenKind::Eq)],
            None,
            4..7,
            expect![[r#"
                syntax error at 1:5: expected `=`
                  let idx
                      ^^^
            "#]],
        );
    }

    #[test]
    fn parse_error_did_find_expected_2() {
        check_parse_error(
            "let a = 10\na + +",
            [ExpectedSyntax::Unnamed(TokenKind::Int), ExpectedSyntax::Unnamed(TokenKind::Ident)],
            Some(TokenKind::Plus),
            15..16,
            expect![[r#"
                syntax error at 2:5: expected identifier or integer literal but found `+`
                  a + +
                      ^
            "#]],
        );
    }

    #[test]
    fn parse_error_did_not_find_expected_multiple() {
        check_parse_error(
            "1000 @",
            [
                ExpectedSyntax::Unnamed(TokenKind::Plus),
                ExpectedSyntax::Unnamed(TokenKind::Hyphen),
                ExpectedSyntax::Unnamed(TokenKind::Asterisk),
                ExpectedSyntax::Unnamed(TokenKind::Slash),
            ],
            Some(TokenKind::Error),
            5..6,
            expect![[r#"
                syntax error at 1:6: expected `+`, `-`, `*` or `/` but found an unrecognized token
                  1000 @
                       ^
            "#]],
        );
    }

    #[test]
    fn parse_error_multiple_expected_syntaxes() {
        check_parse_error(
            "let bar = foo\nlet baz = bar\nbaz -",
            [
                ExpectedSyntax::Named("statement"),
                ExpectedSyntax::Unnamed(TokenKind::Asterisk),
                ExpectedSyntax::Named("expression"),
            ],
            None,
            32..33,
            expect![[r#"
                syntax error at 3:5: expected expression, statement or `*`
                  baz -
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
    fn ty_error_unknown_variable() {
        check_ty_error(
            "let the_value = 10\nteh_value",
            TyErrorKind::UndefinedVar { name: "teh_value".to_string() },
            19..28,
            expect![[r#"
                undefined variable at 2:1: `teh_value` has not been defined
                  teh_value
                  ^^^^^^^^^
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
