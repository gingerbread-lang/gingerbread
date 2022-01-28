use ast::validation::{ValidationError, ValidationErrorKind};
use hir::{IndexingError, IndexingErrorKind};
use line_index::{ColNr, LineIndex, LineNr};
use parser::{ExpectedSyntax, SyntaxError, SyntaxErrorKind};
use std::convert::TryInto;
use text_size::{TextRange, TextSize};
use token::TokenKind;

pub struct Error(ErrorRepr);

enum ErrorRepr {
    Syntax(SyntaxError),
    Validation(ValidationError),
    Indexing(IndexingError),
}

impl Error {
    pub fn from_syntax_error(error: SyntaxError) -> Self {
        Self(ErrorRepr::Syntax(error))
    }

    pub fn from_validation_error(error: ValidationError) -> Self {
        Self(ErrorRepr::Validation(error))
    }

    pub fn from_indexing_error(error: IndexingError) -> Self {
        Self(ErrorRepr::Indexing(error))
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
            ErrorRepr::Indexing(IndexingError { range, .. }) => range,
        }
    }

    pub fn kind(&self) -> &'static str {
        match &self.0 {
            ErrorRepr::Syntax(_) | ErrorRepr::Validation(_) => "syntax error",
            ErrorRepr::Indexing(_) => "error",
        }
    }

    pub fn message(&self) -> String {
        match &self.0 {
            ErrorRepr::Syntax(error) => syntax_error_message(error),
            ErrorRepr::Validation(error) => validation_error_message(error),
            ErrorRepr::Indexing(error) => indexing_error_message(error),
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

fn indexing_error_message(indexing_error: &IndexingError) -> String {
    match &indexing_error.kind {
        IndexingErrorKind::FunctionAlreadyDefined { name } => {
            format!("function `{}` already defined", name)
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

    fn check_indexing_error(
        input: &str,
        kind: IndexingErrorKind,
        range: StdRange<u32>,
        formatted: Expect,
    ) {
        let error = Error::from_indexing_error(IndexingError {
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
            "let = 10;",
            ExpectedSyntax::Named("variable name"),
            SyntaxErrorKind::Missing { offset: 3.into() },
            expect![[r#"
                syntax error at 1:4: missing variable name
                  let = 10;
                     ^
            "#]],
        );
    }

    #[test]
    fn syntax_error_missing_at_end_of_line() {
        check_syntax_error(
            "let a =\nlet b = a;",
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
    fn indexing_error_function_already_defined() {
        check_indexing_error(
            "fnc do_thing -> {};",
            IndexingErrorKind::FunctionAlreadyDefined { name: "do_thing".to_string() },
            0..19,
            expect![[r#"
                error at 1:1: function `do_thing` already defined
                  fnc do_thing -> {};
                  ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }
}
