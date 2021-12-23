mod line_index;

use line_index::{ColNr, LineIndex, LineNr};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, SemanticToken, TextDocumentContentChangeEvent,
    Url,
};
use parser::{Parse, SyntaxErrorKind};
use text_size::{TextRange, TextSize};
use token::TokenKind;

pub struct GlobalState(GlobalStateRepr);

enum GlobalStateRepr {
    OpenedFile { content: String, uri: Url, line_index: LineIndex, parse: Parse },
    NotOpenedFile,
}

impl Default for GlobalState {
    fn default() -> Self {
        Self(GlobalStateRepr::NotOpenedFile)
    }
}

impl GlobalState {
    pub fn open_file(&mut self, content: String, uri: Url) {
        let parse = {
            let tokens = lexer::lex(&content);
            parser::parse_source_file(&tokens)
        };

        let line_index = LineIndex::new(&content);

        self.0 = GlobalStateRepr::OpenedFile { content, uri, line_index, parse };
    }

    pub fn apply_changes(&mut self, changes: Vec<TextDocumentContentChangeEvent>) {
        match &mut self.0 {
            GlobalStateRepr::OpenedFile { content, line_index, .. } => {
                for change in changes {
                    let range = match change.range {
                        Some(r) => convert_lsp_range(r, line_index),
                        None => {
                            // if no range is present, the text is considered
                            // the new full content of the document.
                            *content = change.text;
                            continue;
                        }
                    };

                    let start_idx = usize::from(range.start());
                    let end_idx = usize::from(range.end());
                    content.replace_range(start_idx..end_idx, &change.text);

                    *line_index = LineIndex::new(content);
                }

                self.update_parse();
            }

            GlobalStateRepr::NotOpenedFile => unreachable!(),
        }
    }

    pub fn highlight(&self) -> Vec<SemanticToken> {
        match &self.0 {
            GlobalStateRepr::OpenedFile { content, .. } => {
                let mut data = Vec::new();

                let line_starts: Vec<_> = std::iter::once(0)
                    .chain(content.match_indices('\n').map(|(idx, _)| idx as u32 + 1))
                    .collect();

                let mut prev_token_position = None;

                for token in lexer::lex(content) {
                    if matches!(
                        token.kind,
                        TokenKind::Eq
                            | TokenKind::Colon
                            | TokenKind::Comma
                            | TokenKind::Semicolon
                            | TokenKind::Arrow
                            | TokenKind::LParen
                            | TokenKind::RParen
                            | TokenKind::LBrace
                            | TokenKind::RBrace
                            | TokenKind::Whitespace
                            | TokenKind::Error
                    ) {
                        continue;
                    }

                    let (line_number, line_start) = line_starts
                        .iter()
                        .enumerate()
                        .rev()
                        .find(|(_, line_start)| **line_start <= u32::from(token.range.start()))
                        .unwrap();

                    let line_number = line_number as u32;

                    let column_number = u32::from(token.range.start()) - line_start;

                    let pos = match prev_token_position {
                        Some((prev_line_number, prev_column_number)) => {
                            let on_same_line_as_prev_token = prev_line_number == line_number;

                            if on_same_line_as_prev_token {
                                (0, column_number - prev_column_number)
                            } else {
                                (line_number - prev_line_number, column_number)
                            }
                        }
                        None => (line_number, column_number),
                    };

                    prev_token_position = Some((line_number, column_number));

                    data.push(SemanticToken {
                        delta_line: pos.0,
                        delta_start: pos.1,
                        length: u32::from(token.range.len()),
                        token_type: match token.kind {
                            TokenKind::LetKw | TokenKind::FncKw => 0,
                            TokenKind::Ident => 1,
                            TokenKind::Int => 2,
                            TokenKind::String => 3,
                            TokenKind::Plus
                            | TokenKind::Hyphen
                            | TokenKind::Asterisk
                            | TokenKind::Slash => 4,
                            TokenKind::Comment => 5,
                            TokenKind::Eq
                            | TokenKind::Colon
                            | TokenKind::Comma
                            | TokenKind::Semicolon
                            | TokenKind::Arrow
                            | TokenKind::LParen
                            | TokenKind::RParen
                            | TokenKind::LBrace
                            | TokenKind::RBrace
                            | TokenKind::Whitespace
                            | TokenKind::Error => unreachable!(),
                        },
                        token_modifiers_bitset: 0,
                    });
                }

                data
            }
            GlobalStateRepr::NotOpenedFile => unreachable!(),
        }
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        match &self.0 {
            GlobalStateRepr::OpenedFile { line_index, parse, .. } => {
                let mut diagnostics = Vec::new();

                for error in parse.errors() {
                    let range = match error.kind {
                        SyntaxErrorKind::Missing { offset } => {
                            let pos = convert_text_size(offset, line_index);

                            Range {
                                start: pos,
                                end: Position { line: pos.line, character: pos.character + 1 },
                            }
                        }

                        SyntaxErrorKind::Unexpected { range, .. } => Range {
                            start: convert_text_size(range.start(), line_index),
                            end: convert_text_size(range.end(), line_index),
                        },
                    };

                    let diagnostic = Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("unnamedc".to_string()),
                        message: format!("{:?}", error),
                        related_information: None,
                        tags: None,
                        data: None,
                    };

                    diagnostics.push(diagnostic);
                }

                diagnostics
            }

            GlobalStateRepr::NotOpenedFile => unreachable!(),
        }
    }

    pub fn uri(&self) -> Option<Url> {
        match &self.0 {
            GlobalStateRepr::OpenedFile { uri, .. } => Some(uri.clone()),
            GlobalStateRepr::NotOpenedFile => None,
        }
    }

    fn update_parse(&mut self) {
        match &mut self.0 {
            GlobalStateRepr::OpenedFile { content, parse, .. } => {
                let tokens = lexer::lex(content);
                *parse = parser::parse_source_file(&tokens);
            }
            GlobalStateRepr::NotOpenedFile => unreachable!(),
        }
    }
}

fn convert_lsp_range(range: Range, line_index: &LineIndex) -> TextRange {
    TextRange::new(
        convert_lsp_position(range.start, line_index),
        convert_lsp_position(range.end, line_index),
    )
}

fn convert_lsp_position(position: Position, line_index: &LineIndex) -> TextSize {
    line_index[LineNr(position.line)] + TextSize::from(position.character)
}

fn convert_text_size(offset: TextSize, line_index: &LineIndex) -> Position {
    let (LineNr(line), ColNr(character)) = line_index.line_col(offset);
    Position { line, character }
}
