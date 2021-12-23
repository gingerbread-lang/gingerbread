mod line_index;

use line_index::{ColNr, LineIndex, LineNr};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, SemanticToken, TextDocumentContentChangeEvent,
    Url,
};
use parser::{Parse, SyntaxErrorKind};
use std::collections::HashMap;
use text_size::{TextRange, TextSize};
use token::TokenKind;

#[derive(Default)]
pub struct GlobalState {
    analyses: HashMap<Url, Analysis>,
}

struct Analysis {
    content: String,
    line_index: LineIndex,
    parse: Parse,
}

impl GlobalState {
    pub fn open_file(&mut self, uri: Url, content: String) {
        self.analyses.insert(uri, Analysis::new(content));
    }

    pub fn apply_changes(&mut self, uri: &Url, changes: Vec<TextDocumentContentChangeEvent>) {
        self.analyses.get_mut(uri).unwrap().apply_changes(changes);
    }

    pub fn highlight(&self, uri: &Url) -> Vec<SemanticToken> {
        self.analyses[uri].highlight()
    }

    pub fn diagnostics(&self, uri: &Url) -> Vec<Diagnostic> {
        self.analyses[uri].diagnostics()
    }
}

impl Analysis {
    fn new(content: String) -> Self {
        let line_index = LineIndex::new(&content);

        let parse = {
            let tokens = lexer::lex(&content);
            parser::parse_source_file(&tokens)
        };

        Self { content, line_index, parse }
    }

    fn apply_changes(&mut self, changes: Vec<TextDocumentContentChangeEvent>) {
        for change in changes {
            let range = match change.range {
                Some(r) => convert_lsp_range(r, &self.line_index),
                None => {
                    // if no range is present, the text is considered
                    // the new full content of the document.
                    self.content = change.text;
                    continue;
                }
            };

            let start_idx = usize::from(range.start());
            let end_idx = usize::from(range.end());
            self.content.replace_range(start_idx..end_idx, &change.text);

            self.update_line_index();
        }

        self.update_parse();
    }

    fn highlight(&self) -> Vec<SemanticToken> {
        let mut data = Vec::new();

        let line_starts: Vec<_> = std::iter::once(0)
            .chain(self.content.match_indices('\n').map(|(idx, _)| idx as u32 + 1))
            .collect();

        let mut prev_token_position = None;

        for token in lexer::lex(&self.content) {
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

    fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for error in self.parse.errors() {
            let range = match error.kind {
                SyntaxErrorKind::Missing { offset } => {
                    let pos = convert_text_size(offset, &self.line_index);

                    Range {
                        start: pos,
                        end: Position { line: pos.line, character: pos.character + 1 },
                    }
                }

                SyntaxErrorKind::Unexpected { range, .. } => Range {
                    start: convert_text_size(range.start(), &self.line_index),
                    end: convert_text_size(range.end(), &self.line_index),
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

    fn update_line_index(&mut self) {
        self.line_index = LineIndex::new(&self.content);
    }

    fn update_parse(&mut self) {
        let tokens = lexer::lex(&self.content);
        self.parse = parser::parse_source_file(&tokens);
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
