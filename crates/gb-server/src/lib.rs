mod capabilities;
pub use capabilities::capabilities;

use ide::GlobalState;
use line_index::{ColNr, LineIndex, LineNr};
use lsp::connection::Connection;
use lsp::proto::WriteMsgError;
use lsp_types::notification::{PublishDiagnostics, ShowMessage};
use lsp_types::request::SemanticTokensRefesh as SemanticTokensRefresh;
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    Location, MessageType, Position, PublishDiagnosticsParams, Range, SelectionRange,
    SelectionRangeParams, SemanticToken, SemanticTokens, SemanticTokensParams,
    SemanticTokensResult, ShowMessageParams, SymbolInformation, SymbolKind, WorkspaceSymbolParams,
};
use text_size::{TextRange, TextSize};

pub fn selection_range(
    params: SelectionRangeParams,
    global_state: &mut GlobalState,
) -> Vec<SelectionRange> {
    let line_index = global_state.line_index(&params.text_document.uri);

    params
        .positions
        .into_iter()
        .map(|position| {
            let offset = convert_lsp_position(position, line_index);
            let parent_ranges = global_state.parent_ranges(&params.text_document.uri, offset);

            let root = parent_ranges[0];
            let mut range =
                SelectionRange { range: convert_text_range(root, line_index), parent: None };

            for r in parent_ranges.into_iter().skip(1) {
                range = SelectionRange {
                    range: convert_text_range(r, line_index),
                    parent: Some(Box::new(range)),
                };
            }

            range
        })
        .collect()
}

pub fn workspace_symbol(
    params: WorkspaceSymbolParams,
    global_state: &mut GlobalState,
) -> Vec<SymbolInformation> {
    global_state
        .symbols()
        .into_iter()
        .filter(|symbol| symbol.name.contains(&params.query))
        .map(|symbol| {
            let line_index = global_state.line_index(&symbol.file);
            let range = convert_text_range(symbol.range, line_index);

            #[allow(deprecated)]
            SymbolInformation {
                name: symbol.name,
                kind: SymbolKind::FUNCTION,
                tags: None,
                deprecated: None,
                location: Location { uri: symbol.file, range },
                container_name: None,
            }
        })
        .collect()
}

pub fn semantic_tokens(
    params: SemanticTokensParams,
    global_state: &mut GlobalState,
) -> SemanticTokensResult {
    let mut tokens = Vec::new();
    let mut prev_token_position = None;
    let line_index = global_state.line_index(&params.text_document.uri);

    for highlight in global_state.highlight(&params.text_document.uri) {
        let (line, column) = line_index.line_col(highlight.range.start());

        let (delta_line, delta_column) = match prev_token_position {
            Some((prev_line, prev_column)) if prev_line == line => {
                (LineNr(0), column - prev_column)
            }
            Some((prev_line, _)) => (line - prev_line, column),
            None => (line, column),
        };

        prev_token_position = Some((line, column));

        tokens.push(SemanticToken {
            delta_line: delta_line.0,
            delta_start: delta_column.0,
            length: u32::from(highlight.range.len()),
            token_type: highlight.kind as u32,
            token_modifiers_bitset: highlight.modifiers.into_raw(),
        });
    }

    SemanticTokensResult::Tokens(SemanticTokens { result_id: None, data: tokens })
}

pub fn open_text_document(
    params: DidOpenTextDocumentParams,
    global_state: &mut GlobalState,
    connection: &mut Connection<'_>,
) -> Result<(), WriteMsgError> {
    match global_state.open_file(params.text_document.uri) {
        Ok(Ok(())) => publish_all_diagnostics(global_state, connection)?,
        Ok(Err(())) => connection.notify::<ShowMessage>(ShowMessageParams {
            typ: MessageType::ERROR,
            message: "file is not part of project".to_string(),
        })?,
        Err(e) => connection.notify::<ShowMessage>(ShowMessageParams {
            typ: MessageType::ERROR,
            message: e.to_string(),
        })?,
    }

    Ok(())
}

pub fn change_text_document(
    params: DidChangeTextDocumentParams,
    global_state: &mut GlobalState,
    connection: &mut Connection<'_>,
) -> Result<(), WriteMsgError> {
    global_state.update_contents(&params.text_document.uri, |content, line_index| {
        let mut line_index = line_index.clone();

        for change in params.content_changes {
            let range = match change.range {
                Some(r) => convert_lsp_range(r, &line_index),
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

            line_index = LineIndex::new(content);
        }
    });

    publish_all_diagnostics(global_state, connection)?;
    connection.make_request::<SemanticTokensRefresh>(())?;

    Ok(())
}

fn publish_all_diagnostics(
    global_state: &mut GlobalState,
    connection: &mut Connection<'_>,
) -> Result<(), WriteMsgError> {
    for (uri, diagnostics) in global_state.diagnostics() {
        let line_index = global_state.line_index(uri);

        let diagnostics = diagnostics
            .into_iter()
            .map(|diagnostic| {
                let severity = match diagnostic.severity() {
                    diagnostics::Severity::Warning => DiagnosticSeverity::WARNING,
                    diagnostics::Severity::Error => DiagnosticSeverity::ERROR,
                };

                Diagnostic {
                    range: convert_text_range(diagnostic.range(), line_index),
                    severity: Some(severity),
                    code: None,
                    code_description: None,
                    source: Some("gb".to_string()),
                    message: diagnostic.message(global_state.interner()),
                    related_information: None,
                    tags: None,
                    data: None,
                }
            })
            .collect();

        connection.notify::<PublishDiagnostics>(PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics,
            version: None,
        })?;
    }

    Ok(())
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

fn convert_text_range(range: TextRange, line_index: &LineIndex) -> Range {
    Range {
        start: convert_text_size(range.start(), line_index),
        end: convert_text_size(range.end(), line_index),
    }
}

fn convert_text_size(offset: TextSize, line_index: &LineIndex) -> Position {
    let (LineNr(line), ColNr(character)) = line_index.line_col(offset);
    Position { line, character }
}
