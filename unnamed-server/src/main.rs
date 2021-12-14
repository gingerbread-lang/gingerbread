use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::request::{
    GotoDefinition, Request, SemanticTokensFullRequest,
    SemanticTokensRefesh as SemanticTokensRefresh, Shutdown,
};
use lsp_types::{
    GotoDefinitionResponse, InitializeResult, Location, OneOf, Position, Range, SemanticToken,
    SemanticTokenType, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkDoneProgressOptions,
};
use token::TokenKind;

mod lsp;

fn main() -> anyhow::Result<()> {
    let stdio_connection_storage = lsp::connection::ConnectionStorage::new();

    let capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::INCREMENTAL),
            will_save: None,
            will_save_wait_until: None,
            save: None,
        })),
        definition_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![
                        SemanticTokenType::KEYWORD,
                        SemanticTokenType::VARIABLE,
                        SemanticTokenType::NUMBER,
                        SemanticTokenType::STRING,
                        SemanticTokenType::OPERATOR,
                        SemanticTokenType::COMMENT,
                    ],
                    token_modifiers: Vec::new(),
                },

                full: Some(SemanticTokensFullOptions::Delta { delta: None }),
                range: None,
                work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
            },
        )),
        ..Default::default()
    };

    let connection = lsp::connection::Connection::new(&stdio_connection_storage, |_| {
        InitializeResult { capabilities, server_info: None }
    })?;

    let mut connection = match connection {
        Some(con) => con,
        None => return Ok(()),
    };

    let mut content = String::new();

    loop {
        match connection.read_msg()? {
            lsp::model::Msg::Req(req) => {
                eprintln!(
                    "\n== REQUEST ==\nid: {:?}\nmethod: {}\n{:#}\n",
                    req.id, req.method, req.params
                );

                let mut shutdown = false;

                connection
                    .req_handler(req)
                    .on::<Shutdown, _>(|()| {
                        shutdown = true;
                        Ok(())
                    })?
                    .on::<GotoDefinition, _>(|params| {
                        Ok(Some(GotoDefinitionResponse::Array(vec![Location {
                            uri: params.text_document_position_params.text_document.uri,
                            range: Range { start: Position::new(0, 0), end: Position::new(0, 2) },
                        }])))
                    })?
                    .on::<SemanticTokensFullRequest, _>(|_params| {
                        let content: &str = &content;
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
                                .find(|(_, line_start)| {
                                    **line_start <= u32::from(token.range.start())
                                })
                                .unwrap();

                            let line_number = line_number as u32;

                            let column_number = u32::from(token.range.start()) - line_start;

                            let pos = match prev_token_position {
                                Some((prev_line_number, prev_column_number)) => {
                                    let on_same_line_as_prev_token =
                                        prev_line_number == line_number;

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

                        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                            result_id: None,
                            data,
                        })))
                    })?
                    .finish()?;

                if shutdown {
                    return Ok(());
                }
            }

            lsp::model::Msg::Res(res) => eprintln!(
                "\n== RESPONSE ==\nid: {:?}\nresult: {}\nerror: {:?}\n",
                res.id, res.result, res.error
            ),

            lsp::model::Msg::Not(not) => {
                eprintln!("\n== NOTIFICATION ==\nmethod: {}\n{:#}\n", not.method, not.params);

                connection
                    .not_handler(not)
                    .on::<DidOpenTextDocument, _>(|params| {
                        content = params.text_document.text;
                        Ok(())
                    })?
                    .on::<DidChangeTextDocument, _>(|params| {
                        for change in params.content_changes {
                            let range = match change.range {
                                Some(range) => range,
                                None => {
                                    // if no range is present, the text is considered
                                    // the new full content of the document.
                                    content = change.text;
                                    continue;
                                }
                            };

                            let line_starts: Vec<_> = std::iter::once(0)
                                .chain(content.match_indices('\n').map(|(idx, _)| idx + 1))
                                .collect();

                            let start_idx = line_starts[range.start.line as usize]
                                + range.start.character as usize;

                            let end_idx =
                                line_starts[range.end.line as usize] + range.end.character as usize;

                            content.replace_range(start_idx..end_idx, &change.text);
                        }

                        connection.write_msg(&lsp::model::Msg::Req(lsp::model::Req {
                            id: connection.next_id(),
                            method: SemanticTokensRefresh::METHOD.to_string(),
                            params: serde_json::Value::Null,
                        }))
                    })?;
            }
        }
    }
}
