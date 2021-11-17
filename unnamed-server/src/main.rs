use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::request::{
    GotoDefinition, SemanticTokensFullRequest, SemanticTokensRefesh as SemanticTokensRefresh,
};
use lsp_types::{
    GotoDefinitionResponse, InitializeParams, Location, OneOf, Position, Range, SemanticToken,
    SemanticTokenType, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkDoneProgressOptions,
};
use token::TokenKind;

fn main() -> anyhow::Result<()> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
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
    })
    .unwrap();

    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    Ok(())
}

fn main_loop(connection: Connection, params: serde_json::Value) -> anyhow::Result<()> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();

    let mut content = String::new();
    let mut id = 0;

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                id = req.id.to_string().parse().unwrap();
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                eprintln!(
                    "\n== REQUEST ==\nid: {:?}\nmethod: {}\n{:#}\n",
                    req.id, req.method, req.params
                );

                match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        let result = GotoDefinitionResponse::Array(vec![Location {
                            uri: params.text_document_position_params.text_document.uri,
                            range: Range { start: Position::new(0, 0), end: Position::new(0, 2) },
                        }]);
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response { id, result: Some(result), error: None };
                        connection.sender.send(Message::Response(resp))?;
                    }
                    Err(req) => {
                        if let Ok((id, _params)) = cast::<SemanticTokensFullRequest>(req) {
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

                            let result = SemanticTokens { result_id: None, data };
                            let result = serde_json::to_value(&result).unwrap();
                            let resp = Response { id, result: Some(result), error: None };
                            connection.sender.send(Message::Response(resp))?;
                        }
                    }
                }
            }

            Message::Response(resp) => eprintln!(
                "\n== RESPONSE ==\nid: {:?}\nresult: {}\nerror: {:?}\n",
                resp.id,
                resp.result.unwrap_or(serde_json::Value::Null),
                resp.error
            ),

            Message::Notification(not) => {
                eprintln!("\n== NOTIFICATION ==\nmethod: {}\n{:#}\n", not.method, not.params);

                match cast_not::<DidOpenTextDocument>(not) {
                    Ok(params) => {
                        content = params.text_document.text;
                    }
                    Err(not) => {
                        if let Ok(params) = cast_not::<DidChangeTextDocument>(not) {
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

                                let end_idx = line_starts[range.end.line as usize]
                                    + range.end.character as usize;

                                content.replace_range(start_idx..end_idx, &change.text);
                            }

                            connection.sender.send(Message::Request(Request {
                                id: RequestId::from(id + 1),
                                method:
                                    <SemanticTokensRefresh as lsp_types::request::Request>::METHOD
                                        .to_string(),
                                params: serde_json::Value::Null,
                            }))?;
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_not<N>(not: Notification) -> Result<N::Params, Notification>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}
