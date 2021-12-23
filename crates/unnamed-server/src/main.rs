use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument, PublishDiagnostics};
use lsp_types::request::{
    GotoDefinition, SemanticTokensFullRequest, SemanticTokensRefesh as SemanticTokensRefresh,
    Shutdown,
};
use lsp_types::{
    GotoDefinitionResponse, InitializeResult, Location, OneOf, Position, PublishDiagnosticsParams,
    Range, SemanticTokenType, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkDoneProgressOptions,
};
use unnamed_server::GlobalState;

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

    let mut global_state = GlobalState::default();

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
                    .on::<SemanticTokensFullRequest, _>(|params| {
                        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                            result_id: None,
                            data: global_state.highlight(&params.text_document.uri),
                        })))
                    })?
                    .finish()?;

                if shutdown {
                    return Ok(());
                }
            }

            // this is a comment
            lsp::model::Msg::Res(res) => eprintln!(
                "\n== RESPONSE ==\nid: {:?}\nresult: {}\nerror: {:?}\n",
                res.id, res.result, res.error
            ),

            lsp::model::Msg::Not(not) => {
                eprintln!("\n== NOTIFICATION ==\nmethod: {}\n{:#}\n", not.method, not.params);

                connection
                    .not_handler(not)
                    .on::<DidOpenTextDocument, _>(|params| {
                        global_state.open_file(params.text_document.uri, params.text_document.text);
                        Ok(())
                    })?
                    .on::<DidChangeTextDocument, _>(|params| {
                        global_state
                            .apply_changes(&params.text_document.uri, params.content_changes);

                        let diagnostics = global_state.diagnostics(&params.text_document.uri);

                        connection.notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                            uri: params.text_document.uri,
                            diagnostics,
                            version: None,
                        })?;

                        connection.make_request::<SemanticTokensRefresh>(())?;

                        Ok(())
                    })?;
            }
        }
    }
}
