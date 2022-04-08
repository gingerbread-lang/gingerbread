use gb_server::{GlobalState, HighlightKind};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument, PublishDiagnostics};
use lsp_types::request::{
    SelectionRangeRequest, SemanticTokensFullRequest,
    SemanticTokensRefesh as SemanticTokensRefresh, Shutdown,
};
use lsp_types::{
    InitializeResult, PublishDiagnosticsParams, SelectionRangeProviderCapability, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, WorkDoneProgressOptions,
};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

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
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: HighlightKind::all_lsp(),
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
                    .on::<SelectionRangeRequest, _>(|params| {
                        Ok(Some(
                            params
                                .positions
                                .into_iter()
                                .map(|position| {
                                    global_state
                                        .extend_selection(&params.text_document.uri, position)
                                })
                                .collect(),
                        ))
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

            lsp::model::Msg::Res(res) => eprintln!(
                "\n== RESPONSE ==\nid: {:?}\nresult: {}\nerror: {:?}\n",
                res.id, res.result, res.error
            ),

            lsp::model::Msg::Not(not) => {
                eprintln!("\n== NOTIFICATION ==\nmethod: {}\n{:#}\n", not.method, not.params);

                connection
                    .not_handler(not)
                    .on::<DidOpenTextDocument, _>(|params| {
                        global_state
                            .open_file(params.text_document.uri.clone(), params.text_document.text);

                        for (uri, diagnostics) in global_state.diagnostics() {
                            connection.notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                                uri: uri.clone(),
                                diagnostics,
                                version: None,
                            })?;
                        }

                        Ok(())
                    })?
                    .on::<DidChangeTextDocument, _>(|params| {
                        global_state
                            .apply_changes(&params.text_document.uri, params.content_changes);

                        for (uri, diagnostics) in global_state.diagnostics() {
                            connection.notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                                uri: uri.clone(),
                                diagnostics,
                                version: None,
                            })?;
                        }

                        connection.make_request::<SemanticTokensRefresh>(())?;

                        Ok(())
                    })?;
            }
        }
    }
}
