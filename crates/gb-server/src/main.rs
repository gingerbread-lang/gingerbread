use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::request::{SelectionRangeRequest, SemanticTokensFullRequest, Shutdown};
use lsp_types::InitializeResult;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() -> anyhow::Result<()> {
    let stdio_connection_storage = lsp::connection::ConnectionStorage::new();

    let connection = lsp::connection::Connection::new(&stdio_connection_storage, |_| {
        InitializeResult { capabilities: gb_server::capabilities(), server_info: None }
    })?;

    let mut connection = match connection {
        Some(con) => con,
        None => return Ok(()),
    };

    let mut global_state = ide::GlobalState::default();

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
                        Ok(Some(gb_server::selection_range(params, &mut global_state)))
                    })?
                    .on::<SemanticTokensFullRequest, _>(|params| {
                        Ok(Some(gb_server::semantic_tokens(params, &mut global_state)))
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
                        gb_server::open_text_document(params, &mut global_state, &mut connection)
                    })?
                    .on::<DidChangeTextDocument, _>(|params| {
                        gb_server::change_text_document(params, &mut global_state, &mut connection)
                    })?;
            }
        }
    }
}
