use interner::Interner;
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::request::{
    GotoDefinition, SelectionRangeRequest, SemanticTokensFullRequest, Shutdown, WorkspaceSymbol,
};
use lsp_types::InitializeResult;
use owo_colors::Style;
use std::env;
use std::io::Read;
use std::io::{self, Write};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() -> anyhow::Result<()> {
    match env::args().nth(1).as_deref() {
        Some("highlight") => highlight()?,
        Some("server") => server()?,
        Some(subcommand) => eprintln!("`{subcommand}` is not a valid subcommand"),
        None => eprintln!("please provide a subcommand"),
    }

    Ok(())
}

fn highlight() -> anyhow::Result<()> {
    let mut content = String::new();
    io::stdin().read_to_string(&mut content)?;

    let analysis = {
        let mut interner = Interner::default();
        let mut world_index = hir::WorldIndex::default();
        ide::Analysis::new(
            "file:///tmp.gb".parse().unwrap(),
            content.clone(),
            hir::Name(interner.intern("main")),
            &mut interner,
            &mut world_index,
        )
    };

    let mut bytes = Vec::new();
    let mut last_range = 0..0;

    for highlight in analysis.highlight() {
        let range =
            u32::from(highlight.range.start()) as usize..u32::from(highlight.range.end()) as usize;

        let unhighlighted_region = &content[last_range.end..range.start];
        bytes.extend_from_slice(unhighlighted_region.as_bytes());

        let mut style = Style::new();

        style = match highlight.kind {
            ide::HighlightKind::Keyword => style.magenta(),
            ide::HighlightKind::Local => style,
            ide::HighlightKind::Param => style,
            ide::HighlightKind::Function => style.blue(),
            ide::HighlightKind::Module => style.yellow(),
            ide::HighlightKind::Ty => style.cyan(),
            ide::HighlightKind::Number => style.green(),
            ide::HighlightKind::Quote => style.green(),
            ide::HighlightKind::Escape => style,
            ide::HighlightKind::String => style.green(),
            ide::HighlightKind::Operator => style.magenta(),
            ide::HighlightKind::CommentContents => style.bright_black(),
            ide::HighlightKind::CommentLeader => style.bright_black(),
            ide::HighlightKind::DocCommentContents => style.bright_black(),
            ide::HighlightKind::DocCommentLeader => style.bright_black(),
            ide::HighlightKind::UnresolvedReference => style.red().bold().underline(),
            ide::HighlightKind::__Last => unreachable!(),
        };

        write!(bytes, "{}", style.style(&content[highlight.range]))?;

        last_range = range;
    }
    bytes.extend_from_slice(content[last_range.end..].as_bytes());

    io::stdout().write_all(&bytes)?;

    Ok(())
}

fn server() -> anyhow::Result<()> {
    let stdio_connection_storage = lsp::connection::ConnectionStorage::new();

    let connection = lsp::connection::Connection::new(&stdio_connection_storage, |_| {
        InitializeResult { capabilities: gb::capabilities(), server_info: None }
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
                        Ok(Some(gb::selection_range(params, &mut global_state)))
                    })?
                    .on::<GotoDefinition, _>(|params| {
                        Ok(gb::goto_definition(params, &mut global_state))
                    })?
                    .on::<WorkspaceSymbol, _>(|params| {
                        Ok(Some(gb::workspace_symbol(params, &mut global_state)))
                    })?
                    .on::<SemanticTokensFullRequest, _>(|params| {
                        Ok(Some(gb::semantic_tokens(params, &mut global_state)))
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
                        gb::open_text_document(params, &mut global_state, &mut connection)
                    })?
                    .on::<DidChangeTextDocument, _>(|params| {
                        gb::change_text_document(params, &mut global_state, &mut connection)
                    })?;
            }
        }
    }
}
