use ide::{HighlightKind, HighlightModifier};
use lsp_types::{
    OneOf, SelectionRangeProviderCapability, SemanticTokenModifier, SemanticTokenType,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, WorkDoneProgressOptions,
};

pub fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::INCREMENTAL),
            will_save: None,
            will_save_wait_until: None,
            save: None,
        })),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: token_types(),
                    token_modifiers: token_modifiers(),
                },
                full: Some(SemanticTokensFullOptions::Delta { delta: None }),
                range: None,
                work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
            },
        )),
        ..Default::default()
    }
}

fn token_types() -> Vec<SemanticTokenType> {
    HighlightKind::all()
        .into_iter()
        .map(|kind| match kind {
            HighlightKind::Keyword => SemanticTokenType::KEYWORD,
            HighlightKind::Local => SemanticTokenType::VARIABLE,
            HighlightKind::Param => SemanticTokenType::PARAMETER,
            HighlightKind::Function => SemanticTokenType::FUNCTION,
            HighlightKind::Module => SemanticTokenType::NAMESPACE,
            HighlightKind::Ty => SemanticTokenType::new("builtinType"),
            HighlightKind::Number => SemanticTokenType::NUMBER,
            HighlightKind::String => SemanticTokenType::STRING,
            HighlightKind::Operator => SemanticTokenType::OPERATOR,
            HighlightKind::Comment => SemanticTokenType::COMMENT,
            HighlightKind::__Last => unreachable!(),
        })
        .collect()
}

fn token_modifiers() -> Vec<SemanticTokenModifier> {
    HighlightModifier::all()
        .into_iter()
        .map(|modifier| match modifier {
            HighlightModifier::Declaration => SemanticTokenModifier::DECLARATION,
            HighlightModifier::__Last => unreachable!(),
        })
        .collect()
}
