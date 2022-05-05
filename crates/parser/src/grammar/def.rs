mod function;

use self::function::parse_function;
use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::{NodeKind, TokenKind};

pub(super) const DEF_FIRST: TokenSet =
    TokenSet::new([TokenKind::FncKw, TokenKind::DocCommentLeader]);

pub(super) fn parse_def(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let docs_cm = if p.at(TokenKind::DocCommentLeader) { Some(parse_docs(p)) } else { None };

    let _guard = p.expected_syntax_name("definition");

    if p.at(TokenKind::FncKw) {
        let m = match docs_cm {
            Some(cm) => cm.precede(p),
            None => p.start(),
        };
        return Some(parse_function(p, m));
    }

    p.error_with_recovery_set_no_default(TokenSet::default())
}

fn parse_docs(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(TokenKind::DocCommentLeader));
    let m = p.start();

    while p.at(TokenKind::DocCommentLeader) {
        let m = p.start();
        p.bump();
        if p.at(TokenKind::DocCommentContents) {
            p.bump();
        }
        m.complete(p, NodeKind::DocComment);
    }

    m.complete(p, NodeKind::Docs)
}
