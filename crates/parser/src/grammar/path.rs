use crate::parser::{CompletedMarker, Parser};
use syntax::{NodeKind, TokenKind};

pub(super) fn parse_path(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));
    let m = p.start();
    p.bump();

    if p.at(TokenKind::Dot) {
        p.bump();
        p.expect_with_no_skip(TokenKind::Ident);
    }

    m.complete(p, NodeKind::Path)
}
