use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::{NodeKind, TokenKind};

pub(super) fn parse_path(p: &mut Parser<'_>, recovery_set: TokenSet) -> CompletedMarker {
    let m = p.start();
    p.expect_with_recovery_set(TokenKind::Ident, recovery_set);

    if p.at(TokenKind::Dot) {
        p.bump();
        p.expect_with_no_skip(TokenKind::Ident);
    }

    m.complete(p, NodeKind::Path)
}
