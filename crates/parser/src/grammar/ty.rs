use super::path::parse_path;
use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::NodeKind;

pub(super) fn parse_ty(p: &mut Parser<'_>, recovery_set: TokenSet) -> CompletedMarker {
    let m = p.start();
    parse_path(p, recovery_set);
    m.complete(p, NodeKind::Ty)
}
