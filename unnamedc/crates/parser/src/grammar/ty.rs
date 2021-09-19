use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::SyntaxKind;
use token::TokenKind;

pub(super) fn parse_ty(p: &mut Parser<'_, '_>) -> CompletedMarker {
    parse_ty_with_recovery_set(p, TokenSet::default())
}

pub(super) fn parse_ty_with_recovery_set(
    p: &mut Parser<'_, '_>,
    recovery_set: TokenSet,
) -> CompletedMarker {
    let m = p.start();
    p.expect_with_recovery_set(TokenKind::Ident, recovery_set);
    m.complete(p, SyntaxKind::Ty)
}
