use syntax::SyntaxKind;
use token::TokenKind;
use crate::parser::{CompletedMarker, Parser};

pub(super) fn parse_ty(p: &mut Parser<'_, '_>) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Ident);
    m.complete(p, SyntaxKind::Ty)
}
