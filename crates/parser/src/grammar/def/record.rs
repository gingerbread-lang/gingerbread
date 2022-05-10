use crate::grammar::ty::parse_ty;
use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::{NodeKind, TokenKind};

pub(super) fn parse_record(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(TokenKind::RecKw));
    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident);
    p.expect(TokenKind::LBrace);

    while p.at(TokenKind::Ident) {
        let m = p.start();
        p.bump();
        p.expect(TokenKind::Colon);
        parse_ty(p, TokenSet::default());
        m.complete(p, NodeKind::Field);

        if !p.at(TokenKind::RBrace) {
            p.expect_with_recovery_set(TokenKind::Comma, TokenSet::new([TokenKind::Ident]));
        }
    }

    p.expect(TokenKind::RBrace);
    p.expect(TokenKind::Semicolon);

    m.complete(p, NodeKind::Record)
}
