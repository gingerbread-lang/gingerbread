use crate::grammar::ty::parse_ty;
use crate::parser::{CompletedMarker, Marker, Parser};
use crate::token_set::TokenSet;
use syntax::{NodeKind, TokenKind};

pub(super) fn parse_record(p: &mut Parser<'_>, m: Marker) -> CompletedMarker {
    assert!(p.at(TokenKind::RecKw));
    p.bump();

    {
        let _guard = p.expected_syntax_name("record name");
        p.expect(TokenKind::Ident);
    }

    p.expect(TokenKind::LBrace);

    while !p.at(TokenKind::RBrace) && !p.at_eof() && !p.at_default_recovery_set() {
        let m = p.start();

        {
            let _guard = p.expected_syntax_name("field name");
            p.expect_with_recovery_set(
                TokenKind::Ident,
                TokenSet::new([TokenKind::Colon, TokenKind::Comma]),
            );
        }

        p.expect_with_recovery_set(
            TokenKind::Colon,
            TokenSet::new([TokenKind::Ident, TokenKind::Comma]),
        );

        {
            let _guard = p.expected_syntax_name("field type");
            parse_ty(p, TokenSet::new([TokenKind::Comma]));
        }

        m.complete(p, NodeKind::Field);

        if !p.at(TokenKind::RBrace) {
            p.expect_with_recovery_set(TokenKind::Comma, TokenSet::new([TokenKind::Ident]));
        }
    }

    p.expect(TokenKind::RBrace);
    p.expect(TokenKind::Semicolon);

    m.complete(p, NodeKind::Record)
}
