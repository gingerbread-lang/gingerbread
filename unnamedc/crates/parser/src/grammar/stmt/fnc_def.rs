use crate::grammar::expr::parse_expr;
use crate::grammar::ty::parse_ty;
use crate::parser::{CompletedMarker, Parser};
use syntax::SyntaxKind;
use token::TokenKind;

pub(super) fn parse_fnc_def(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::FncKw));
    let m = p.start();
    p.bump();

    p.expect(TokenKind::Ident);

    parse_fnc_params(p);

    if p.at(TokenKind::Colon) {
        parse_ret_ty(p);
    }

    p.expect(TokenKind::Arrow);
    parse_expr(p);

    m.complete(p, SyntaxKind::FncDef)
}

fn parse_fnc_params(p: &mut Parser<'_, '_>) -> CompletedMarker {
    let m = p.start();

    p.expect(TokenKind::LParen);

    loop {
        if p.at(TokenKind::RParen) {
            break;
        }

        parse_fnc_param(p);

        if p.at(TokenKind::RParen) {
            break;
        }

        p.expect(TokenKind::Comma);
    }
    p.expect(TokenKind::RParen);

    m.complete(p, SyntaxKind::Params)
}

fn parse_ret_ty(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::Colon));
    let m = p.start();
    p.bump();
    parse_ty(p);
    m.complete(p, SyntaxKind::RetTy)
}

fn parse_fnc_param(p: &mut Parser<'_, '_>) -> CompletedMarker {
    let m = p.start();

    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Colon);
    parse_ty(p);

    m.complete(p, SyntaxKind::Param)
}
