use crate::grammar::expr::{parse_expr, EXPR_FIRST};
use crate::grammar::ty::{parse_ty, parse_ty_with_recovery_set};
use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::SyntaxKind;
use token::TokenKind;

pub(super) fn parse_fnc_def(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::FncKw));
    let m = p.start();
    p.bump();

    {
        let _guard = p.expected_syntax_name("function name");
        p.expect_with_recovery_set(TokenKind::Ident, TokenSet::new([TokenKind::LParen]));
    }

    if p.at(TokenKind::LParen) {
        parse_fnc_params(p);
    } else {
        let _guard = p.expected_syntax_name("function parameters");

        // never eat the token that could not start function parameters
        p.error_with_recovery_set_no_default(TokenSet::ALL);
    }

    if p.at(TokenKind::Colon) {
        parse_ret_ty(p);
    }

    p.expect_with_recovery_set(TokenKind::Arrow, EXPR_FIRST);
    parse_expr(p, "function body");

    m.complete(p, SyntaxKind::FncDef)
}

fn parse_fnc_params(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));
    let m = p.start();
    p.bump();

    loop {
        if should_stop_fnc_params(p) {
            break;
        }

        parse_fnc_param(p);

        if should_stop_fnc_params(p) {
            break;
        }

        p.expect(TokenKind::Comma);
    }

    p.expect_with_recovery_set(TokenKind::RParen, TokenSet::new([TokenKind::Arrow]));

    m.complete(p, SyntaxKind::Params)
}

fn should_stop_fnc_params(p: &mut Parser<'_, '_>) -> bool {
    p.at_set(TokenSet::new([TokenKind::RParen, TokenKind::Arrow]))
        || p.at_default_recovery_set()
        || p.at_eof()
}

fn parse_ret_ty(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::Colon));
    let m = p.start();
    p.bump();

    {
        let _guard = p.expected_syntax_name("return type");
        parse_ty_with_recovery_set(p, TokenSet::new([TokenKind::Arrow]).union(EXPR_FIRST));
    }

    m.complete(p, SyntaxKind::RetTy)
}

fn parse_fnc_param(p: &mut Parser<'_, '_>) -> CompletedMarker {
    let m = p.start();

    {
        let _guard = p.expected_syntax_name("parameter name");
        p.expect_with_recovery_set(TokenKind::Ident, TokenSet::new([TokenKind::Colon]));
    }
    p.expect(TokenKind::Colon);
    parse_ty(p);

    m.complete(p, SyntaxKind::Param)
}
