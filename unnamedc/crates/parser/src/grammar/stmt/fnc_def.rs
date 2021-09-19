use crate::grammar::expr::{parse_expr, EXPR_FIRST};
use crate::grammar::ty::parse_ty;
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

    parse_fnc_params(p);

    if p.at(TokenKind::Colon) {
        parse_ret_ty(p);
    }

    p.expect_with_recovery_set(TokenKind::Arrow, EXPR_FIRST);
    parse_expr(p, "function body");

    m.complete(p, SyntaxKind::FncDef)
}

fn parse_fnc_params(p: &mut Parser<'_, '_>) -> CompletedMarker {
    let m = p.start();

    if p.at(TokenKind::Arrow) || p.at_eof() {
        let _guard = p.expected_syntax_name("function parameters");
        p.expect_with_recovery_set(TokenKind::LParen, TokenSet::new([TokenKind::Arrow]));
        return m.complete(p, SyntaxKind::Params);
    }

    p.expect(TokenKind::LParen);

    let mut iters = 0;
    loop {
        dbg!(&p);

        if should_stop_fnc_params(p) {
            break;
        }

        parse_fnc_param(p);

        if should_stop_fnc_params(p) {
            break;
        }

        p.expect(TokenKind::Comma);

        iters += 1;

        if iters > 2 {
            panic!()
        }
    }
    p.expect(TokenKind::RParen);

    m.complete(p, SyntaxKind::Params)
}

fn should_stop_fnc_params(p: &mut Parser<'_, '_>) -> bool {
    p.at_set(TokenSet::new([TokenKind::RParen, TokenKind::Arrow]))
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

    {
        let _guard = p.expected_syntax_name("parameter name");
        p.expect_with_recovery_set(TokenKind::Ident, TokenSet::new([TokenKind::Colon]));
    }
    p.expect(TokenKind::Colon);
    parse_ty(p);

    m.complete(p, SyntaxKind::Param)
}
