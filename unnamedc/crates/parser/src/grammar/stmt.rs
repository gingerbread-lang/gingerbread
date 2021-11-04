use crate::grammar::expr::parse_expr;
use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::SyntaxKind;
use token::TokenKind;

pub(super) fn parse_stmt(p: &mut Parser<'_, '_>) -> Option<CompletedMarker> {
    let _guard = p.expected_syntax_name("statement");

    if p.at(TokenKind::Semicolon) {
        return p.error_with_recovery_set_no_default(TokenSet::default());
    }

    if p.at(TokenKind::LetKw) {
        return Some(parse_local_def(p));
    }

    parse_expr(p, "statement")
}

fn parse_local_def(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::LetKw));
    let m = p.start();
    p.bump();

    {
        let _guard = p.expected_syntax_name("variable name");
        p.expect_with_recovery_set(TokenKind::Ident, TokenSet::new([TokenKind::Eq]));
    }

    p.expect(TokenKind::Eq);
    parse_expr(p, "variable value");

    p.expect(TokenKind::Semicolon);

    m.complete(p, SyntaxKind::LocalDef)
}
