use crate::grammar::expr::parse_expr;
use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::SyntaxKind;
use token::TokenKind;

pub(super) fn parse_statement(p: &mut Parser<'_, '_>) -> Option<CompletedMarker> {
    let _guard = p.expected_syntax_name("statement");

    if p.at(TokenKind::Semicolon) {
        return p.error_with_recovery_set_no_default(TokenSet::default());
    }

    if p.at(TokenKind::LetKw) {
        return Some(parse_local_def(p));
    }

    let cm = parse_expr(p, "statement")?;

    if p.at(TokenKind::RBrace) || p.at_eof() {
        return Some(cm);
    }

    let m = cm.precede(p);
    p.expect_with_no_skip(TokenKind::Semicolon);

    Some(m.complete(p, SyntaxKind::ExprStatement))
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

    p.expect_with_no_skip(TokenKind::Semicolon);

    m.complete(p, SyntaxKind::LocalDef)
}
