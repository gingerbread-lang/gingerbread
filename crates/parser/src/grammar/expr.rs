use crate::grammar::def::DEF_FIRST;
use crate::grammar::statement::parse_statement;
use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::{NodeKind, TokenKind};

pub(super) const EXPR_FIRST: TokenSet =
    TokenSet::new([TokenKind::Ident, TokenKind::LBrace, TokenKind::Int, TokenKind::Quote]);

pub(super) fn parse_expr(
    p: &mut Parser<'_>,
    expected_syntax_name: &'static str,
) -> Option<CompletedMarker> {
    parse_expr_with_recovery_set(p, TokenSet::default(), expected_syntax_name)
}

fn parse_expr_with_recovery_set(
    p: &mut Parser<'_>,
    recovery_set: TokenSet,
    expected_syntax_name: &'static str,
) -> Option<CompletedMarker> {
    parse_expr_bp(p, 0, recovery_set, expected_syntax_name)
}

fn parse_expr_bp(
    p: &mut Parser<'_>,
    min_bp: u8,
    recovery_set: TokenSet,
    expected_syntax_name: &'static str,
) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p, recovery_set, expected_syntax_name)?;

    loop {
        let (left_bp, right_bp) = if p.at(TokenKind::Plus) || p.at(TokenKind::Hyphen) {
            (1, 2)
        } else if p.at(TokenKind::Asterisk) || p.at(TokenKind::Slash) {
            (3, 4)
        } else {
            break;
        };

        if left_bp < min_bp {
            break;
        }

        p.bump();

        let m = lhs.precede(p);
        parse_expr_bp(p, right_bp, recovery_set, "operand");
        lhs = m.complete(p, NodeKind::BinaryExpr);
    }

    Some(lhs)
}

fn parse_lhs(
    p: &mut Parser<'_>,
    recovery_set: TokenSet,
    expected_syntax_name: &'static str,
) -> Option<CompletedMarker> {
    let _guard = p.expected_syntax_name(expected_syntax_name);

    let completed_marker = if p.at(TokenKind::Ident) {
        parse_call(p)
    } else if p.at(TokenKind::LBrace) {
        parse_block(p)
    } else if p.at(TokenKind::Int) {
        parse_int_literal(p)
    } else if p.at(TokenKind::Quote) {
        parse_string_literal(p)
    } else {
        return p.error_with_recovery_set(recovery_set);
    };

    Some(completed_marker)
}

fn parse_call(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));
    let m = p.start();
    p.bump();

    if p.at(TokenKind::Dot) {
        p.bump();
        p.expect_with_no_skip(TokenKind::Ident);
    }

    if p.at_set(EXPR_FIRST) {
        parse_arg_list(p);
    }

    m.complete(p, NodeKind::Call)
}

fn parse_arg_list(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at_set(EXPR_FIRST));
    let m = p.start();

    while p.at_set(EXPR_FIRST) {
        parse_arg(p);

        if p.at(TokenKind::Comma) {
            p.bump();
        } else {
            break;
        }
    }

    m.complete(p, NodeKind::ArgList)
}

fn parse_arg(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at_set(EXPR_FIRST));
    let m = p.start();

    parse_expr(p, "expression");

    m.complete(p, NodeKind::Arg)
}

fn parse_block(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(TokenKind::LBrace));
    let m = p.start();
    p.bump();

    while !p.at(TokenKind::RBrace) && !p.at_set(DEF_FIRST) && !p.at_eof() {
        parse_statement(p);
    }

    p.expect(TokenKind::RBrace);

    m.complete(p, NodeKind::Block)
}

fn parse_int_literal(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(TokenKind::Int));
    let m = p.start();
    p.bump();
    m.complete(p, NodeKind::IntLiteral)
}

fn parse_string_literal(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(TokenKind::Quote));
    let m = p.start();
    p.bump();

    while p.at(TokenKind::StringContents) || p.at(TokenKind::Escape) {
        p.bump();
    }

    p.expect(TokenKind::Quote);
    m.complete(p, NodeKind::StringLiteral)
}
