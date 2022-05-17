mod def;
mod expr;
mod path;
mod statement;
mod ty;

use self::def::{parse_def, DEF_FIRST};
use self::statement::parse_statement;
use crate::parser::Parser;
use crate::token_set::TokenSet;
use syntax::{NodeKind, TokenKind};

pub(crate) fn source_file(p: &mut Parser<'_>) {
    let m = p.start();

    while !p.at_eof() {
        self::def::parse_def(p);
    }

    m.complete(p, NodeKind::Root);
}

pub(crate) fn repl_line(p: &mut Parser<'_>) {
    let m = p.start();

    while !p.at_eof() {
        if p.at_set(DEF_FIRST) {
            parse_def(p);
        } else if p.at(TokenKind::RBrace) || p.at(TokenKind::Semicolon) {
            let _guard = p.expected_syntax_name("definition or statement");
            p.error_with_recovery_set_no_default(TokenSet::default());
        } else {
            parse_statement(p);
        }
    }

    m.complete(p, NodeKind::Root);
}
