mod def;
mod expr;
mod stmt;
mod ty;

use self::def::{parse_def, DEF_FIRST};
use self::stmt::parse_stmt;
use crate::parser::Parser;
use crate::token_set::TokenSet;
use syntax::SyntaxKind;
use token::TokenKind;

pub(crate) fn source_file(p: &mut Parser<'_, '_>) {
    let m = p.start();

    loop {
        if p.at_eof() {
            break;
        }

        self::def::parse_def(p);
    }

    m.complete(p, SyntaxKind::Root);
}

pub(crate) fn repl_line(p: &mut Parser<'_, '_>) {
    let m = p.start();

    loop {
        if p.at_eof() {
            break;
        }

        if p.at_set(DEF_FIRST) {
            parse_def(p);
        } else if p.at(TokenKind::RBrace) || p.at(TokenKind::Semicolon) {
            let _guard = p.expected_syntax_name("definition or statement");
            p.error_with_recovery_set_no_default(TokenSet::default());
        } else {
            parse_stmt(p);
        }
    }

    m.complete(p, SyntaxKind::Root);
}
