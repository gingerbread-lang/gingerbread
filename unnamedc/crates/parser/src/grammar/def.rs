mod fnc_def;

use self::fnc_def::parse_fnc_def;
use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use token::TokenKind;

pub(super) const DEF_FIRST: TokenSet = TokenSet::new([TokenKind::FncKw]);

pub(super) fn parse_def(p: &mut Parser<'_, '_>) -> Option<CompletedMarker> {
    let _guard = p.expected_syntax_name("definition");

    if p.at(TokenKind::FncKw) {
        return Some(parse_fnc_def(p));
    }

    p.error_with_recovery_set_no_default(TokenSet::default())
}
