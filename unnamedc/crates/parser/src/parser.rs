mod expected_name_guard;
mod marker;

use self::expected_name_guard::ExpectedNameGuard;
pub(crate) use self::marker::{CompletedMarker, Marker};
use crate::error::{ExpectedGroup, ParseError};
use crate::event::Event;
use std::cell::Cell;
use std::collections::BTreeSet;
use std::mem;
use std::rc::Rc;
use syntax::SyntaxKind;
use token::{Token, TokenKind};

const DEFAULT_RECOVERY_SET: [TokenKind; 1] = [TokenKind::LetKw];

#[derive(Debug)]
pub(crate) struct Parser<'tokens, 'input> {
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    events: Vec<Event>,
    expected_groups: Vec<ExpectedGroup>,
    current_expected_group: Rc<Cell<Option<usize>>>,
}

impl<'tokens, 'input> Parser<'tokens, 'input> {
    pub(crate) fn new(tokens: &'tokens [Token<'input>]) -> Self {
        Self {
            tokens,
            token_idx: 0,
            events: Vec::new(),
            expected_groups: Vec::new(),
            current_expected_group: Rc::new(Cell::new(None)),
        }
    }

    pub(crate) fn parse(mut self, grammar: impl Fn(&mut Self)) -> Vec<Event> {
        grammar(&mut self);
        self.events
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        self.expect_with_recovery_set(kind, DEFAULT_RECOVERY_SET)
    }

    pub(crate) fn expect_with_recovery_set<const LEN: usize>(
        &mut self,
        kind: TokenKind,
        recovery_set: [TokenKind; LEN],
    ) {
        if self.at(kind) {
            self.bump();
        } else {
            let mut recovery_set_with_defaults = DEFAULT_RECOVERY_SET.to_vec();
            recovery_set_with_defaults.extend_from_slice(&recovery_set);
            self.error_with_recovery_set(&recovery_set_with_defaults);
        }
    }

    pub(crate) fn error(&mut self) -> Option<CompletedMarker> {
        self.error_with_recovery_set(&DEFAULT_RECOVERY_SET)
    }

    #[must_use]
    pub(crate) fn expected_name(&mut self, name: &'static str) -> ExpectedNameGuard {
        self.current_expected_group.set(Some(self.expected_groups.len()));
        self.expected_groups.push(ExpectedGroup { name: Some(name), kinds: BTreeSet::new() });

        ExpectedNameGuard::new(Rc::clone(&self.current_expected_group))
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        let group = self.ensure_expected_group_is_present();
        group.kinds.insert(kind);

        self.skip_whitespace();
        self.at_raw(kind)
    }

    pub(crate) fn at_eof(&mut self) -> bool {
        self.skip_whitespace();
        self.current_token().is_none()
    }

    pub(crate) fn bump(&mut self) {
        self.clear_expected_groups();
        self.events.push(Event::AddToken);
        self.token_idx += 1;
    }

    fn error_with_recovery_set(&mut self, recovery_set: &[TokenKind]) -> Option<CompletedMarker> {
        let expected_groups = mem::take(&mut self.expected_groups);
        self.current_expected_group.set(None);

        // when we’re at EOF or at a token we shouldn’t skip,
        // we use the *previous* non-whitespace token’s range
        // and don’t create an error node

        if self.at_eof() || self.at_set(recovery_set) {
            let range = self.previous_token().range;
            self.events.push(Event::Error(ParseError::new(expected_groups, None, range)));

            return None;
        }

        let current_token = self.current_token();

        // we use the current token’s range
        //
        // if we’re at the end of the input,
        // we use the last non-whitespace token’s range
        //
        // if the input is empty, we panic,
        // since parsing an empty input should always succeed
        let range = current_token
            .map(|token| token.range)
            .or_else(|| {
                self.tokens
                    .iter()
                    .rfind(|token| token.kind != TokenKind::Whitespace)
                    .map(|last_token| last_token.range)
            })
            .unwrap();

        self.events.push(Event::Error(ParseError::new(
            expected_groups,
            current_token.map(|token| token.kind),
            range,
        )));

        let m = self.start();
        self.bump();
        Some(m.complete(self, SyntaxKind::Error))
    }

    fn clear_expected_groups(&mut self) {
        self.expected_groups.clear();
        self.current_expected_group.set(None);
    }

    fn ensure_expected_group_is_present(&mut self) -> &mut ExpectedGroup {
        let idx = if let Some(idx) = self.current_expected_group.get() {
            idx
        } else {
            let idx = self.expected_groups.len();
            self.current_expected_group.set(Some(idx));
            self.expected_groups.push(ExpectedGroup { name: None, kinds: BTreeSet::new() });

            idx
        };

        &mut self.expected_groups[idx]
    }

    fn at_set(&self, set: &[TokenKind]) -> bool {
        self.peek().map_or(false, |kind| set.contains(&kind))
    }

    fn previous_token(&mut self) -> Token<'input> {
        let mut previous_token_idx = self.token_idx - 1;
        while let Some(Token { kind: TokenKind::Whitespace, .. }) =
            self.tokens.get(previous_token_idx)
        {
            previous_token_idx -= 1;
        }

        self.tokens[previous_token_idx]
    }

    fn skip_whitespace(&mut self) {
        if self.at_raw(TokenKind::Whitespace) {
            self.token_idx += 1;
        }
    }

    fn at_raw(&self, kind: TokenKind) -> bool {
        self.peek().map_or(false, |k| k == kind)
    }

    fn peek(&self) -> Option<TokenKind> {
        self.current_token().map(|token| token.kind)
    }

    fn current_token(&self) -> Option<Token<'input>> {
        self.tokens.get(self.token_idx).copied()
    }
}
