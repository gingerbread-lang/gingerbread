mod expected_syntax_name_guard;
mod marker;

use self::expected_syntax_name_guard::ExpectedSyntaxNameGuard;
pub(crate) use self::marker::{CompletedMarker, Marker};
use crate::error::{ExpectedSyntax, ParseError, ParseErrorKind};
use crate::event::Event;
use crate::token_set::TokenSet;
use std::cell::Cell;
use std::collections::BTreeSet;
use std::mem;
use std::rc::Rc;
use syntax::SyntaxKind;
use token::{Token, TokenKind};

const DEFAULT_RECOVERY_SET: TokenSet = TokenSet::new([TokenKind::LetKw]);

#[derive(Debug)]
pub(crate) struct Parser<'tokens, 'input> {
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    events: Vec<Event>,
    expected_syntaxes: BTreeSet<ExpectedSyntax>,
    is_named_expected_syntax_active: Rc<Cell<bool>>,
}

impl<'tokens, 'input> Parser<'tokens, 'input> {
    pub(crate) fn new(tokens: &'tokens [Token<'input>]) -> Self {
        Self {
            tokens,
            token_idx: 0,
            events: Vec::new(),
            expected_syntaxes: BTreeSet::new(),
            is_named_expected_syntax_active: Rc::new(Cell::new(false)),
        }
    }

    pub(crate) fn parse(mut self, grammar: impl Fn(&mut Self)) -> Vec<Event> {
        grammar(&mut self);
        self.events
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        self.expect_with_recovery_set(kind, TokenSet::default())
    }

    pub(crate) fn expect_with_recovery_set(&mut self, kind: TokenKind, recovery_set: TokenSet) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery_set(recovery_set);
        }
    }

    pub(crate) fn error_with_recovery_set(
        &mut self,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        let expected_syntaxes = mem::take(&mut self.expected_syntaxes);
        self.is_named_expected_syntax_active.set(false);

        if self.at_eof() || self.at_set(DEFAULT_RECOVERY_SET | recovery_set) {
            let range = self.previous_token().range;
            self.events.push(Event::Error(ParseError {
                expected_syntaxes,
                kind: ParseErrorKind::Missing { offset: range.end() },
            }));

            return None;
        }

        // we can unwrap because we would have returned if we were at EOF
        let current_token = self.current_token().unwrap();

        self.events.push(Event::Error(ParseError {
            expected_syntaxes,
            kind: ParseErrorKind::Unexpected {
                found: current_token.kind,
                range: current_token.range,
            },
        }));

        let m = self.start();
        self.bump();
        Some(m.complete(self, SyntaxKind::Error))
    }

    #[must_use]
    pub(crate) fn expected_syntax_name(&mut self, name: &'static str) -> ExpectedSyntaxNameGuard {
        self.is_named_expected_syntax_active.set(true);
        self.expected_syntaxes.insert(ExpectedSyntax::Named(name));

        ExpectedSyntaxNameGuard::new(Rc::clone(&self.is_named_expected_syntax_active))
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        if !self.is_named_expected_syntax_active.get() {
            self.expected_syntaxes.insert(ExpectedSyntax::Unnamed(kind));
        }

        self.skip_whitespace();
        self.at_raw(kind)
    }

    pub(crate) fn at_eof(&mut self) -> bool {
        self.skip_whitespace();
        self.current_token().is_none()
    }

    pub(crate) fn bump(&mut self) {
        self.clear_expected_syntaxes();
        self.events.push(Event::AddToken);
        self.token_idx += 1;
    }

    fn clear_expected_syntaxes(&mut self) {
        self.expected_syntaxes.clear();
        self.is_named_expected_syntax_active.set(false);
    }

    fn at_set(&self, set: TokenSet) -> bool {
        self.peek().map_or(false, |kind| set.contains(kind))
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
