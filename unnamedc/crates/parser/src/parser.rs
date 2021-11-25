mod marker;

pub(crate) use self::marker::{CompletedMarker, Marker};
use crate::error::{ExpectedSyntax, ParseError, ParseErrorKind};
use crate::event::Event;
use crate::token_set::TokenSet;
use std::cell::Cell;
use std::rc::Rc;
use syntax::SyntaxKind;
use token::{Token, TokenKind};

const DEFAULT_RECOVERY_SET: TokenSet = TokenSet::new([
    TokenKind::LetKw,
    TokenKind::FncKw,
    TokenKind::LBrace,
    TokenKind::RBrace,
    TokenKind::Semicolon,
]);

#[derive(Debug)]
pub(crate) struct Parser<'tokens, 'input> {
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    events: Vec<Event>,
    expected_syntax: Option<ExpectedSyntax>,
    expected_syntax_tracking_state: Rc<Cell<ExpectedSyntaxTrackingState>>,
}

impl<'tokens, 'input> Parser<'tokens, 'input> {
    pub(crate) fn new(tokens: &'tokens [Token<'input>]) -> Self {
        Self {
            tokens,
            token_idx: 0,
            events: Vec::new(),
            expected_syntax: None,
            expected_syntax_tracking_state: Rc::new(Cell::new(
                ExpectedSyntaxTrackingState::Unnamed,
            )),
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

    pub(crate) fn expect_with_no_skip(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_no_skip();
        }
    }

    pub(crate) fn error_with_recovery_set(
        &mut self,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        self.error_with_recovery_set_no_default(recovery_set.union(DEFAULT_RECOVERY_SET))
    }

    pub(crate) fn error_with_no_skip(&mut self) -> Option<CompletedMarker> {
        self.error_with_recovery_set_no_default(TokenSet::ALL)
    }

    pub(crate) fn error_with_recovery_set_no_default(
        &mut self,
        recovery_set: TokenSet,
    ) -> Option<CompletedMarker> {
        // we must have been expecting something if there was an error
        let expected_syntax = self.expected_syntax.take().unwrap();
        self.expected_syntax_tracking_state.set(ExpectedSyntaxTrackingState::Unnamed);

        if self.at_eof() || self.at_set(recovery_set) {
            let range = self.previous_token().range;
            self.events.push(Event::Error(ParseError {
                expected_syntax,
                kind: ParseErrorKind::Missing { offset: range.end() },
            }));

            return None;
        }

        // we can unwrap because we would have returned if we were at EOF
        let current_token = self.current_token().unwrap();

        self.events.push(Event::Error(ParseError {
            expected_syntax,
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
    pub(crate) fn expected_syntax_name(&mut self, name: &'static str) -> ExpectedSyntaxGuard {
        self.expected_syntax_tracking_state.set(ExpectedSyntaxTrackingState::Named);
        self.expected_syntax = Some(ExpectedSyntax::Named(name));

        ExpectedSyntaxGuard::new(Rc::clone(&self.expected_syntax_tracking_state))
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        if let ExpectedSyntaxTrackingState::Unnamed = self.expected_syntax_tracking_state.get() {
            self.expected_syntax = Some(ExpectedSyntax::Unnamed(kind));
        }

        self.skip_trivia();
        self.at_raw(kind)
    }

    pub(crate) fn at_eof(&mut self) -> bool {
        self.skip_trivia();
        self.current_token().is_none()
    }

    pub(crate) fn at_default_recovery_set(&mut self) -> bool {
        self.at_set(DEFAULT_RECOVERY_SET)
    }

    pub(crate) fn at_set(&mut self, set: TokenSet) -> bool {
        self.skip_trivia();
        self.peek().map_or(false, |kind| set.contains(kind))
    }

    pub(crate) fn bump(&mut self) {
        self.clear_expected_syntaxes();
        self.events.push(Event::AddToken);
        self.token_idx += 1;
    }

    fn clear_expected_syntaxes(&mut self) {
        self.expected_syntax = None;
        self.expected_syntax_tracking_state.set(ExpectedSyntaxTrackingState::Unnamed);
    }

    fn previous_token(&mut self) -> Token<'input> {
        let mut previous_token_idx = self.token_idx - 1;
        while let Some(Token { kind: TokenKind::Whitespace | TokenKind::Comment, .. }) =
            self.tokens.get(previous_token_idx)
        {
            previous_token_idx -= 1;
        }

        self.tokens[previous_token_idx]
    }

    fn skip_trivia(&mut self) {
        while self.at_raw(TokenKind::Whitespace) || self.at_raw(TokenKind::Comment) {
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

pub(crate) struct ExpectedSyntaxGuard {
    expected_syntax_tracking_state: Rc<Cell<ExpectedSyntaxTrackingState>>,
}

impl ExpectedSyntaxGuard {
    fn new(expected_syntax_tracking_state: Rc<Cell<ExpectedSyntaxTrackingState>>) -> Self {
        Self { expected_syntax_tracking_state }
    }
}

impl Drop for ExpectedSyntaxGuard {
    fn drop(&mut self) {
        self.expected_syntax_tracking_state.set(ExpectedSyntaxTrackingState::Unnamed);
    }
}

#[derive(Debug, Clone, Copy)]
enum ExpectedSyntaxTrackingState {
    Named,
    Unnamed,
}
