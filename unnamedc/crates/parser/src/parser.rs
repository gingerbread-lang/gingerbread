mod marker;

pub(crate) use self::marker::{CompletedMarker, Marker};
use crate::error::ParseError;
use crate::event::Event;
use std::collections::BTreeSet;
use std::mem;
use syntax::SyntaxKind;
use token::{Token, TokenKind};

const RECOVERY_SET: [TokenKind; 1] = [TokenKind::LetKw];

pub(crate) struct Parser<'tokens, 'input> {
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    events: Vec<Event>,
    expected_kinds: BTreeSet<TokenKind>,
}

impl<'tokens, 'input> Parser<'tokens, 'input> {
    pub(crate) fn new(tokens: &'tokens [Token<'input>]) -> Self {
        Self { tokens, token_idx: 0, events: Vec::new(), expected_kinds: BTreeSet::new() }
    }

    pub(crate) fn parse(mut self, grammar: impl Fn(&mut Self)) -> Vec<Event> {
        grammar(&mut self);
        self.events
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
        }
    }

    pub(crate) fn error(&mut self) -> Option<CompletedMarker> {
        let expected = mem::take(&mut self.expected_kinds);

        // when we’re at EOF or at a token we shouldn’t skip,
        // we use the *previous* non-whitespace token’s range
        // and don’t create an error node

        if self.at_eof() || self.at_set(RECOVERY_SET) {
            let range = self.previous_token().range;
            self.events.push(Event::Error(ParseError { expected, found: None, range }));

            return None;
        }

        let current_token = self.current_token();

        self.events.push(Event::Error(ParseError {
            expected,
            found: current_token.map(|token| token.kind),
            range: {
                // we use the current token’s range
                //
                // if we’re at the end of the input,
                // we use the last non-whitespace token’s range
                //
                // if the input is empty, we panic,
                // since parsing an empty input should always succeed
                current_token
                    .map(|token| token.range)
                    .or_else(|| {
                        self.tokens
                            .iter()
                            .rfind(|token| token.kind != TokenKind::Whitespace)
                            .map(|last_token| last_token.range)
                    })
                    .unwrap()
            },
        }));

        let m = self.start();
        self.bump();
        Some(m.complete(self, SyntaxKind::Error))
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.insert(kind);

        self.skip_whitespace();
        self.at_raw(kind)
    }

    pub(crate) fn at_eof(&mut self) -> bool {
        self.skip_whitespace();
        self.current_token().is_none()
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.events.push(Event::AddToken);
        self.token_idx += 1;
    }

    fn at_set<const SET_LEN: usize>(&self, set: [TokenKind; SET_LEN]) -> bool {
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
