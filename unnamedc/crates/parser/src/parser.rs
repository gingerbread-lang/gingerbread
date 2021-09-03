mod marker;

pub(crate) use self::marker::{CompletedMarker, Marker};
use crate::error::ParseError;
use crate::event::Event;
use syntax::SyntaxKind;
use token::{Token, TokenKind};

pub(crate) struct Parser<'tokens, 'input> {
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    events: Vec<Event>,
    expected_kinds: Vec<TokenKind>,
}

impl<'tokens, 'input> Parser<'tokens, 'input> {
    pub(crate) fn new(tokens: &'tokens [Token<'input>]) -> Self {
        Self { tokens, token_idx: 0, events: Vec::new(), expected_kinds: Vec::new() }
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
        let current_token = self.current_token();

        self.events.push(Event::Error(ParseError {
            expected: self.expected_kinds.clone(),
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

        if self.at_eof() {
            return None;
        }

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
        self.expected_kinds.push(kind);

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
