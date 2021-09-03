mod marker;

use self::marker::Marker;
use crate::event::Event;
use token::{Token, TokenKind};

pub(crate) struct Parser<'tokens, 'input> {
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    events: Vec<Event>,
}

impl<'tokens, 'input> Parser<'tokens, 'input> {
    pub(crate) fn new(tokens: &'tokens [Token<'input>]) -> Self {
        Self { tokens, token_idx: 0, events: Vec::new() }
    }

    pub(crate) fn parse(mut self, grammar: impl Fn(&mut Self)) -> Vec<Event> {
        grammar(&mut self);
        self.events
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.skip_whitespace();
        self.at_raw(kind)
    }

    pub(crate) fn bump(&mut self) {
        self.events.push(Event::AddToken);
        self.token_idx += 1;
    }

    fn skip_whitespace(&mut self) {
        if self.at_raw(TokenKind::Whitespace) {
            self.token_idx += 1;
        }
    }

    fn at_raw(&self, kind: TokenKind) -> bool {
        self.tokens.get(self.token_idx).map_or(false, |token| token.kind == kind)
    }
}
