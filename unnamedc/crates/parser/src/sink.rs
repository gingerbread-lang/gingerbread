use super::Event;
use crate::error::ParseError;
use crate::Parse;
use std::mem;
use syntax::SyntaxBuilder;
use token::{Token, TokenKind};

pub(crate) struct Sink<'tokens, 'input> {
    events: Vec<Event>,
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    builder: SyntaxBuilder,
    errors: Vec<ParseError>,
}

impl<'tokens, 'input> Sink<'tokens, 'input> {
    pub(crate) fn new(events: Vec<Event>, tokens: &'tokens [Token<'input>]) -> Self {
        Self { events, tokens, token_idx: 0, builder: SyntaxBuilder::default(), errors: Vec::new() }
    }

    pub(crate) fn finish(mut self) -> Parse {
        for event_idx in 0..self.events.len() {
            match mem::replace(&mut self.events[event_idx], Event::Placeholder) {
                Event::StartNode { kind } => self.builder.start_node(kind),
                Event::FinishNode => self.builder.finish_node(),
                Event::AddToken => self.add_token(),
                Event::Error(error) => self.errors.push(error),
                Event::Placeholder => unreachable!(),
            }

            self.skip_whitespace();
        }

        Parse { syntax_node: self.builder.finish(), errors: self.errors }
    }

    fn skip_whitespace(&mut self) {
        if let Some(Token { kind: TokenKind::Whitespace, .. }) = self.current_token() {
            self.add_token();
        }
    }

    fn add_token(&mut self) {
        let Token { kind, text, .. } = self.current_token().unwrap();
        self.builder.token(kind.into(), text);
        self.token_idx += 1;
    }

    fn current_token(&self) -> Option<Token<'input>> {
        self.tokens.get(self.token_idx).copied()
    }
}
