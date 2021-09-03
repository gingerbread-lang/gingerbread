use super::Event;
use crate::error::ParseError;
use crate::Parse;
use rowan::{GreenNodeBuilder, Language};
use std::mem;
use syntax::UnnamedLang;
use token::{Token, TokenKind};

pub(crate) struct Sink<'a> {
    events: Vec<Event>,
    tokens: Vec<Token<'a>>,
    token_idx: usize,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,
}

impl<'a> Sink<'a> {
    pub(crate) fn new(events: Vec<Event>, tokens: Vec<Token<'a>>) -> Self {
        Self { events, tokens, token_idx: 0, builder: GreenNodeBuilder::new(), errors: Vec::new() }
    }

    pub(crate) fn finish(mut self) -> Parse {
        for event_idx in 0..self.events.len() {
            match mem::replace(&mut self.events[event_idx], Event::Placeholder) {
                Event::StartNode { kind } => {
                    self.builder.start_node(UnnamedLang::kind_to_raw(kind))
                }
                Event::FinishNode => self.builder.finish_node(),
                Event::AddToken => self.add_token(),
                Event::Error(error) => self.errors.push(error),
                Event::Placeholder => unreachable!(),
            }

            self.skip_whitespace();
        }

        Parse { green_node: self.builder.finish(), errors: self.errors }
    }

    fn skip_whitespace(&mut self) {
        if let Some(Token { kind: TokenKind::Whitespace, .. }) = self.current_token() {
            self.add_token();
        }
    }

    fn add_token(&mut self) {
        let Token { kind, text, .. } = self.current_token().unwrap();
        self.builder.token(UnnamedLang::kind_to_raw(kind.into()), text);
        self.token_idx += 1;
    }

    fn current_token(&self) -> Option<Token<'a>> {
        self.tokens.get(self.token_idx).copied()
    }
}
