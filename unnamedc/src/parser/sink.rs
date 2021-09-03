use super::Event;
use crate::lexer::{Token, TokenKind};
use crate::syntax::UnnamedLang;
use crate::Parse;
use rowan::{GreenNodeBuilder, Language};
use std::mem;

pub(super) struct Sink<'a> {
    events: Vec<Event>,
    tokens: Vec<Token<'a>>,
    token_idx: usize,
    builder: GreenNodeBuilder<'static>,
}

impl<'a> Sink<'a> {
    pub(super) fn new(events: Vec<Event>, tokens: Vec<Token<'a>>) -> Self {
        Self { events, tokens, token_idx: 0, builder: GreenNodeBuilder::new() }
    }

    pub(super) fn finish(mut self) -> Parse {
        for event_idx in 0..self.events.len() {
            match mem::replace(&mut self.events[event_idx], Event::Placeholder) {
                Event::StartNode { kind } => {
                    self.builder.start_node(UnnamedLang::kind_to_raw(kind))
                }
                Event::FinishNode => self.builder.finish_node(),
                Event::Token => self.add_token(),
                Event::Placeholder => unreachable!(),
            }

            self.skip_whitespace();
        }

        Parse { green_node: self.builder.finish() }
    }

    fn skip_whitespace(&mut self) {
        if let Some(Token { kind: TokenKind::Whitespace, .. }) = self.current_token() {
            self.add_token();
        }
    }

    fn add_token(&mut self) {
        let Token { kind, text } = self.current_token().unwrap();
        self.builder.token(UnnamedLang::kind_to_raw(kind.into()), text);
        self.token_idx += 1;
    }

    fn current_token(&self) -> Option<Token<'a>> {
        self.tokens.get(self.token_idx).copied()
    }
}
