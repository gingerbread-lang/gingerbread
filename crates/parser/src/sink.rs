use super::event::Event;
use crate::{Parse, SyntaxError};
use syntax::SyntaxBuilder;
use token::{Token, TokenKind};

pub(crate) struct Sink<'tokens, 'input> {
    events: Vec<Event>,
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    builder: SyntaxBuilder,
}

impl<'tokens, 'input> Sink<'tokens, 'input> {
    pub(crate) fn new(events: Vec<Event>, tokens: &'tokens [Token<'input>]) -> Self {
        Self { events, tokens, token_idx: 0, builder: SyntaxBuilder::default() }
    }

    pub(crate) fn finish(mut self, errors: Vec<SyntaxError>) -> Parse {
        // the first event always starts the root node,
        // and the last event always finishes that node
        debug_assert!(matches!(self.events[0], Event::StartNode { .. }));
        debug_assert!(matches!(self.events[self.events.len() - 1], Event::FinishNode));

        // We want to avoid nodes having trailing trivia:
        //
        // BinExpr
        //   BinExpr
        //     [1] [ ] [*] [ ] [2] [ ]
        //   [+] [ ] [3]
        //
        // An error attached to the nested BinExpr would include
        // the trailing whitespace in its range:
        //
        // 1 * 2 + 3
        // ^^^^^^

        // we go through all event indices apart from the last one,
        // since we can’t peek what the next event is when we’re at the last
        // and thus need to handle it specially
        for event_idx in 0..self.events.len() - 1 {
            self.process_event(event_idx);

            let next_event = &self.events[event_idx + 1];
            match next_event {
                Event::StartNode { .. } | Event::AddToken => self.skip_trivia(),
                Event::FinishNode => {}
                Event::Placeholder => unreachable!(),
            }
        }
        // unconditionally skip any trivia before processing the last event
        // to ensure we don’t miss trailing trivia at the end of the input
        self.skip_trivia();
        self.process_event(self.events.len() - 1);

        Parse { syntax_node: self.builder.finish(), errors }
    }

    fn process_event(&mut self, idx: usize) {
        match self.events[idx] {
            Event::StartNode { kind } => self.builder.start_node(kind),
            Event::FinishNode => self.builder.finish_node(),
            Event::AddToken => self.add_token(),
            Event::Placeholder => unreachable!(),
        }
    }

    fn skip_trivia(&mut self) {
        while let Some(Token { kind: TokenKind::Whitespace | TokenKind::Comment, .. }) =
            self.current_token()
        {
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
