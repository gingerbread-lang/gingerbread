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
        assert!(matches!(self.events.get(0), Some(Event::StartNode { .. })));
        assert!(matches!(self.events.last(), Some(Event::FinishNode)));

        // We want to avoid nodes having trailing trivia:
        //
        // BinaryExpr
        //   BinaryExpr
        //     [1] [ ] [*] [ ] [2] [ ]
        //   [+] [ ] [3]
        //
        // An error attached to the nested BinaryExpr would include
        // the trailing whitespace in its range:
        //
        // 1 * 2 + 3
        // ^^^^^^

        // we go through all events apart from the last one,
        // since we can’t peek what the next event is when we’re at the last
        // and thus need to handle it specially

        let mut current = self.events.as_ptr();
        let mut next = unsafe { current.add(1) };
        let last = &self.events[self.events.len() - 1] as *const _;

        while current != last {
            self.process_event(unsafe { *current });

            match unsafe { *next } {
                Event::StartNode { .. } | Event::AddToken => self.skip_trivia(),
                Event::FinishNode => {}
            }

            current = next;
            next = unsafe { current.add(1) };
        }

        // unconditionally skip any trivia before processing the last event
        // to ensure we don’t miss trailing trivia at the end of the input
        self.skip_trivia();
        self.process_event(unsafe { *last });

        Parse { syntax_node: self.builder.finish(), errors }
    }

    fn process_event(&mut self, event: Event) {
        match event {
            Event::StartNode { kind } => self.builder.start_node(kind),
            Event::FinishNode => self.builder.finish_node(),
            Event::AddToken => self.add_token(),
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
