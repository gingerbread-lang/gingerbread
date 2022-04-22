use super::event::Event;
use crate::{Parse, SyntaxError};
use syntax::{SyntaxBuilder, TokenKind};
use token::Tokens;

pub(crate) struct Sink<'tokens> {
    events: Vec<Event>,
    tokens: &'tokens Tokens,
    token_idx: usize,
    builder: SyntaxBuilder,
}

impl<'tokens> Sink<'tokens> {
    pub(crate) fn new(events: Vec<Event>, tokens: &'tokens Tokens, input: &str) -> Self {
        Self { events, tokens, token_idx: 0, builder: SyntaxBuilder::new(input) }
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

        Parse { syntax_tree: self.builder.finish(), errors }
    }

    #[inline(always)]
    fn process_event(&mut self, event: Event) {
        match event {
            Event::StartNode { kind } => self.builder.start_node(kind),
            Event::FinishNode => self.builder.finish_node(),
            Event::AddToken => self.add_token(),
        }
    }

    #[inline(always)]
    fn skip_trivia(&mut self) {
        while let Some(TokenKind::Whitespace | TokenKind::Comment) =
            self.tokens.get_kind(self.token_idx)
        {
            self.add_token();
        }
    }

    #[inline(always)]
    fn add_token(&mut self) {
        let kind = self.tokens.kind(self.token_idx);
        let range = self.tokens.range(self.token_idx);
        self.builder.add_token(kind, range);
        self.token_idx += 1;
    }
}
