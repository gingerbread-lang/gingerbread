use super::event::Event;
use super::Parser;
use crate::syntax::SyntaxKind;
use drop_bomb::DropBomb;
use std::mem;

pub(super) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Self { pos, bomb: DropBomb::new("markers must be completed") }
    }

    pub(super) fn complete(mut self, parser: &mut Parser<'_, '_>, kind: SyntaxKind) {
        self.bomb.defuse();
        let old_event = mem::replace(&mut parser.events[self.pos], Event::StartNode { kind });
        assert_eq!(old_event, Event::Placeholder);
        parser.events.push(Event::FinishNode);
    }
}
