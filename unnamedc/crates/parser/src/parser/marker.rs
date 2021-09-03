use super::Parser;
use crate::event::Event;
use drop_bomb::DropBomb;
use std::mem;
use syntax::SyntaxKind;

pub(crate) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Self {
        Self { pos, bomb: DropBomb::new("markers must be completed") }
    }

    pub(crate) fn complete(mut self, p: &mut Parser<'_, '_>, kind: SyntaxKind) {
        self.bomb.defuse();
        let old_event = mem::replace(&mut p.events[self.pos], Event::StartNode { kind });
        assert_eq!(old_event, Event::Placeholder);
        p.events.push(Event::FinishNode);
    }
}
