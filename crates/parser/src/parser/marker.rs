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
    pub(super) fn new(pos: usize) -> Self {
        Self { pos, bomb: DropBomb::new("markers must be completed") }
    }

    pub(crate) fn complete(mut self, p: &mut Parser<'_, '_>, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();
        let old_event = mem::replace(&mut p.events[self.pos], Some(Event::StartNode { kind }));
        debug_assert!(old_event.is_none());
        p.events.push(Some(Event::FinishNode));

        CompletedMarker { pos: self.pos }
    }
}

pub(crate) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    pub(crate) fn precede(self, p: &mut Parser<'_, '_>) -> Marker {
        p.events.insert(self.pos, None);
        Marker::new(self.pos)
    }
}
