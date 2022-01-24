use syntax::SyntaxKind;

#[derive(Debug, Clone, Copy)]
pub(crate) enum Event {
    StartNode { kind: SyntaxKind },
    FinishNode,
    AddToken,
}

static_assertions::assert_eq_size!(Event, Option<Event>, u8);
