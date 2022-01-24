use syntax::SyntaxKind;

#[derive(Debug, Clone, Copy)]
pub(crate) enum Event {
    StartNode { kind: SyntaxKind },
    FinishNode,
    AddToken,
    Placeholder,
}
