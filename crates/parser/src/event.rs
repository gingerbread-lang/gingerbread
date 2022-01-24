use syntax::SyntaxKind;

#[derive(Debug)]
pub(crate) enum Event {
    StartNode { kind: SyntaxKind },
    FinishNode,
    AddToken,
    Placeholder,
}
