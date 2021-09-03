use crate::syntax::SyntaxKind;

#[derive(Debug, PartialEq)]
pub(super) enum Event {
    StartNode { kind: SyntaxKind },
    FinishNode,
    AddToken,
    Placeholder,
}
