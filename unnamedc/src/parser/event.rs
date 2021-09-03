use crate::syntax::SyntaxKind;

pub(super) enum Event {
    StartNode { kind: SyntaxKind },
    FinishNode,
    Token,
    Placeholder,
}
