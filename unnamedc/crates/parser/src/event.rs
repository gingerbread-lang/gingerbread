use crate::SyntaxError;
use syntax::SyntaxKind;

#[derive(Debug, PartialEq)]
pub(crate) enum Event {
    StartNode { kind: SyntaxKind },
    FinishNode,
    AddToken,
    Error(SyntaxError),
    Placeholder,
}
