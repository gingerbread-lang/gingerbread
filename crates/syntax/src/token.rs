use crate::{SyntaxKind, SyntaxTree};
use text_size::TextRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxToken {
    pub(crate) idx: u32,
}

impl SyntaxToken {
    pub fn kind(self, tree: &SyntaxTree) -> SyntaxKind {
        tree.get_add_token(self.idx).0
    }

    pub fn text(self, tree: &SyntaxTree) -> &str {
        let (_, start, end) = tree.get_add_token(self.idx);
        tree.get_text(start, end)
    }

    pub fn range(self, tree: &SyntaxTree) -> TextRange {
        let (_, start, end) = tree.get_add_token(self.idx);
        TextRange::new(start.into(), end.into())
    }
}
