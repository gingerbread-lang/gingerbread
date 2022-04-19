use crate::{SyntaxKind, SyntaxTree};
use std::marker::PhantomData;
use text_size::TextRange;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxToken<K> {
    idx: u32,
    phantom: PhantomData<K>,
}

impl<K: SyntaxKind> SyntaxToken<K> {
    pub(crate) fn new(idx: u32) -> Self {
        Self { idx, phantom: PhantomData }
    }

    pub fn kind(self, tree: &SyntaxTree<K>) -> K {
        unsafe { tree.get_add_token(self.idx).0 }
    }

    pub fn text(self, tree: &SyntaxTree<K>) -> &str {
        unsafe {
            let (_, start, end) = tree.get_add_token(self.idx);
            tree.get_text(start, end)
        }
    }

    pub fn range(self, tree: &SyntaxTree<K>) -> TextRange {
        let (_, start, end) = unsafe { tree.get_add_token(self.idx) };
        TextRange::new(start.into(), end.into())
    }
}
