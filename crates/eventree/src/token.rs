use crate::{SyntaxKind, SyntaxTree};
use std::marker::PhantomData;
use std::num::NonZeroU32;
use text_size::TextRange;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxToken<K> {
    idx: NonZeroU32,
    phantom: PhantomData<K>,
}

static_assertions::assert_eq_size!(SyntaxToken<()>, Option<SyntaxToken<()>>, u32);

impl<K: SyntaxKind> SyntaxToken<K> {
    #[inline(always)]
    pub(crate) fn new(idx: u32) -> Self {
        Self {
            idx: if cfg!(debug_assertions) {
                NonZeroU32::new(idx).unwrap()
            } else {
                unsafe { NonZeroU32::new_unchecked(idx) }
            },
            phantom: PhantomData,
        }
    }

    pub fn kind(self, tree: &SyntaxTree<K>) -> K {
        unsafe { tree.get_add_token(self.idx.get()).0 }
    }

    pub fn text(self, tree: &SyntaxTree<K>) -> &str {
        unsafe {
            let (_, start, end) = tree.get_add_token(self.idx.get());
            tree.get_text(start, end)
        }
    }

    pub fn range(self, tree: &SyntaxTree<K>) -> TextRange {
        let (_, start, end) = unsafe { tree.get_add_token(self.idx.get()) };
        TextRange::new(start.into(), end.into())
    }
}
