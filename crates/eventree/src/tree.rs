use crate::{SyntaxKind, SyntaxNode, SyntaxToken};
use std::marker::PhantomData;
use std::slice;
use text_size::TextRange;

pub struct SyntaxTree<K> {
    data: Vec<u8>,
    root: u32,
    phantom: PhantomData<K>,
}

pub struct SyntaxBuilder<K> {
    data: Vec<u8>,
    root: Option<u32>,
    current_len: u32,
    start_node_idxs: Vec<usize>,
    phantom: PhantomData<K>,
}

pub(crate) const START_NODE_SIZE: u32 = 2 + 4 + 4 + 4;
pub(crate) const ADD_TOKEN_SIZE: u32 = 2 + 4 + 4;
pub(crate) const FINISH_NODE_SIZE: u32 = 2;

const FINISH_NODE_IDX_PLACEHOLDER: u32 = 0;

impl<K: SyntaxKind> SyntaxBuilder<K> {
    pub fn new(text: &str) -> Self {
        debug_assert!(K::LAST < u16::MAX / 2);

        Self {
            data: text.as_bytes().to_vec(),
            root: None,
            current_len: 0,
            start_node_idxs: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub fn start_node(&mut self, kind: K) {
        if self.root.is_none() {
            self.root = Some(self.data.len() as u32);
        }

        self.start_node_idxs.push(self.data.len());

        self.data.reserve(START_NODE_SIZE as usize);
        unsafe {
            let ptr = self.data_end_ptr();
            (ptr as *mut u16).write_unaligned((K::LAST + kind.to_raw() + 1).to_le());
            (ptr.add(2) as *mut u32).write_unaligned(FINISH_NODE_IDX_PLACEHOLDER.to_le());
            (ptr.add(6) as *mut u32).write_unaligned(self.current_len.to_le());
            (ptr.add(10) as *mut u32).write_unaligned(self.current_len.to_le());
            self.data.set_len(self.data.len() + START_NODE_SIZE as usize);
        }
    }

    pub fn add_token(&mut self, kind: K, range: TextRange) {
        let start = u32::from(range.start());
        let end = u32::from(range.end());
        self.current_len = end;

        self.data.reserve(ADD_TOKEN_SIZE as usize);
        unsafe {
            let ptr = self.data_end_ptr();
            (ptr as *mut u16).write_unaligned(kind.to_raw().to_le());
            (ptr.add(2) as *mut u32).write_unaligned(start.to_le());
            (ptr.add(6) as *mut u32).write_unaligned(end.to_le());
            self.data.set_len(self.data.len() + ADD_TOKEN_SIZE as usize);
        }
    }

    pub fn finish_node(&mut self) {
        let start_node_idx = self.start_node_idxs.pop().unwrap();
        let finish_node_idx = self.data.len() as u32;

        self.data.reserve(FINISH_NODE_SIZE as usize);
        unsafe {
            let ptr = self.data_end_ptr() as *mut u16;
            ptr.write_unaligned(u16::MAX.to_le());
            self.data.set_len(self.data.len() + FINISH_NODE_SIZE as usize);
        }

        unsafe {
            let ptr = self.data.as_mut_ptr().add(start_node_idx);
            debug_assert!(is_tag_start_node::<K>((ptr as *const u16).read_unaligned().to_le()));

            debug_assert_eq!(
                (ptr.add(2) as *const u32).read_unaligned().to_le(),
                FINISH_NODE_IDX_PLACEHOLDER
            );
            (ptr.add(2) as *mut u32).write_unaligned(finish_node_idx.to_le());

            (ptr.add(10) as *mut u32).write_unaligned(self.current_len.to_le());
        }
    }

    pub fn finish(self) -> SyntaxTree<K> {
        let Self { mut data, root, current_len: _, start_node_idxs: _, phantom: _ } = self;
        data.shrink_to_fit();

        SyntaxTree { data, root: root.unwrap(), phantom: PhantomData }
    }

    fn data_end_ptr(&mut self) -> *mut u8 {
        unsafe { self.data.as_mut_ptr().add(self.data.len()) }
    }
}

impl<K: SyntaxKind> SyntaxTree<K> {
    pub fn root(&self) -> SyntaxNode<K> {
        SyntaxNode { idx: self.root, phantom: PhantomData }
    }

    pub(crate) fn get_text(&self, start: u32, end: u32) -> &str {
        let start = start as usize;
        let end = end as usize;

        unsafe {
            let slice = slice::from_raw_parts(self.data.as_ptr().add(start), end - start);

            if cfg!(debug_assertions) {
                std::str::from_utf8(slice).unwrap()
            } else {
                std::str::from_utf8_unchecked(slice)
            }
        }
    }

    pub(crate) fn get_start_node(&self, idx: u32) -> (K, u32, u32, u32) {
        let idx = idx as usize;
        debug_assert!(idx + START_NODE_SIZE as usize <= self.data.len());

        unsafe {
            let ptr = self.data.as_ptr().add(idx);
            let tag = (ptr as *const u16).read_unaligned().to_le();
            let finish_node_idx = (ptr.add(2) as *const u32).read_unaligned().to_le();
            let start = (ptr.add(6) as *const u32).read_unaligned().to_le();
            let end = (ptr.add(10) as *const u32).read_unaligned().to_le();

            debug_assert!(is_tag_start_node::<K>(tag));
            let kind = K::from_raw(tag - K::LAST - 1);

            (kind, finish_node_idx, start, end)
        }
    }

    pub(crate) fn get_add_token(&self, idx: u32) -> (K, u32, u32) {
        let idx = idx as usize;
        debug_assert!(idx + ADD_TOKEN_SIZE as usize <= self.data.len());

        unsafe {
            let ptr = self.data.as_ptr().add(idx);
            let tag = (ptr as *const u16).read_unaligned().to_le();
            let start = (ptr.add(2) as *const u32).read_unaligned().to_le();
            let end = (ptr.add(6) as *const u32).read_unaligned().to_le();

            debug_assert!(is_tag_add_token::<K>(tag));
            let kind = K::from_raw(tag);

            (kind, start, end)
        }
    }

    pub(crate) fn is_start_node(&self, idx: u32) -> bool {
        let idx = idx as usize;
        debug_assert!(idx < self.data.len());
        is_tag_start_node::<K>(
            unsafe { (self.data.as_ptr().add(idx) as *const u16).read_unaligned() }.to_le(),
        )
    }

    pub(crate) fn is_add_token(&self, idx: u32) -> bool {
        let idx = idx as usize;
        debug_assert!(idx < self.data.len());
        is_tag_add_token::<K>(
            unsafe { (self.data.as_ptr().add(idx) as *const u16).read_unaligned() }.to_le(),
        )
    }

    pub(crate) fn is_finish_node(&self, idx: u32) -> bool {
        let idx = idx as usize;
        debug_assert!(idx < self.data.len());
        is_tag_finish_node(
            unsafe { (self.data.as_ptr().add(idx) as *const u16).read_unaligned() }.to_le(),
        )
    }
}

impl<K: SyntaxKind> std::fmt::Debug for SyntaxTree<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !f.alternate() {
            return f.debug_struct("SyntaxTree").field("data", &self.data).finish();
        }

        let mut indentation_level = 0_usize;

        let mut idx = self.root;
        while idx < self.data.len() as u32 {
            if self.is_finish_node(idx) {
                indentation_level -= 1;
                idx += FINISH_NODE_SIZE;
                continue;
            }

            for _ in 0..indentation_level {
                write!(f, "  ")?;
            }

            if self.is_start_node(idx) {
                let node = SyntaxNode { idx, phantom: PhantomData };
                let kind = node.kind(self);
                let range = node.range(self);
                writeln!(f, "{kind:?}@{range:?}")?;
                indentation_level += 1;
                idx += START_NODE_SIZE;
                continue;
            }

            if self.is_add_token(idx) {
                let token = SyntaxToken { idx, phantom: PhantomData };
                let kind = token.kind(self);
                let text = token.text(self);
                let range = token.range(self);
                writeln!(f, "{kind:?}@{range:?} {text:?}")?;
                idx += ADD_TOKEN_SIZE;
                continue;
            }

            unreachable!()
        }

        Ok(())
    }
}

fn is_tag_start_node<K: SyntaxKind>(tag: u16) -> bool {
    tag > K::LAST && tag != u16::MAX
}

fn is_tag_add_token<K: SyntaxKind>(tag: u16) -> bool {
    tag < K::LAST
}

fn is_tag_finish_node(tag: u16) -> bool {
    tag == u16::MAX
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(u16)]
    enum SyntaxKind {
        Root,
        Arrow,
        Block,
        Comment,
        FncKw,
        Function,
        Ident,
        LBrace,
        LetKw,
        RBrace,
        Semicolon,
        __Last,
    }

    unsafe impl crate::SyntaxKind for SyntaxKind {
        const LAST: u16 = Self::__Last as u16;

        fn to_raw(self) -> u16 {
            self as u16
        }

        unsafe fn from_raw(raw: u16) -> Self {
            std::mem::transmute(raw)
        }
    }

    fn check<const N: usize>(
        input: &str,
        f: impl Fn(&mut SyntaxBuilder<SyntaxKind>),
        data: [u8; N],
    ) {
        let mut builder = SyntaxBuilder::new(input);
        f(&mut builder);
        let tree = builder.finish();
        assert_eq!(tree.data, data);
    }

    #[test]
    fn just_root() {
        check(
            "",
            |b| {
                b.start_node(SyntaxKind::Root);
                b.finish_node();
            },
            [
                SyntaxKind::Root as u8 + SyntaxKind::__Last as u8 + 1,
                0,
                14,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                255,
                255,
            ],
        );
    }

    #[test]
    fn add_token() {
        check(
            "let",
            |b| {
                b.start_node(SyntaxKind::Root);
                b.add_token(SyntaxKind::LetKw, TextRange::new(0.into(), 3.into()));
                b.finish_node();
            },
            [
                b"l"[0],
                b"e"[0],
                b"t"[0],
                SyntaxKind::Root as u8 + SyntaxKind::__Last as u8 + 1,
                0,
                27,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                3,
                0,
                0,
                0,
                SyntaxKind::LetKw as u8,
                0,
                0,
                0,
                0,
                0,
                3,
                0,
                0,
                0,
                255,
                255,
            ],
        );
    }

    #[test]
    fn debug_empty() {
        let mut builder = SyntaxBuilder::new("");
        builder.start_node(SyntaxKind::Root);
        builder.finish_node();

        let tree = builder.finish();
        expect![[r##"
            Root@0..0
        "##]]
        .assert_eq(&format!("{tree:#?}"));
    }

    #[test]
    fn debug_complex() {
        let mut builder = SyntaxBuilder::new("# foo\nfncbar->{};");
        builder.start_node(SyntaxKind::Root);
        builder.add_token(SyntaxKind::Comment, TextRange::new(0.into(), 6.into()));
        builder.start_node(SyntaxKind::Function);
        builder.add_token(SyntaxKind::FncKw, TextRange::new(6.into(), 9.into()));
        builder.add_token(SyntaxKind::Ident, TextRange::new(9.into(), 12.into()));
        builder.add_token(SyntaxKind::Arrow, TextRange::new(12.into(), 14.into()));
        builder.start_node(SyntaxKind::Block);
        builder.add_token(SyntaxKind::LBrace, TextRange::new(14.into(), 15.into()));
        builder.add_token(SyntaxKind::RBrace, TextRange::new(15.into(), 16.into()));
        builder.finish_node();
        builder.add_token(SyntaxKind::Semicolon, TextRange::new(16.into(), 17.into()));
        builder.finish_node();
        builder.finish_node();

        let tree = builder.finish();
        expect![[r##"
            Root@0..17
              Comment@0..6 "# foo\n"
              Function@6..17
                FncKw@6..9 "fnc"
                Ident@9..12 "bar"
                Arrow@12..14 "->"
                Block@14..16
                  LBrace@14..15 "{"
                  RBrace@15..16 "}"
                Semicolon@16..17 ";"
        "##]]
        .assert_eq(&format!("{tree:#?}"));
    }
}
