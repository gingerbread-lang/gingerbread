use crate::{SyntaxKind, SyntaxNode, SyntaxToken};
use std::mem;

pub struct SyntaxTree {
    data: Vec<u8>,
    text: String,
}

#[derive(Default)]
pub struct SyntaxBuilder {
    data: Vec<u8>,
    text: String,
    start_node_idxs: Vec<usize>,
}

pub(crate) const START_NODE_SIZE: u32 = 1 + 4 + 4 + 4;
pub(crate) const ADD_TOKEN_SIZE: u32 = 1 + 4 + 4;
pub(crate) const FINISH_NODE_SIZE: u32 = 1;

// the corresponding FinishNode could never be at index 0,
// since that is always a StartNode for the root note.
const FINISH_NODE_POS_PLACEHOLDER: u32 = 0;

impl SyntaxBuilder {
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.data.reserve(START_NODE_SIZE as usize);

        self.start_node_idxs.push(self.data.len());
        self.data.push(SyntaxKind::__Last as u8 + kind as u8 + 1);
        self.data.extend_from_slice(&FINISH_NODE_POS_PLACEHOLDER.to_le_bytes());
        self.data.extend_from_slice(&(self.text.len() as u32).to_le_bytes());
        self.data.extend_from_slice(&(self.text.len() as u32).to_le_bytes());
    }

    pub fn add_token(&mut self, kind: SyntaxKind, text: &str) {
        self.data.reserve(ADD_TOKEN_SIZE as usize);

        let start = self.text.len();
        let end = self.text.len() + text.len();
        self.text.push_str(text);
        debug_assert_eq!(&self.text[start..end], text);

        self.data.push(kind as u8);
        self.data.extend_from_slice(&(start as u32).to_le_bytes());
        self.data.extend_from_slice(&(end as u32).to_le_bytes());
    }

    pub fn finish_node(&mut self) {
        self.data.reserve(FINISH_NODE_SIZE as usize);

        let start_node_idx = self.start_node_idxs.pop().unwrap();
        let finish_node_pos = self.data.len() as u32;
        self.data.push(u8::MAX);

        debug_assert!(is_tag_start_node(self.data[start_node_idx]));

        let old_finish_node_pos = &mut self.data[start_node_idx + 1..start_node_idx + 5];
        debug_assert_eq!(
            u32::from_le_bytes((*old_finish_node_pos).try_into().unwrap()),
            FINISH_NODE_POS_PLACEHOLDER
        );
        old_finish_node_pos.copy_from_slice(&finish_node_pos.to_le_bytes());

        let node_end = &mut self.data[start_node_idx + 9..start_node_idx + 13];
        node_end.copy_from_slice(&(self.text.len() as u32).to_le_bytes());
    }

    pub fn finish(self) -> SyntaxTree {
        SyntaxTree { data: self.data, text: self.text }
    }
}

impl SyntaxTree {
    pub fn root(&self) -> SyntaxNode {
        SyntaxNode(0)
    }

    pub(crate) fn get_text(&self, start: u32, end: u32) -> &str {
        &self.text[start as usize..end as usize]
    }

    pub(crate) fn get_start_node(&self, idx: u32) -> (SyntaxKind, u32, u32, u32) {
        let idx = idx as usize;
        let tag = self.data[idx];
        let finish_node_pos = self.data[idx + 1..idx + 5].try_into().unwrap();
        let start = self.data[idx + 5..idx + 9].try_into().unwrap();
        let end = self.data[idx + 9..idx + 13].try_into().unwrap();

        let kind = unsafe { mem::transmute::<u8, SyntaxKind>(tag - SyntaxKind::__Last as u8 - 1) };

        (
            kind,
            u32::from_le_bytes(finish_node_pos),
            u32::from_le_bytes(start),
            u32::from_le_bytes(end),
        )
    }

    pub(crate) fn get_add_token(&self, idx: u32) -> (SyntaxKind, u32, u32) {
        let idx = idx as usize;
        let tag = self.data[idx];
        let start = self.data[idx + 1..idx + 5].try_into().unwrap();
        let end = self.data[idx + 5..idx + 9].try_into().unwrap();

        assert!(is_tag_add_token(tag));
        let kind = unsafe { mem::transmute::<u8, SyntaxKind>(tag) };

        (kind, u32::from_le_bytes(start), u32::from_le_bytes(end))
    }

    pub(crate) fn is_start_node(&self, idx: u32) -> bool {
        is_tag_start_node(self.data[idx as usize])
    }

    pub(crate) fn is_add_token(&self, idx: u32) -> bool {
        is_tag_add_token(self.data[idx as usize])
    }

    pub(crate) fn is_finish_node(&self, idx: u32) -> bool {
        is_tag_finish_node(self.data[idx as usize])
    }
}

impl std::fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !f.alternate() {
            return f
                .debug_struct("SyntaxTree")
                .field("events", &self.data)
                .field("text", &self.text)
                .finish();
        }

        let mut indentation_level = 0_usize;

        let mut idx = 0;
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
                let node = SyntaxNode(idx);
                let kind = node.kind(self);
                let range = node.range(self);
                writeln!(f, "{kind:?}@{range:?}")?;
                indentation_level += 1;
                idx += START_NODE_SIZE;
                continue;
            }

            if self.is_add_token(idx) {
                let token = SyntaxToken(idx);
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

fn is_tag_start_node(tag: u8) -> bool {
    tag > SyntaxKind::__Last as u8 && tag != u8::MAX
}

fn is_tag_add_token(tag: u8) -> bool {
    tag < SyntaxKind::__Last as u8
}

fn is_tag_finish_node(tag: u8) -> bool {
    tag == u8::MAX
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    fn check<const N: usize>(f: impl Fn(&mut SyntaxBuilder), data: [u8; N], text: &str) {
        let mut builder = SyntaxBuilder::default();
        f(&mut builder);
        let tree = builder.finish();
        assert_eq!((tree.data, tree.text), (data.to_vec(), text.to_string()));
    }

    #[test]
    fn empty() {
        check(|_| {}, [], "");
    }

    #[test]
    fn just_root() {
        check(
            |b| {
                b.start_node(SyntaxKind::Root);
                b.finish_node();
            },
            [
                SyntaxKind::Root as u8 + SyntaxKind::__Last as u8 + 1,
                13,
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
            ],
            "",
        );
    }

    #[test]
    fn add_token() {
        check(
            |b| {
                b.start_node(SyntaxKind::Root);
                b.add_token(SyntaxKind::LetKw, "let");
                b.finish_node();
            },
            [
                SyntaxKind::Root as u8 + SyntaxKind::__Last as u8 + 1,
                22,
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
                3,
                0,
                0,
                0,
                255,
            ],
            "let",
        );
    }

    #[test]
    fn debug_empty() {
        let mut builder = SyntaxBuilder::default();
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
        let mut builder = SyntaxBuilder::default();
        builder.start_node(SyntaxKind::Root);
        builder.add_token(SyntaxKind::Comment, "# foo\n");
        builder.start_node(SyntaxKind::Function);
        builder.add_token(SyntaxKind::FncKw, "fnc");
        builder.add_token(SyntaxKind::Ident, "bar");
        builder.add_token(SyntaxKind::Arrow, "->");
        builder.start_node(SyntaxKind::Block);
        builder.add_token(SyntaxKind::LBrace, "{");
        builder.add_token(SyntaxKind::RBrace, "}");
        builder.finish_node();
        builder.add_token(SyntaxKind::Semicolon, ";");
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
