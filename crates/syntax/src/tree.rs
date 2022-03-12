use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

pub struct SyntaxTree {
    events: Vec<Event>,
    text: String,
}

#[derive(Default)]
pub struct SyntaxBuilder {
    events: Vec<Event>,
    text: String,
    start_node_idxs: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Event {
    StartNode { kind: SyntaxKind, start: u32, finish_node_pos: u32 },
    AddToken { kind: SyntaxKind, start: u32, end: u32 },
    FinishNode,
}

// the corresponding FinishNode could never be at index 0,
// since that is always a StartNode for the root note.
const FINISH_NODE_POS_PLACEHOLDER: u32 = 0;

impl SyntaxBuilder {
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.start_node_idxs.push(self.events.len());
        self.push_event(Event::StartNode {
            kind,
            finish_node_pos: FINISH_NODE_POS_PLACEHOLDER,
            start: self.text.len() as u32,
        });
    }

    pub fn add_token(&mut self, kind: SyntaxKind, text: &str) {
        let start = self.text.len();
        let end = self.text.len() + text.len();
        self.text.push_str(text);
        assert_eq!(&self.text[start..end], text);

        self.push_event(Event::AddToken { kind, start: start as u32, end: end as u32 });
    }

    pub fn finish_node(&mut self) {
        let start_node_idx = self.start_node_idxs.pop().unwrap();
        let finish_node_idx = self.events.len() as u32;
        self.push_event(Event::FinishNode);

        match &mut self.events[start_node_idx] {
            Event::StartNode { finish_node_pos, .. } => {
                assert_eq!(*finish_node_pos, FINISH_NODE_POS_PLACEHOLDER);
                *finish_node_pos = finish_node_idx;
            }
            _ => unreachable!(),
        }
    }

    pub fn finish(self) -> SyntaxTree {
        SyntaxTree { events: self.events, text: self.text }
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }
}

impl SyntaxTree {
    pub fn root(&self) -> SyntaxNode {
        SyntaxNode(0)
    }

    pub(crate) fn get_text(&self, start: u32, end: u32) -> &str {
        &self.text[start as usize..end as usize]
    }

    pub(crate) fn get_start_node(&self, idx: u32) -> (SyntaxKind, u32, u32) {
        match self.events[idx as usize] {
            Event::StartNode { kind, finish_node_pos, start } => {
                assert_ne!(finish_node_pos, FINISH_NODE_POS_PLACEHOLDER);
                (kind, finish_node_pos, start)
            }
            _ => panic!(),
        }
    }

    pub(crate) fn get_add_token(&self, idx: u32) -> (SyntaxKind, u32, u32) {
        match self.events[idx as usize] {
            Event::AddToken { kind, start, end } => (kind, start, end),
            _ => panic!(),
        }
    }

    pub(crate) fn is_start_node(&self, idx: u32) -> bool {
        matches!(self.events[idx as usize], Event::StartNode { .. })
    }

    pub(crate) fn is_add_token(&self, idx: u32) -> bool {
        matches!(self.events[idx as usize], Event::AddToken { .. })
    }

    pub(crate) fn is_finish_node(&self, idx: u32) -> bool {
        matches!(self.events[idx as usize], Event::FinishNode)
    }
}

impl std::fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !f.alternate() {
            return f
                .debug_struct("SyntaxTree")
                .field("events", &self.events)
                .field("text", &self.text)
                .finish();
        }

        let mut indentation_level = 0_usize;

        for idx in 0..self.events.len() as u32 {
            if self.is_finish_node(idx) {
                indentation_level -= 1;
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
                continue;
            }

            if self.is_add_token(idx) {
                let token = SyntaxToken(idx);
                let kind = token.kind(self);
                let text = token.text(self);
                let range = token.range(self);
                writeln!(f, "{kind:?}@{range:?} {text:?}")?;
                continue;
            }

            unreachable!()
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    fn check<const N: usize>(f: impl Fn(&mut SyntaxBuilder), events: [Event; N], text: &str) {
        let mut builder = SyntaxBuilder::default();
        f(&mut builder);
        let tree = builder.finish();
        assert_eq!((tree.events, tree.text), (events.to_vec(), text.to_string()));
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
                Event::StartNode { kind: SyntaxKind::Root, finish_node_pos: 1, start: 0 },
                Event::FinishNode,
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
                Event::StartNode { kind: SyntaxKind::Root, finish_node_pos: 2, start: 0 },
                Event::AddToken { kind: SyntaxKind::LetKw, start: 0, end: 3 },
                Event::FinishNode,
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
