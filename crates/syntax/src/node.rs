use crate::tree::{ADD_TOKEN_SIZE, FINISH_NODE_SIZE, START_NODE_SIZE};
use crate::{SyntaxKind, SyntaxToken, SyntaxTree};
use text_size::TextRange;

#[derive(Debug, Clone, Copy)]
pub struct SyntaxNode {
    pub(crate) idx: u32,
}

impl SyntaxNode {
    pub fn kind(self, tree: &SyntaxTree) -> SyntaxKind {
        tree.get_start_node(self.idx).0
    }

    pub fn child_nodes(self, tree: &SyntaxTree) -> impl Iterator<Item = SyntaxNode> + '_ {
        ChildNodes {
            idx: self.idx + START_NODE_SIZE,
            finish_idx: tree.get_start_node(self.idx).1,
            tree,
        }
    }

    pub fn child_tokens(self, tree: &SyntaxTree) -> impl Iterator<Item = SyntaxToken> + '_ {
        ChildTokens {
            idx: self.idx + START_NODE_SIZE,
            finish_idx: tree.get_start_node(self.idx).1,
            tree,
        }
    }

    pub fn descendant_nodes(self, tree: &SyntaxTree) -> impl Iterator<Item = SyntaxNode> + '_ {
        DescendantNodes {
            idx: self.idx + START_NODE_SIZE,
            finish_idx: tree.get_start_node(self.idx).1,
            tree,
        }
    }

    pub fn descendant_tokens(self, tree: &SyntaxTree) -> impl Iterator<Item = SyntaxToken> + '_ {
        DescendantTokens {
            idx: self.idx + START_NODE_SIZE,
            finish_idx: tree.get_start_node(self.idx).1,
            tree,
        }
    }

    pub fn range(self, tree: &SyntaxTree) -> TextRange {
        let (_, _, start, end) = tree.get_start_node(self.idx);
        TextRange::new(start.into(), end.into())
    }

    pub fn text(self, tree: &SyntaxTree) -> &str {
        let (_, _, start, end) = tree.get_start_node(self.idx);
        tree.get_text(start, end)
    }
}

struct ChildNodes<'a> {
    idx: u32,
    finish_idx: u32,
    tree: &'a SyntaxTree,
}

impl Iterator for ChildNodes<'_> {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < self.finish_idx {
            if self.tree.is_start_node(self.idx) {
                let (_, finish_node_idx, _, _) = self.tree.get_start_node(self.idx);
                let node = SyntaxNode { idx: self.idx };
                self.idx = finish_node_idx + FINISH_NODE_SIZE;
                return Some(node);
            }

            if self.tree.is_add_token(self.idx) {
                self.idx += ADD_TOKEN_SIZE;
                continue;
            }

            unreachable!()
        }

        None
    }
}

struct ChildTokens<'a> {
    finish_idx: u32,
    idx: u32,
    tree: &'a SyntaxTree,
}

impl Iterator for ChildTokens<'_> {
    type Item = SyntaxToken;

    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < self.finish_idx {
            if self.tree.is_start_node(self.idx) {
                let (_, finish_node_idx, _, _) = self.tree.get_start_node(self.idx);
                self.idx = finish_node_idx + FINISH_NODE_SIZE;
                continue;
            }

            if self.tree.is_add_token(self.idx) {
                let token = SyntaxToken { idx: self.idx };
                self.idx += ADD_TOKEN_SIZE;
                return Some(token);
            }

            unreachable!()
        }

        None
    }
}

struct DescendantNodes<'a> {
    finish_idx: u32,
    idx: u32,
    tree: &'a SyntaxTree,
}

impl Iterator for DescendantNodes<'_> {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < self.finish_idx {
            if self.tree.is_start_node(self.idx) {
                let node = SyntaxNode { idx: self.idx };
                self.idx += START_NODE_SIZE;
                return Some(node);
            }

            if self.tree.is_add_token(self.idx) {
                self.idx += ADD_TOKEN_SIZE;
                continue;
            }

            if self.tree.is_finish_node(self.idx) {
                self.idx += FINISH_NODE_SIZE;
                continue;
            }

            unreachable!()
        }

        None
    }
}

struct DescendantTokens<'a> {
    finish_idx: u32,
    idx: u32,
    tree: &'a SyntaxTree,
}

impl Iterator for DescendantTokens<'_> {
    type Item = SyntaxToken;

    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < self.finish_idx {
            if self.tree.is_add_token(self.idx) {
                let token = SyntaxToken { idx: self.idx };
                self.idx += ADD_TOKEN_SIZE;
                return Some(token);
            }

            if self.tree.is_start_node(self.idx) {
                self.idx += START_NODE_SIZE;
                continue;
            }

            if self.tree.is_finish_node(self.idx) {
                self.idx += FINISH_NODE_SIZE;
                continue;
            }

            unreachable!()
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SyntaxBuilder;

    fn example_tree() -> SyntaxTree {
        let mut builder = SyntaxBuilder::new("2*5+10foo");

        builder.start_node(SyntaxKind::Root);
        {
            builder.start_node(SyntaxKind::BinaryExpr);
            {
                builder.start_node(SyntaxKind::BinaryExpr);
                builder.add_token(SyntaxKind::IntLiteral, TextRange::new(0.into(), 1.into()));
                builder.add_token(SyntaxKind::Asterisk, TextRange::new(1.into(), 2.into()));
                builder.add_token(SyntaxKind::IntLiteral, TextRange::new(2.into(), 3.into()));
                builder.finish_node();
            }
            builder.add_token(SyntaxKind::Plus, TextRange::new(3.into(), 4.into()));
            builder.add_token(SyntaxKind::IntLiteral, TextRange::new(4.into(), 6.into()));
            builder.finish_node();
        }
        {
            builder.start_node(SyntaxKind::Call);
            builder.add_token(SyntaxKind::Ident, TextRange::new(6.into(), 9.into()));
            builder.finish_node();
        }
        builder.finish_node();

        builder.finish()
    }

    #[test]
    fn child_nodes() {
        let tree = example_tree();
        let root = tree.root();

        let mut child_nodes = root.child_nodes(&tree);
        let binary_expr = child_nodes.next().unwrap();
        assert_eq!(binary_expr.kind(&tree), SyntaxKind::BinaryExpr);
        let call = child_nodes.next().unwrap();
        assert_eq!(call.kind(&tree), SyntaxKind::Call);
        assert!(child_nodes.next().is_none());

        let mut child_nodes = binary_expr.child_nodes(&tree);
        assert_eq!(child_nodes.next().unwrap().kind(&tree), SyntaxKind::BinaryExpr);
        assert!(child_nodes.next().is_none());

        let mut child_nodes = call.child_nodes(&tree);
        assert!(child_nodes.next().is_none());
    }

    #[test]
    fn child_tokens() {
        let tree = example_tree();
        let root = tree.root();

        let mut child_tokens = root.child_tokens(&tree);
        assert!(child_tokens.next().is_none());

        let mut child_nodes = root.child_nodes(&tree);
        let binary_expr = child_nodes.next().unwrap();
        assert_eq!(binary_expr.kind(&tree), SyntaxKind::BinaryExpr);
        let call = child_nodes.next().unwrap();
        assert_eq!(call.kind(&tree), SyntaxKind::Call);
        assert!(child_nodes.next().is_none());

        let mut child_tokens = binary_expr.child_tokens(&tree);
        assert_eq!(child_tokens.next().unwrap().kind(&tree), SyntaxKind::Plus);
        assert_eq!(child_tokens.next().unwrap().kind(&tree), SyntaxKind::IntLiteral);
        assert!(child_tokens.next().is_none());

        let mut child_tokens = call.child_tokens(&tree);
        assert_eq!(child_tokens.next().unwrap().kind(&tree), SyntaxKind::Ident);
        assert!(child_tokens.next().is_none());
    }

    #[test]
    fn descendant_nodes() {
        let tree = example_tree();
        let root = tree.root();

        let mut descendant_nodes = root.descendant_nodes(&tree);
        let binary_expr = descendant_nodes.next().unwrap();
        assert_eq!(binary_expr.kind(&tree), SyntaxKind::BinaryExpr);
        let binary_expr_2 = descendant_nodes.next().unwrap();
        assert_eq!(binary_expr_2.kind(&tree), SyntaxKind::BinaryExpr);
        let call = descendant_nodes.next().unwrap();
        assert_eq!(call.kind(&tree), SyntaxKind::Call);
        assert!(descendant_nodes.next().is_none());

        let mut descendant_nodes = binary_expr.child_nodes(&tree);
        assert_eq!(descendant_nodes.next().unwrap().kind(&tree), SyntaxKind::BinaryExpr);
        assert!(descendant_nodes.next().is_none());

        let mut descendant_nodes = call.child_nodes(&tree);
        assert!(descendant_nodes.next().is_none());
    }

    #[test]
    fn descendant_tokens() {
        let tree = example_tree();
        let root = tree.root();

        let mut descendant_tokens = root.descendant_tokens(&tree);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::IntLiteral);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::Asterisk);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::IntLiteral);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::Plus);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::IntLiteral);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::Ident);
        assert!(descendant_tokens.next().is_none());

        let mut child_nodes = root.child_nodes(&tree);

        let binary_expr = child_nodes.next().unwrap();
        assert_eq!(binary_expr.kind(&tree), SyntaxKind::BinaryExpr);
        let mut descendant_tokens = binary_expr.descendant_tokens(&tree);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::IntLiteral);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::Asterisk);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::IntLiteral);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::Plus);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::IntLiteral);
        assert!(descendant_tokens.next().is_none());

        let call = child_nodes.next().unwrap();
        assert_eq!(call.kind(&tree), SyntaxKind::Call);
        let mut descendant_tokens = call.descendant_tokens(&tree);
        assert_eq!(descendant_tokens.next().unwrap().kind(&tree), SyntaxKind::Ident);
        assert!(descendant_tokens.next().is_none());

        assert!(child_nodes.next().is_none());
    }
}
