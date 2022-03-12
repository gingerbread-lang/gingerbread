use crate::{SyntaxKind, SyntaxToken, SyntaxTree};
use text_size::TextRange;

#[derive(Debug, Clone, Copy)]
pub struct SyntaxNode(pub(crate) u32);

impl SyntaxNode {
    pub fn kind(self, tree: &SyntaxTree) -> SyntaxKind {
        tree.get_start_node(self.0).0
    }

    pub fn child_nodes(self, tree: &SyntaxTree) -> impl Iterator<Item = SyntaxNode> + '_ {
        ChildNodes { pos: self.0 + 1, finish_pos: tree.get_start_node(self.0).1, tree }
    }

    pub fn child_tokens(self, tree: &SyntaxTree) -> impl Iterator<Item = SyntaxToken> + '_ {
        ChildTokens { pos: self.0 + 1, finish_pos: tree.get_start_node(self.0).1, tree }
    }

    pub fn descendant_nodes(self, tree: &SyntaxTree) -> impl Iterator<Item = SyntaxNode> + '_ {
        DescendantNodes { pos: self.0 + 1, finish_pos: tree.get_start_node(self.0).1, tree }
    }

    pub fn descendant_tokens(self, tree: &SyntaxTree) -> impl Iterator<Item = SyntaxToken> + '_ {
        DescendantTokens { pos: self.0 + 1, finish_pos: tree.get_start_node(self.0).1, tree }
    }

    pub fn range(self, tree: &SyntaxTree) -> TextRange {
        let (_, finish_node_pos, start) = tree.get_start_node(self.0);

        let has_no_children = finish_node_pos == self.0 + 1;
        if has_no_children {
            return TextRange::empty(start.into());
        }

        let mut first_token_idx = self.0 + 1;

        let first_token_range = loop {
            if tree.is_add_token(first_token_idx) {
                break SyntaxToken(first_token_idx).range(tree);
            }
            first_token_idx += 1;
        };

        let mut last_token_idx = finish_node_pos - 1;
        let last_token_range = loop {
            if tree.is_add_token(last_token_idx) {
                break SyntaxToken(last_token_idx).range(tree);
            }
            last_token_idx -= 1;
        };

        TextRange::new(first_token_range.start(), last_token_range.end())
    }

    pub fn text(self, tree: &SyntaxTree) -> String {
        let mut s = String::with_capacity(self.range(tree).len().into());
        for token in self.descendant_tokens(tree) {
            s.push_str(token.text(tree));
        }

        s
    }
}

struct ChildNodes<'a> {
    pos: u32,
    finish_pos: u32,
    tree: &'a SyntaxTree,
}

impl Iterator for ChildNodes<'_> {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.tree.is_start_node(self.pos) {
                let (_, finish_node_pos, _) = self.tree.get_start_node(self.pos);
                let node = SyntaxNode(self.pos);
                self.pos = finish_node_pos + 1;
                return Some(node);
            }

            if self.pos == self.finish_pos {
                return None;
            }

            assert!(self.tree.is_add_token(self.pos));
            self.pos += 1;
        }
    }
}

struct ChildTokens<'a> {
    finish_pos: u32,
    pos: u32,
    tree: &'a SyntaxTree,
}

impl Iterator for ChildTokens<'_> {
    type Item = SyntaxToken;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.tree.is_start_node(self.pos) {
                let (_, finish_node_pos, _) = self.tree.get_start_node(self.pos);
                self.pos = finish_node_pos + 1;
                continue;
            }

            if self.tree.is_add_token(self.pos) {
                let token = SyntaxToken(self.pos);
                self.pos += 1;
                return Some(token);
            }

            if self.pos == self.finish_pos {
                return None;
            }

            unreachable!()
        }
    }
}

struct DescendantNodes<'a> {
    finish_pos: u32,
    pos: u32,
    tree: &'a SyntaxTree,
}

impl Iterator for DescendantNodes<'_> {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.tree.is_start_node(self.pos) {
                let node = SyntaxNode(self.pos);
                self.pos += 1;
                return Some(node);
            }

            if self.pos == self.finish_pos {
                return None;
            }

            assert!(self.tree.is_add_token(self.pos) || self.tree.is_finish_node(self.pos));
            self.pos += 1;
        }
    }
}

struct DescendantTokens<'a> {
    finish_pos: u32,
    pos: u32,
    tree: &'a SyntaxTree,
}

impl Iterator for DescendantTokens<'_> {
    type Item = SyntaxToken;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.tree.is_add_token(self.pos) {
                let token = SyntaxToken(self.pos);
                self.pos += 1;
                return Some(token);
            }

            if self.pos == self.finish_pos {
                return None;
            }

            assert!(self.tree.is_start_node(self.pos) || self.tree.is_finish_node(self.pos));
            self.pos += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SyntaxBuilder;

    fn example_tree() -> SyntaxTree {
        let mut builder = SyntaxBuilder::default();

        builder.start_node(SyntaxKind::Root);
        {
            builder.start_node(SyntaxKind::BinaryExpr);
            {
                builder.start_node(SyntaxKind::BinaryExpr);
                builder.add_token(SyntaxKind::IntLiteral, "2");
                builder.add_token(SyntaxKind::Asterisk, "*");
                builder.add_token(SyntaxKind::IntLiteral, "5");
                builder.finish_node();
            }
            builder.add_token(SyntaxKind::Plus, "+");
            builder.add_token(SyntaxKind::IntLiteral, "10");
            builder.finish_node();
        }
        {
            builder.start_node(SyntaxKind::Call);
            builder.add_token(SyntaxKind::Ident, "foo");
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
