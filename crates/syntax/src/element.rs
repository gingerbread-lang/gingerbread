use crate::{SyntaxNode, SyntaxToken};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxElement {
    Node(SyntaxNode),
    Token(SyntaxToken),
}

impl SyntaxElement {
    pub fn assert_node(self) -> SyntaxNode {
        match self {
            Self::Node(node) => node,
            Self::Token(_) => panic!("expected node"),
        }
    }

    pub fn assert_token(self) -> SyntaxToken {
        match self {
            Self::Node(_) => panic!("expected token"),
            Self::Token(token) => token,
        }
    }
}
