mod event;
mod grammar;
mod parser;
mod sink;

use self::event::Event;
use self::parser::Parser;
use self::sink::Sink;
use rowan::GreenNode;
use syntax::SyntaxNode;
use token::Token;

pub fn parse<'a>(tokens: impl Iterator<Item = Token<'a>>) -> Parse {
    let tokens: Vec<_> = tokens.collect();
    let events = Parser::new(&tokens).parse(grammar::root);

    Sink::new(events, tokens).finish()
}

pub struct Parse {
    green_node: GreenNode,
}

impl Parse {
    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn debug_syntax_tree(&self) -> String {
        let mut tree = format!("{:#?}", self.syntax_node());
        tree.remove(tree.len() - 1);

        tree
    }
}
