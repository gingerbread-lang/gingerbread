mod error;
mod event;
mod grammar;
mod parser;
mod sink;

use self::error::ParseError;
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
    errors: Vec<ParseError>,
}

impl Parse {
    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn debug_syntax_tree(&self) -> String {
        let mut s = format!("{:#?}", self.syntax_node());
        s.remove(s.len() - 1);

        for error in &self.errors {
            s.push('\n');
            s.push_str(&error.to_string());
        }

        s
    }
}
