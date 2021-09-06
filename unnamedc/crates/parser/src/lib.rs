mod error;
mod event;
mod grammar;
mod parser;
mod sink;
mod token_set;

use self::error::ParseError;
use self::event::Event;
use self::parser::Parser;
use self::sink::Sink;
use rowan::GreenNode;
use std::fmt;
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
}

impl fmt::Debug for Parse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tree = format!("{:#?}", self.syntax_node());
        write!(f, "{}", &tree[0..tree.len() - 1])?;

        for error in &self.errors {
            write!(f, "\n{}", error)?;
        }

        Ok(())
    }
}
