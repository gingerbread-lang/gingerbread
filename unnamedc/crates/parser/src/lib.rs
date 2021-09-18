pub mod error;
mod event;
mod grammar;
mod parser;
mod sink;
mod token_set;

use self::error::ParseError;
use self::parser::Parser;
use self::sink::Sink;
use std::fmt;
use syntax::SyntaxNode;
use token::Token;

pub fn parse(tokens: &[Token<'_>]) -> Parse {
    let events = Parser::new(tokens).parse(grammar::root);
    Sink::new(events, tokens).finish()
}

pub struct Parse {
    syntax_node: SyntaxNode,
    errors: Vec<ParseError>,
}

impl Parse {
    pub fn syntax_node(&self) -> SyntaxNode {
        self.syntax_node.clone()
    }

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }
}

impl fmt::Debug for Parse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tree = format!("{:#?}", self.syntax_node());
        write!(f, "{}", &tree[0..tree.len() - 1])?;

        for error in &self.errors {
            write!(f, "\n{:?}", error)?;
        }

        Ok(())
    }
}
