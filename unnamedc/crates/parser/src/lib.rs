pub mod error;
mod event;
mod grammar;
mod parser;
mod sink;
mod token_set;

#[cfg(test)]
mod tests;

use self::error::ParseError;
use self::parser::Parser;
use self::sink::Sink;
use std::fmt;
use syntax::SyntaxNode;
use token::Token;

pub fn parse_source_file(tokens: &[Token<'_>]) -> Parse {
    let events = Parser::new(tokens).parse(grammar::source_file);
    Sink::new(events, tokens).finish()
}

pub fn parse_repl_line(tokens: &[Token<'_>]) -> Parse {
    let events = Parser::new(tokens).parse(grammar::repl_line);
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
