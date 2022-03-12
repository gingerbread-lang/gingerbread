mod error;
mod event;
mod grammar;
mod parser;
mod sink;
mod token_set;

#[cfg(test)]
mod tests;

pub use self::error::{ExpectedSyntax, SyntaxError, SyntaxErrorKind};

use self::parser::Parser;
use self::sink::Sink;
use std::fmt;
use syntax::SyntaxTree;
use token::Token;

pub fn parse_source_file(tokens: &[Token<'_>]) -> Parse {
    let (events, errors) = Parser::new(tokens).parse(grammar::source_file);
    Sink::new(events, tokens).finish(errors)
}

pub fn parse_repl_line(tokens: &[Token<'_>]) -> Parse {
    let (events, errors) = Parser::new(tokens).parse(grammar::repl_line);
    Sink::new(events, tokens).finish(errors)
}

pub struct Parse {
    syntax_tree: SyntaxTree,
    errors: Vec<SyntaxError>,
}

impl Parse {
    pub fn syntax_tree(&self) -> &SyntaxTree {
        &self.syntax_tree
    }

    pub fn into_syntax_tree(self) -> SyntaxTree {
        self.syntax_tree
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }
}

impl fmt::Debug for Parse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tree = format!("{:#?}", self.syntax_tree);
        write!(f, "{}", &tree[0..tree.len() - 1])?;

        for error in &self.errors {
            write!(f, "\n{:?}", error)?;
        }

        Ok(())
    }
}
