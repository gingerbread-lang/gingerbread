mod event;
mod marker;
mod sink;

use self::event::Event;
use self::marker::Marker;
use self::sink::Sink;
use crate::lexer::{Token, TokenKind};
use crate::syntax::{SyntaxKind, SyntaxNode};
use rowan::GreenNode;

pub fn parse<'a>(tokens: impl Iterator<Item = Token<'a>>) -> Parse {
    let tokens: Vec<_> = tokens.collect();
    let events = Parser::new(&tokens).parse();

    Sink::new(events, tokens).finish()
}

struct Parser<'tokens, 'input> {
    tokens: &'tokens [Token<'input>],
    token_idx: usize,
    events: Vec<Event>,
}

impl<'tokens, 'input> Parser<'tokens, 'input> {
    fn new(tokens: &'tokens [Token<'input>]) -> Self {
        Self { tokens, token_idx: 0, events: Vec::new() }
    }

    fn parse(mut self) -> Vec<Event> {
        let root_m = self.start();

        if self.at(TokenKind::Ident) {
            self.parse_var_ref();
        }

        root_m.complete(&mut self, SyntaxKind::Root);

        self.events
    }

    fn parse_var_ref(&mut self) {
        assert!(self.at(TokenKind::Ident));

        let m = self.start();
        self.add_token();

        m.complete(self, SyntaxKind::VarRef);
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.skip_whitespace();

        self.at_raw(kind)
    }

    fn skip_whitespace(&mut self) {
        if self.at_raw(TokenKind::Whitespace) {
            self.token_idx += 1;
        }
    }

    fn at_raw(&self, kind: TokenKind) -> bool {
        self.tokens.get(self.token_idx).map_or(false, |token| token.kind == kind)
    }

    fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    fn add_token(&mut self) {
        self.events.push(Event::AddToken);
        self.token_idx += 1;
    }
}

pub struct Parse {
    green_node: GreenNode,
}

impl Parse {
    fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    fn debug_syntax_tree(&self) -> String {
        let mut tree = format!("{:#?}", self.syntax_node());
        tree.remove(tree.len() - 1);

        tree
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex;
    use expect_test::{expect, Expect};

    fn check(input: &str, expect: Expect) {
        let parse = parse(lex(input));
        expect.assert_eq(&parse.debug_syntax_tree());
    }

    #[test]
    fn parse_nothing() {
        check("", expect![["Root@0..0"]]);
    }

    #[test]
    fn parse_var_ref() {
        check(
            "foo",
            expect![[r#"
Root@0..3
  VarRef@0..3
    Ident@0..3 "foo""#]],
        );
    }

    #[test]
    fn parse_var_ref_with_whitespace() {
        check(
            " foo   ",
            expect![[r#"
Root@0..7
  Whitespace@0..1 " "
  VarRef@1..7
    Ident@1..4 "foo"
    Whitespace@4..7 "   ""#]],
        );
    }
}
