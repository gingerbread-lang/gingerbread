mod event;
mod marker;
mod sink;

use self::event::Event;
use self::marker::Marker;
use self::sink::Sink;
use crate::lexer::Token;
use crate::syntax::{SyntaxKind, SyntaxNode};
use rowan::GreenNode;

pub fn parse<'a>(tokens: impl Iterator<Item = Token<'a>>) -> Parse {
    let tokens: Vec<_> = tokens.collect();
    let events = Parser::new(&tokens).parse();

    Sink::new(events, tokens).finish()
}

struct Parser<'tokens, 'input> {
    tokens: &'tokens [Token<'input>],
    events: Vec<Event>,
}

impl<'tokens, 'input> Parser<'tokens, 'input> {
    fn new(tokens: &'tokens [Token<'input>]) -> Self {
        Self { tokens, events: Vec::new() }
    }

    fn parse(mut self) -> Vec<Event> {
        self.events.push(Event::StartNode { kind: SyntaxKind::Root });
        self.events.push(Event::StartNode { kind: SyntaxKind::VarRef });
        self.events.push(Event::Token);
        self.events.push(Event::FinishNode);
        self.events.push(Event::FinishNode);

        self.events
    }

    fn start(&self) -> Marker {
        Marker::new(self.events.len())
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
