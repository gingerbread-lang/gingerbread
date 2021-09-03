use crate::lexer::Token;
use crate::syntax::{SyntaxKind, SyntaxNode, UnnamedLang};
use rowan::{GreenNode, GreenNodeBuilder, Language};

pub fn parse<'a>(tokens: impl Iterator<Item = Token<'a>>) -> Parse {
    Parser::new(tokens).parse()
}

struct Parser<'a, I: Iterator<Item = Token<'a>>> {
    tokens: I,
    builder: GreenNodeBuilder<'static>,
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    fn new(tokens: I) -> Self {
        Self { tokens, builder: GreenNodeBuilder::new() }
    }

    fn parse(mut self) -> Parse {
        self.builder.start_node(UnnamedLang::kind_to_raw(SyntaxKind::Root));
        self.builder.start_node(UnnamedLang::kind_to_raw(SyntaxKind::VarRef));

        let token = self.tokens.next().unwrap();
        self.builder.token(UnnamedLang::kind_to_raw(token.kind.into()), token.text);

        self.builder.finish_node();
        self.builder.finish_node();

        Parse { green_node: self.builder.finish() }
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
}
