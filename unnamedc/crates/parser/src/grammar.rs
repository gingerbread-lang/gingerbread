use crate::parser::Parser;
use syntax::SyntaxKind;
use token::TokenKind;

pub(crate) fn root(p: &mut Parser<'_, '_>) {
    let m = p.start();

    if p.at(TokenKind::Ident) {
        parse_var_ref(p);
    }

    m.complete(p, SyntaxKind::Root);
}

fn parse_var_ref(p: &mut Parser<'_, '_>) {
    assert!(p.at(TokenKind::Ident));

    let m = p.start();
    p.bump();

    m.complete(p, SyntaxKind::VarRef);
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    fn check(input: &str, expect: Expect) {
        let tokens = lexer::lex(input);
        let parse = crate::parse(tokens);
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
