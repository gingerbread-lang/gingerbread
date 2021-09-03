use crate::parser::Parser;
use syntax::SyntaxKind;
use token::TokenKind;

pub(crate) fn root(p: &mut Parser<'_, '_>) {
    let m = p.start();

    if !p.at_eof() {
        let m = p.start();
        parse_lhs(p);

        if p.at(TokenKind::Plus)
            || p.at(TokenKind::Hyphen)
            || p.at(TokenKind::Asterisk)
            || p.at(TokenKind::Slash)
        {
            p.bump();
            parse_lhs(p);
            m.complete(p, SyntaxKind::BinExpr);
        } else {
            m.abandon(p);
        }
    }

    m.complete(p, SyntaxKind::Root);
}

fn parse_lhs(p: &mut Parser<'_, '_>) {
    if p.at(TokenKind::Ident) {
        parse_var_ref(p);
    } else if p.at(TokenKind::Int) {
        parse_int_literal(p);
    } else {
        panic!();
    }
}

fn parse_var_ref(p: &mut Parser<'_, '_>) {
    assert!(p.at(TokenKind::Ident));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::VarRef);
}

fn parse_int_literal(p: &mut Parser<'_, '_>) {
    assert!(p.at(TokenKind::Int));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::IntLiteral);
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

    #[test]
    fn parse_int_literal() {
        check(
            "123",
            expect![[r#"
Root@0..3
  IntLiteral@0..3
    Int@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_int_literal_addition() {
        check(
            "2+4",
            expect![[r#"
Root@0..3
  BinExpr@0..3
    IntLiteral@0..1
      Int@0..1 "2"
    Plus@1..2 "+"
    IntLiteral@2..3
      Int@2..3 "4""#]],
        );
    }

    #[test]
    fn parse_var_ref_and_int_literal_subtraction() {
        check(
            "len-1",
            expect![[r#"
Root@0..5
  BinExpr@0..5
    VarRef@0..3
      Ident@0..3 "len"
    Hyphen@3..4 "-"
    IntLiteral@4..5
      Int@4..5 "1""#]],
        );
    }

    #[test]
    fn parse_var_ref_multiplication() {
        check(
            "foo * bar",
            expect![[r#"
Root@0..9
  BinExpr@0..9
    VarRef@0..4
      Ident@0..3 "foo"
      Whitespace@3..4 " "
    Asterisk@4..5 "*"
    Whitespace@5..6 " "
    VarRef@6..9
      Ident@6..9 "bar""#]],
        );
    }

    #[test]
    fn parse_int_literal_division() {
        check(
            "22/ 7",
            expect![[r#"
Root@0..5
  BinExpr@0..5
    IntLiteral@0..2
      Int@0..2 "22"
    Slash@2..3 "/"
    Whitespace@3..4 " "
    IntLiteral@4..5
      Int@4..5 "7""#]],
        );
    }
}
