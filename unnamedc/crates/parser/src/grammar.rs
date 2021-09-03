use crate::parser::{CompletedMarker, Parser};
use syntax::SyntaxKind;
use token::TokenKind;

pub(crate) fn root(p: &mut Parser<'_, '_>) {
    let m = p.start();

    if !p.at_eof() {
        parse_expr(p);
    }

    m.complete(p, SyntaxKind::Root);
}

fn parse_expr(p: &mut Parser<'_, '_>) -> Option<CompletedMarker> {
    parse_expr_bp(p, 0)
}

fn parse_expr_bp(p: &mut Parser<'_, '_>, min_bp: u8) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p)?;

    loop {
        let (left_bp, right_bp) = if p.at(TokenKind::Plus) || p.at(TokenKind::Hyphen) {
            (1, 2)
        } else if p.at(TokenKind::Asterisk) || p.at(TokenKind::Slash) {
            (3, 4)
        } else {
            break;
        };

        if left_bp < min_bp {
            break;
        }

        p.bump();

        let m = lhs.precede(p);
        parse_expr_bp(p, right_bp);
        lhs = m.complete(p, SyntaxKind::BinExpr);
    }

    Some(lhs)
}

fn parse_lhs(p: &mut Parser<'_, '_>) -> Option<CompletedMarker> {
    let completed_marker = if p.at(TokenKind::Ident) {
        parse_var_ref(p)
    } else if p.at(TokenKind::Int) {
        parse_int_literal(p)
    } else if p.at(TokenKind::LParen) {
        parse_paren_expr(p)
    } else {
        return p.error();
    };

    Some(completed_marker)
}

fn parse_var_ref(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::VarRef)
}

fn parse_int_literal(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::Int));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::IntLiteral)
}

fn parse_paren_expr(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));
    let m = p.start();
    p.bump();

    parse_expr(p);
    p.expect(TokenKind::RParen);

    m.complete(p, SyntaxKind::ParenExpr)
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

    #[test]
    fn parse_two_additions() {
        check(
            "1+2+3",
            expect![[r#"
Root@0..5
  BinExpr@0..5
    BinExpr@0..3
      IntLiteral@0..1
        Int@0..1 "1"
      Plus@1..2 "+"
      IntLiteral@2..3
        Int@2..3 "2"
    Plus@3..4 "+"
    IntLiteral@4..5
      Int@4..5 "3""#]],
        );
    }

    #[test]
    fn parse_four_multiplications() {
        check(
            "x1*x2*x3*x4",
            expect![[r#"
Root@0..11
  BinExpr@0..11
    BinExpr@0..8
      BinExpr@0..5
        VarRef@0..2
          Ident@0..2 "x1"
        Asterisk@2..3 "*"
        VarRef@3..5
          Ident@3..5 "x2"
      Asterisk@5..6 "*"
      VarRef@6..8
        Ident@6..8 "x3"
    Asterisk@8..9 "*"
    VarRef@9..11
      Ident@9..11 "x4""#]],
        );
    }

    #[test]
    fn parse_addition_and_multiplication() {
        check(
            "1+2*3",
            expect![[r#"
Root@0..5
  BinExpr@0..5
    IntLiteral@0..1
      Int@0..1 "1"
    Plus@1..2 "+"
    BinExpr@2..5
      IntLiteral@2..3
        Int@2..3 "2"
      Asterisk@3..4 "*"
      IntLiteral@4..5
        Int@4..5 "3""#]],
        );
    }

    #[test]
    fn parse_division_and_subtraction() {
        check(
            "10/9-8/7",
            expect![[r#"
Root@0..8
  BinExpr@0..8
    BinExpr@0..4
      IntLiteral@0..2
        Int@0..2 "10"
      Slash@2..3 "/"
      IntLiteral@3..4
        Int@3..4 "9"
    Hyphen@4..5 "-"
    BinExpr@5..8
      IntLiteral@5..6
        Int@5..6 "8"
      Slash@6..7 "/"
      IntLiteral@7..8
        Int@7..8 "7""#]],
        );
    }

    #[test]
    fn parse_paren_expr() {
        check(
            "(5)",
            expect![[r#"
Root@0..3
  ParenExpr@0..3
    LParen@0..1 "("
    IntLiteral@1..2
      Int@1..2 "5"
    RParen@2..3 ")""#]],
        );
    }

    #[test]
    fn parse_repeated_paren_expr() {
        check(
            "((((10))))",
            expect![[r#"
Root@0..10
  ParenExpr@0..10
    LParen@0..1 "("
    ParenExpr@1..9
      LParen@1..2 "("
      ParenExpr@2..8
        LParen@2..3 "("
        ParenExpr@3..7
          LParen@3..4 "("
          IntLiteral@4..6
            Int@4..6 "10"
          RParen@6..7 ")"
        RParen@7..8 ")"
      RParen@8..9 ")"
    RParen@9..10 ")""#]],
        );
    }

    #[test]
    fn parse_paren_expr_addition_and_multiplication() {
        check(
            "(42+4)*2",
            expect![[r#"
Root@0..8
  BinExpr@0..8
    ParenExpr@0..6
      LParen@0..1 "("
      BinExpr@1..5
        IntLiteral@1..3
          Int@1..3 "42"
        Plus@3..4 "+"
        IntLiteral@4..5
          Int@4..5 "4"
      RParen@5..6 ")"
    Asterisk@6..7 "*"
    IntLiteral@7..8
      Int@7..8 "2""#]],
        );
    }

    #[test]
    fn parse_unclosed_paren_expr() {
        check(
            "(foo",
            expect![[r#"
Root@0..4
  ParenExpr@0..4
    LParen@0..1 "("
    VarRef@1..4
      Ident@1..4 "foo"
error at 1..4: expected `+`, `-`, `*`, `/` or `)`"#]],
        );
    }

    #[test]
    fn parse_unclosed_paren_expr_with_whitespace() {
        check(
            "(a ",
            expect![[r#"
Root@0..3
  ParenExpr@0..3
    LParen@0..1 "("
    VarRef@1..3
      Ident@1..2 "a"
      Whitespace@2..3 " "
error at 1..2: expected `+`, `-`, `*`, `/` or `)`"#]],
        );
    }

    #[test]
    fn parse_bin_expr_without_rhs() {
        check(
            "1+",
            expect![[r#"
Root@0..2
  BinExpr@0..2
    IntLiteral@0..1
      Int@0..1 "1"
    Plus@1..2 "+"
error at 1..2: expected identifier, integer literal or `(`"#]],
        );
    }

    #[test]
    fn parse_bin_expr_with_broken_lhs() {
        check(
            "å*5",
            expect![[r#"
Root@0..4
  BinExpr@0..4
    Error@0..2
      Error@0..2 "å"
    Asterisk@2..3 "*"
    IntLiteral@3..4
      Int@3..4 "5"
error at 0..2: expected identifier, integer literal or `(` but found an unrecognized token"#]],
        );
    }

    #[test]
    fn parse_bin_expr_with_broken_rhs() {
        check(
            "10 - %",
            expect![[r#"
Root@0..6
  BinExpr@0..6
    IntLiteral@0..3
      Int@0..2 "10"
      Whitespace@2..3 " "
    Hyphen@3..4 "-"
    Whitespace@4..5 " "
    Error@5..6
      Error@5..6 "%"
error at 5..6: expected identifier, integer literal or `(` but found an unrecognized token"#]],
        );
    }

    #[test]
    fn parse_nested_bin_expr_with_broken_lhs_and_rhs() {
        check(
            "5 * ($ - foo) / ?",
            expect![[r#"
Root@0..17
  BinExpr@0..17
    BinExpr@0..14
      IntLiteral@0..2
        Int@0..1 "5"
        Whitespace@1..2 " "
      Asterisk@2..3 "*"
      Whitespace@3..4 " "
      ParenExpr@4..14
        LParen@4..5 "("
        BinExpr@5..12
          Error@5..7
            Error@5..6 "$"
            Whitespace@6..7 " "
          Hyphen@7..8 "-"
          Whitespace@8..9 " "
          VarRef@9..12
            Ident@9..12 "foo"
        RParen@12..13 ")"
        Whitespace@13..14 " "
    Slash@14..15 "/"
    Whitespace@15..16 " "
    Error@16..17
      Error@16..17 "?"
error at 5..6: expected identifier, integer literal or `(` but found an unrecognized token
error at 16..17: expected identifier, integer literal or `(` but found an unrecognized token"#]],
        );
    }
}
