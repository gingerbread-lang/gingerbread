use crate::parser::{CompletedMarker, Parser};
use crate::token_set::TokenSet;
use syntax::SyntaxKind;
use token::TokenKind;

pub(crate) fn root(p: &mut Parser<'_, '_>) {
    let m = p.start();

    loop {
        if p.at_eof() {
            break;
        }

        let _guard = p.expected_syntax_name("statement");
        if p.at(TokenKind::LetKw) {
            parse_var_def(p);
        } else {
            parse_expr(p);
        }
    }

    m.complete(p, SyntaxKind::Root);
}

fn parse_var_def(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::LetKw));
    let m = p.start();
    p.bump();

    {
        let _guard = p.expected_syntax_name("variable name");
        p.expect_with_recovery_set(TokenKind::Ident, TokenSet::new([TokenKind::Eq]));
    }

    p.expect(TokenKind::Eq);
    parse_expr(p);

    m.complete(p, SyntaxKind::VarDef)
}

fn parse_expr(p: &mut Parser<'_, '_>) -> Option<CompletedMarker> {
    parse_expr_with_recovery_set(p, TokenSet::default())
}

fn parse_expr_with_recovery_set(
    p: &mut Parser<'_, '_>,
    recovery_set: TokenSet,
) -> Option<CompletedMarker> {
    parse_expr_bp(p, 0, recovery_set)
}

fn parse_expr_bp(
    p: &mut Parser<'_, '_>,
    min_bp: u8,
    recovery_set: TokenSet,
) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p, recovery_set)?;

    loop {
        let _guard = p.expected_syntax_name("binary operator");
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
        parse_expr_bp(p, right_bp, recovery_set);
        lhs = m.complete(p, SyntaxKind::BinExpr);
    }

    Some(lhs)
}

fn parse_lhs(p: &mut Parser<'_, '_>, recovery_set: TokenSet) -> Option<CompletedMarker> {
    let _guard = p.expected_syntax_name("expression");
    let completed_marker = if p.at(TokenKind::Ident) {
        parse_var_ref(p)
    } else if p.at(TokenKind::Int) {
        parse_int_literal(p)
    } else if p.at(TokenKind::String) {
        parse_string_literal(p)
    } else if p.at(TokenKind::LParen) {
        parse_paren_expr(p)
    } else {
        return p.error_with_recovery_set(recovery_set);
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

fn parse_string_literal(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::String));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringLiteral)
}

fn parse_paren_expr(p: &mut Parser<'_, '_>) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));
    let m = p.start();
    p.bump();

    parse_expr_with_recovery_set(p, TokenSet::new([TokenKind::RParen]));
    p.expect(TokenKind::RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    fn check(input: &str, expect: Expect) {
        let parse = crate::parse(&lexer::lex(input));
        expect.assert_debug_eq(&parse);
    }

    #[test]
    fn parse_nothing() {
        check(
            "",
            expect![[r#"
                Root@0..0
            "#]],
        );
    }

    #[test]
    fn parse_var_ref() {
        check(
            "foo",
            expect![[r#"
                Root@0..3
                  VarRef@0..3
                    Ident@0..3 "foo"
            "#]],
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
                    Whitespace@4..7 "   "
            "#]],
        );
    }

    #[test]
    fn parse_int_literal() {
        check(
            "123",
            expect![[r#"
                Root@0..3
                  IntLiteral@0..3
                    Int@0..3 "123"
            "#]],
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
                      Int@2..3 "4"
            "#]],
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
                      Int@4..5 "1"
            "#]],
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
                      Ident@6..9 "bar"
            "#]],
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
                      Int@4..5 "7"
            "#]],
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
                      Int@4..5 "3"
            "#]],
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
                      Ident@9..11 "x4"
            "#]],
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
                        Int@4..5 "3"
            "#]],
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
                        Int@7..8 "7"
            "#]],
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
                    RParen@2..3 ")"
            "#]],
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
                    RParen@9..10 ")"
            "#]],
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
                      Int@7..8 "2"
            "#]],
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
                error at 4: missing binary operator, RParen
            "#]],
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
                error at 2: missing binary operator, RParen
            "#]],
        );
    }

    #[test]
    fn parse_paren_expr_with_no_contents() {
        check(
            "()",
            expect![[r#"
                Root@0..2
                  ParenExpr@0..2
                    LParen@0..1 "("
                    RParen@1..2 ")"
                error at 1: missing expression
            "#]],
        );
    }

    #[test]
    fn parse_unclosed_paren_expr_recovery_on_var_def() {
        check(
            "(1+1 let foo = bar",
            expect![[r#"
                Root@0..18
                  ParenExpr@0..5
                    LParen@0..1 "("
                    BinExpr@1..5
                      IntLiteral@1..2
                        Int@1..2 "1"
                      Plus@2..3 "+"
                      IntLiteral@3..5
                        Int@3..4 "1"
                        Whitespace@4..5 " "
                  VarDef@5..18
                    LetKw@5..8 "let"
                    Whitespace@8..9 " "
                    Ident@9..12 "foo"
                    Whitespace@12..13 " "
                    Eq@13..14 "="
                    Whitespace@14..15 " "
                    VarRef@15..18
                      Ident@15..18 "bar"
                error at 4: missing binary operator, RParen
            "#]],
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
                error at 2: missing expression
            "#]],
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
                error at 0..2: expected expression, statement but found Error
            "#]],
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
                error at 5..6: expected expression but found Error
            "#]],
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
                error at 5..6: expected expression but found Error
                error at 16..17: expected expression but found Error
            "#]],
        );
    }

    #[test]
    fn parse_var_def() {
        check(
            "let a = b",
            expect![[r#"
                Root@0..9
                  VarDef@0..9
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                    VarRef@8..9
                      Ident@8..9 "b"
            "#]],
        );
    }

    #[test]
    fn parse_2_var_defs() {
        check(
            "let b=c\nlet a=b",
            expect![[r#"
                Root@0..15
                  VarDef@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "b"
                    Eq@5..6 "="
                    VarRef@6..8
                      Ident@6..7 "c"
                      Whitespace@7..8 "\n"
                  VarDef@8..15
                    LetKw@8..11 "let"
                    Whitespace@11..12 " "
                    Ident@12..13 "a"
                    Eq@13..14 "="
                    VarRef@14..15
                      Ident@14..15 "b"
            "#]],
        );
    }

    #[test]
    fn parse_2_exprs() {
        check(
            "2 4 6",
            expect![[r#"
                Root@0..5
                  IntLiteral@0..2
                    Int@0..1 "2"
                    Whitespace@1..2 " "
                  IntLiteral@2..4
                    Int@2..3 "4"
                    Whitespace@3..4 " "
                  IntLiteral@4..5
                    Int@4..5 "6"
            "#]],
        );
    }

    #[test]
    fn parse_var_def_with_missing_name() {
        check(
            "let = 92",
            expect![[r#"
                Root@0..8
                  VarDef@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Eq@4..5 "="
                    Whitespace@5..6 " "
                    IntLiteral@6..8
                      Int@6..8 "92"
                error at 3: missing variable name
            "#]],
        );
    }

    #[test]
    fn parse_var_defs_with_missing_value() {
        check(
            "let foo =\nlet bar = 92",
            expect![[r#"
                Root@0..22
                  VarDef@0..10
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                    Eq@8..9 "="
                    Whitespace@9..10 "\n"
                  VarDef@10..22
                    LetKw@10..13 "let"
                    Whitespace@13..14 " "
                    Ident@14..17 "bar"
                    Whitespace@17..18 " "
                    Eq@18..19 "="
                    Whitespace@19..20 " "
                    IntLiteral@20..22
                      Int@20..22 "92"
                error at 9: missing expression
            "#]],
        );
    }

    #[test]
    fn parse_broken_var_defs() {
        check(
            "let a let",
            expect![[r#"
                Root@0..9
                  VarDef@0..6
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                  VarDef@6..9
                    LetKw@6..9 "let"
                error at 5: missing Eq
                error at 5: missing expression
                error at 9: missing variable name
                error at 9: missing Eq
                error at 9: missing expression
            "#]],
        );
    }

    #[test]
    fn parse_var_def_with_only_let() {
        check(
            "let",
            expect![[r#"
                Root@0..3
                  VarDef@0..3
                    LetKw@0..3 "let"
                error at 3: missing variable name
                error at 3: missing Eq
                error at 3: missing expression
            "#]],
        );
    }

    #[test]
    fn parse_var_defs_first_with_only_let() {
        check(
            "let let a = b",
            expect![[r#"
                Root@0..13
                  VarDef@0..4
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                  VarDef@4..13
                    LetKw@4..7 "let"
                    Whitespace@7..8 " "
                    Ident@8..9 "a"
                    Whitespace@9..10 " "
                    Eq@10..11 "="
                    Whitespace@11..12 " "
                    VarRef@12..13
                      Ident@12..13 "b"
                error at 3: missing variable name
                error at 3: missing Eq
                error at 3: missing expression
            "#]],
        );
    }

    #[test]
    fn parse_string_literal() {
        check(
            "\"Hello, world!\"",
            expect![[r#"
                Root@0..15
                  StringLiteral@0..15
                    String@0..15 "\"Hello, world!\""
            "#]],
        );
    }
}
