mod expr;
mod stmt;
mod ty;

use crate::parser::Parser;
use syntax::SyntaxKind;

pub(crate) fn root(p: &mut Parser<'_, '_>) {
    let m = p.start();

    loop {
        if p.at_eof() {
            break;
        }

        self::stmt::parse_stmt(p);
    }

    m.complete(p, SyntaxKind::Root);
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
                error at 4: missing RParen
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
                error at 2: missing RParen
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
                error at 1: missing inner expression
            "#]],
        );
    }

    #[test]
    fn parse_unclosed_paren_expr_recovery_on_local_def() {
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
                  LocalDef@5..18
                    LetKw@5..8 "let"
                    Whitespace@8..9 " "
                    Ident@9..12 "foo"
                    Whitespace@12..13 " "
                    Eq@13..14 "="
                    Whitespace@14..15 " "
                    VarRef@15..18
                      Ident@15..18 "bar"
                error at 4: missing RParen
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
                error at 2: missing operand
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
                error at 0..2: expected statement but found Error
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
                error at 5..6: expected operand but found Error
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
                error at 5..6: expected inner expression but found Error
                error at 16..17: expected operand but found Error
            "#]],
        );
    }

    #[test]
    fn parse_local_def() {
        check(
            "let a = b",
            expect![[r#"
                Root@0..9
                  LocalDef@0..9
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
    fn parse_2_local_defs() {
        check(
            "let b=c\nlet a=b",
            expect![[r#"
                Root@0..15
                  LocalDef@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "b"
                    Eq@5..6 "="
                    VarRef@6..8
                      Ident@6..7 "c"
                      Whitespace@7..8 "\n"
                  LocalDef@8..15
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
    fn parse_local_def_with_missing_name() {
        check(
            "let = 92",
            expect![[r#"
                Root@0..8
                  LocalDef@0..8
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
    fn parse_local_defs_with_missing_value() {
        check(
            "let foo =\nlet bar = 92",
            expect![[r#"
                Root@0..22
                  LocalDef@0..10
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                    Eq@8..9 "="
                    Whitespace@9..10 "\n"
                  LocalDef@10..22
                    LetKw@10..13 "let"
                    Whitespace@13..14 " "
                    Ident@14..17 "bar"
                    Whitespace@17..18 " "
                    Eq@18..19 "="
                    Whitespace@19..20 " "
                    IntLiteral@20..22
                      Int@20..22 "92"
                error at 9: missing variable value
            "#]],
        );
    }

    #[test]
    fn parse_broken_local_defs() {
        check(
            "let a let",
            expect![[r#"
                Root@0..9
                  LocalDef@0..6
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                  LocalDef@6..9
                    LetKw@6..9 "let"
                error at 5: missing Eq
                error at 5: missing variable value
                error at 9: missing variable name
                error at 9: missing Eq
                error at 9: missing variable value
            "#]],
        );
    }

    #[test]
    fn parse_local_def_with_only_let() {
        check(
            "let",
            expect![[r#"
                Root@0..3
                  LocalDef@0..3
                    LetKw@0..3 "let"
                error at 3: missing variable name
                error at 3: missing Eq
                error at 3: missing variable value
            "#]],
        );
    }

    #[test]
    fn parse_local_defs_first_with_only_let() {
        check(
            "let let a = b",
            expect![[r#"
                Root@0..13
                  LocalDef@0..4
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                  LocalDef@4..13
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
                error at 3: missing variable value
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

    #[test]
    fn parse_empty_block() {
        check(
            "{}",
            expect![[r#"
                Root@0..2
                  Block@0..2
                    LBrace@0..1 "{"
                    RBrace@1..2 "}"
            "#]],
        );
    }

    #[test]
    fn parse_block_with_local_def() {
        check(
            "{ let zero = 0 }",
            expect![[r#"
                Root@0..16
                  Block@0..16
                    LBrace@0..1 "{"
                    Whitespace@1..2 " "
                    LocalDef@2..15
                      LetKw@2..5 "let"
                      Whitespace@5..6 " "
                      Ident@6..10 "zero"
                      Whitespace@10..11 " "
                      Eq@11..12 "="
                      Whitespace@12..13 " "
                      IntLiteral@13..15
                        Int@13..14 "0"
                        Whitespace@14..15 " "
                    RBrace@15..16 "}"
            "#]],
        );
    }

    #[test]
    fn parse_block_with_stmts() {
        check(
            "{let a=100 let b=a*10 b-1}",
            expect![[r#"
                Root@0..26
                  Block@0..26
                    LBrace@0..1 "{"
                    LocalDef@1..11
                      LetKw@1..4 "let"
                      Whitespace@4..5 " "
                      Ident@5..6 "a"
                      Eq@6..7 "="
                      IntLiteral@7..11
                        Int@7..10 "100"
                        Whitespace@10..11 " "
                    LocalDef@11..22
                      LetKw@11..14 "let"
                      Whitespace@14..15 " "
                      Ident@15..16 "b"
                      Eq@16..17 "="
                      BinExpr@17..22
                        VarRef@17..18
                          Ident@17..18 "a"
                        Asterisk@18..19 "*"
                        IntLiteral@19..22
                          Int@19..21 "10"
                          Whitespace@21..22 " "
                    BinExpr@22..25
                      VarRef@22..23
                        Ident@22..23 "b"
                      Hyphen@23..24 "-"
                      IntLiteral@24..25
                        Int@24..25 "1"
                    RBrace@25..26 "}"
            "#]],
        );
    }

    #[test]
    fn parse_missing_operand_in_block() {
        check(
            "{1+}",
            expect![[r#"
                Root@0..4
                  Block@0..4
                    LBrace@0..1 "{"
                    BinExpr@1..3
                      IntLiteral@1..2
                        Int@1..2 "1"
                      Plus@2..3 "+"
                    RBrace@3..4 "}"
                error at 3: missing operand
            "#]],
        );
    }

    #[test]
    fn parse_block_with_unclosed_brace() {
        check(
            "{5",
            expect![[r#"
                Root@0..2
                  Block@0..2
                    LBrace@0..1 "{"
                    IntLiteral@1..2
                      Int@1..2 "5"
                error at 2: missing RBrace
            "#]],
        );
    }

    #[test]
    fn parse_nested_block() {
        check(
            "{ let foo={let bar=23 bar*2} foo*2 }",
            expect![[r#"
                Root@0..36
                  Block@0..36
                    LBrace@0..1 "{"
                    Whitespace@1..2 " "
                    LocalDef@2..29
                      LetKw@2..5 "let"
                      Whitespace@5..6 " "
                      Ident@6..9 "foo"
                      Eq@9..10 "="
                      Block@10..29
                        LBrace@10..11 "{"
                        LocalDef@11..22
                          LetKw@11..14 "let"
                          Whitespace@14..15 " "
                          Ident@15..18 "bar"
                          Eq@18..19 "="
                          IntLiteral@19..22
                            Int@19..21 "23"
                            Whitespace@21..22 " "
                        BinExpr@22..27
                          VarRef@22..25
                            Ident@22..25 "bar"
                          Asterisk@25..26 "*"
                          IntLiteral@26..27
                            Int@26..27 "2"
                        RBrace@27..28 "}"
                        Whitespace@28..29 " "
                    BinExpr@29..35
                      VarRef@29..32
                        Ident@29..32 "foo"
                      Asterisk@32..33 "*"
                      IntLiteral@33..35
                        Int@33..34 "2"
                        Whitespace@34..35 " "
                    RBrace@35..36 "}"
            "#]],
        );
    }

    #[test]
    fn parse_block_closed_by_wrong_delimiter() {
        check(
            "{a)",
            expect![[r#"
                Root@0..3
                  Block@0..3
                    LBrace@0..1 "{"
                    VarRef@1..2
                      Ident@1..2 "a"
                    Error@2..3
                      RParen@2..3 ")"
                error at 2..3: expected RBrace but found RParen
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def() {
        check(
            "fnc add(x: s32, y: s32): s32 -> x + y",
            expect![[r#"
                Root@0..37
                  FncDef@0..37
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..7 "add"
                    ParamList@7..23
                      LParen@7..8 "("
                      Param@8..14
                        Ident@8..9 "x"
                        Colon@9..10 ":"
                        Whitespace@10..11 " "
                        Ty@11..14
                          Ident@11..14 "s32"
                      Comma@14..15 ","
                      Whitespace@15..16 " "
                      Param@16..22
                        Ident@16..17 "y"
                        Colon@17..18 ":"
                        Whitespace@18..19 " "
                        Ty@19..22
                          Ident@19..22 "s32"
                      RParen@22..23 ")"
                    RetTy@23..29
                      Colon@23..24 ":"
                      Whitespace@24..25 " "
                      Ty@25..29
                        Ident@25..28 "s32"
                        Whitespace@28..29 " "
                    Arrow@29..31 "->"
                    Whitespace@31..32 " "
                    BinExpr@32..37
                      VarRef@32..34
                        Ident@32..33 "x"
                        Whitespace@33..34 " "
                      Plus@34..35 "+"
                      Whitespace@35..36 " "
                      VarRef@36..37
                        Ident@36..37 "y"
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_without_ret_ty() {
        check(
            "fnc nothing() -> {}",
            expect![[r#"
                Root@0..19
                  FncDef@0..19
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..11 "nothing"
                    ParamList@11..14
                      LParen@11..12 "("
                      RParen@12..13 ")"
                      Whitespace@13..14 " "
                    Arrow@14..16 "->"
                    Whitespace@16..17 " "
                    Block@17..19
                      LBrace@17..18 "{"
                      RBrace@18..19 "}"
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_trailing_comma_in_param_list() {
        check(
            "fnc drop(n: s32,) -> {}",
            expect![[r#"
                Root@0..23
                  FncDef@0..23
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..8 "drop"
                    ParamList@8..18
                      LParen@8..9 "("
                      Param@9..15
                        Ident@9..10 "n"
                        Colon@10..11 ":"
                        Whitespace@11..12 " "
                        Ty@12..15
                          Ident@12..15 "s32"
                      Comma@15..16 ","
                      RParen@16..17 ")"
                      Whitespace@17..18 " "
                    Arrow@18..20 "->"
                    Whitespace@20..21 " "
                    Block@21..23
                      LBrace@21..22 "{"
                      RBrace@22..23 "}"
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_missing_param_name() {
        check(
            "fnc id(: foo) -> {}",
            expect![[r#"
                Root@0..19
                  FncDef@0..19
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..6 "id"
                    ParamList@6..14
                      LParen@6..7 "("
                      Param@7..12
                        Colon@7..8 ":"
                        Whitespace@8..9 " "
                        Ty@9..12
                          Ident@9..12 "foo"
                      RParen@12..13 ")"
                      Whitespace@13..14 " "
                    Arrow@14..16 "->"
                    Whitespace@16..17 " "
                    Block@17..19
                      LBrace@17..18 "{"
                      RBrace@18..19 "}"
                error at 7: missing parameter name
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_missing_arrow() {
        check(
            "fnc five(): s32 5",
            expect![[r#"
                Root@0..17
                  FncDef@0..17
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..8 "five"
                    ParamList@8..10
                      LParen@8..9 "("
                      RParen@9..10 ")"
                    RetTy@10..16
                      Colon@10..11 ":"
                      Whitespace@11..12 " "
                      Ty@12..16
                        Ident@12..15 "s32"
                        Whitespace@15..16 " "
                    IntLiteral@16..17
                      Int@16..17 "5"
                error at 15: missing Arrow
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_missing_name() {
        check(
            "fnc (x: s32) -> x",
            expect![[r#"
                Root@0..17
                  FncDef@0..17
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    ParamList@4..13
                      LParen@4..5 "("
                      Param@5..11
                        Ident@5..6 "x"
                        Colon@6..7 ":"
                        Whitespace@7..8 " "
                        Ty@8..11
                          Ident@8..11 "s32"
                      RParen@11..12 ")"
                      Whitespace@12..13 " "
                    Arrow@13..15 "->"
                    Whitespace@15..16 " "
                    VarRef@16..17
                      Ident@16..17 "x"
                error at 3: missing function name
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_missing_param_list() {
        check(
            "fnc ten -> 10",
            expect![[r#"
                Root@0..13
                  FncDef@0..13
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..7 "ten"
                    Whitespace@7..8 " "
                    Arrow@8..10 "->"
                    Whitespace@10..11 " "
                    IntLiteral@11..13
                      Int@11..13 "10"
                error at 7: missing function parameters
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_only_keyword() {
        check(
            "fnc",
            expect![[r#"
                Root@0..3
                  FncDef@0..3
                    FncKw@0..3 "fnc"
                error at 3: missing function name
                error at 3: missing function parameters
                error at 3: missing Arrow
                error at 3: missing function body
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_missing_closing_paren_on_param_list() {
        check(
            "fnc main(",
            expect![[r#"
                Root@0..9
                  FncDef@0..9
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..8 "main"
                    ParamList@8..9
                      LParen@8..9 "("
                error at 9: missing RParen
                error at 9: missing Arrow
                error at 9: missing function body
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_recovery_on_arrow_with_unclosed_param_list() {
        check(
            "fnc foo(bar: s32 -> {}",
            expect![[r#"
                Root@0..22
                  FncDef@0..22
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    ParamList@7..17
                      LParen@7..8 "("
                      Param@8..17
                        Ident@8..11 "bar"
                        Colon@11..12 ":"
                        Whitespace@12..13 " "
                        Ty@13..17
                          Ident@13..16 "s32"
                          Whitespace@16..17 " "
                    Arrow@17..19 "->"
                    Whitespace@19..20 " "
                    Block@20..22
                      LBrace@20..21 "{"
                      RBrace@21..22 "}"
                error at 16: missing RParen
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_missing_ret_ty_name() {
        check(
            "fnc five(): -> 5",
            expect![[r#"
                Root@0..16
                  FncDef@0..16
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..8 "five"
                    ParamList@8..10
                      LParen@8..9 "("
                      RParen@9..10 ")"
                    RetTy@10..12
                      Colon@10..11 ":"
                      Whitespace@11..12 " "
                      Ty@12..12
                    Arrow@12..14 "->"
                    Whitespace@14..15 " "
                    IntLiteral@15..16
                      Int@15..16 "5"
                error at 11: missing return type
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_with_missing_ret_ty_name_and_arrow() {
        check(
            "fnc gen(): { let foo = 5\nfoo }",
            expect![[r#"
                Root@0..30
                  FncDef@0..30
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..7 "gen"
                    ParamList@7..9
                      LParen@7..8 "("
                      RParen@8..9 ")"
                    RetTy@9..11
                      Colon@9..10 ":"
                      Whitespace@10..11 " "
                      Ty@11..11
                    Block@11..30
                      LBrace@11..12 "{"
                      Whitespace@12..13 " "
                      LocalDef@13..25
                        LetKw@13..16 "let"
                        Whitespace@16..17 " "
                        Ident@17..20 "foo"
                        Whitespace@20..21 " "
                        Eq@21..22 "="
                        Whitespace@22..23 " "
                        IntLiteral@23..25
                          Int@23..24 "5"
                          Whitespace@24..25 "\n"
                      VarRef@25..29
                        Ident@25..28 "foo"
                        Whitespace@28..29 " "
                      RBrace@29..30 "}"
                error at 10: missing return type
                error at 10: missing Arrow
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_missing_everything_with_brace() {
        check(
            "fnc {",
            expect![[r#"
                Root@0..5
                  FncDef@0..5
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Block@4..5
                      LBrace@4..5 "{"
                error at 3: missing function name
                error at 3: missing function parameters
                error at 3: missing Arrow
                error at 5: missing RBrace
            "#]],
        );
    }

    #[test]
    fn parse_fnc_def_interrupted_by_local_def() {
        check(
            "fnc foo let bar = 5",
            expect![[r#"
                Root@0..19
                  FncDef@0..8
                    FncKw@0..3 "fnc"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                  LocalDef@8..19
                    LetKw@8..11 "let"
                    Whitespace@11..12 " "
                    Ident@12..15 "bar"
                    Whitespace@15..16 " "
                    Eq@16..17 "="
                    Whitespace@17..18 " "
                    IntLiteral@18..19
                      Int@18..19 "5"
                error at 7: missing function parameters
                error at 7: missing Arrow
                error at 7: missing function body
            "#]],
        );
    }

    #[test]
    fn parse_local_def_interrupted_by_fnc_def() {
        check(
            "let a = fnc one() -> 1",
            expect![[r#"
                Root@0..22
                  LocalDef@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                  FncDef@8..22
                    FncKw@8..11 "fnc"
                    Whitespace@11..12 " "
                    Ident@12..15 "one"
                    ParamList@15..18
                      LParen@15..16 "("
                      RParen@16..17 ")"
                      Whitespace@17..18 " "
                    Arrow@18..20 "->"
                    Whitespace@20..21 " "
                    IntLiteral@21..22
                      Int@21..22 "1"
                error at 7: missing variable value
            "#]],
        );
    }

    #[test]
    fn parse_extra_right_braces() {
        check(
            "{}}",
            expect![[r#"
                Root@0..3
                  Block@0..2
                    LBrace@0..1 "{"
                    RBrace@1..2 "}"
                  Error@2..3
                    RBrace@2..3 "}"
                error at 2..3: expected statement but found RBrace
            "#]],
        );
    }
}
