use crate::{Index, Name};
use arena::{Arena, Id};
use ast::AstToken;
use std::collections::HashMap;
use std::fmt;
use text_size::TextRange;

pub struct Bodies {
    local_defs: Arena<LocalDef>,
    statements: Arena<Statement>,
    exprs: Arena<Expr>,
    function_bodies: HashMap<Name, Id<Expr>>,
}

#[derive(Debug)]
pub enum Expr {
    Missing,
    IntLiteral(u32),
    StringLiteral(String),
    Binary { lhs: Id<Expr>, rhs: Id<Expr>, operator: BinaryOperator },
    Block { statements: Vec<Id<Statement>>, tail_expr: Option<Id<Expr>> },
    Local(Id<LocalDef>),
    Param { idx: u32 },
    Call { name: Name, args: Vec<Id<Expr>> },
}

#[derive(Debug)]
pub enum Statement {
    Expr(Id<Expr>),
    LocalDef(Id<LocalDef>),
}

#[derive(Debug)]
pub struct LocalDef {
    pub value: Id<Expr>,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoweringDiagnostic {
    pub kind: LoweringDiagnosticKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoweringDiagnosticKind {
    UndefinedLocal { name: String },
    MismatchedArgCount { name: String, expected: u32, got: u32 },
    CalledLocal { name: String },
}

pub fn lower(root: &ast::Root, index: &Index) -> (Bodies, Vec<LoweringDiagnostic>) {
    let mut ctx = Ctx::new(index);

    for def in root.defs() {
        match def {
            ast::Def::Function(function) => ctx.lower_function(function),
        }
    }

    (ctx.bodies, ctx.diagnostics)
}

struct Ctx<'a> {
    bodies: Bodies,
    index: &'a Index,
    diagnostics: Vec<LoweringDiagnostic>,
    scopes: Vec<HashMap<String, Id<LocalDef>>>,
    params: HashMap<String, u32>,
}

impl<'a> Ctx<'a> {
    fn new(index: &'a Index) -> Self {
        Self {
            bodies: Bodies {
                local_defs: Arena::new(),
                statements: Arena::new(),
                exprs: Arena::new(),
                function_bodies: HashMap::new(),
            },
            index,
            diagnostics: Vec::new(),
            scopes: vec![HashMap::new()],
            params: HashMap::new(),
        }
    }

    fn lower_function(&mut self, function: ast::Function) {
        let name = match function.name() {
            Some(ident) => Name(ident.text().to_string()),
            None => return,
        };

        if let Some(param_list) = function.param_list() {
            for (idx, param) in param_list.params().enumerate() {
                if let Some(ident) = param.name() {
                    self.params.insert(ident.text().to_string(), idx as u32);
                }
            }
        }

        let expr = self.lower_expr(function.body());
        let id = self.bodies.exprs.alloc(expr);

        self.params.clear();

        self.bodies.function_bodies.insert(name, id);
    }

    fn lower_statement(&mut self, statement: ast::Statement) -> Statement {
        match statement {
            ast::Statement::LocalDef(local_def) => self.lower_local_def(local_def),
            ast::Statement::ExprStatement(expr_statement) => {
                let expr = self.lower_expr(expr_statement.expr());
                Statement::Expr(self.bodies.exprs.alloc(expr))
            }
        }
    }

    fn lower_local_def(&mut self, local_def: ast::LocalDef) -> Statement {
        let value = self.lower_expr(local_def.value());
        let value = self.bodies.exprs.alloc(value);
        let id = self.bodies.local_defs.alloc(LocalDef { value });

        if let Some(ident) = local_def.name() {
            self.insert_into_current_scope(ident.text().to_string(), id);
        }

        Statement::LocalDef(id)
    }

    fn lower_expr(&mut self, expr: Option<ast::Expr>) -> Expr {
        let expr = match expr {
            Some(expr) => expr,
            None => return Expr::Missing,
        };

        match expr {
            ast::Expr::Binary(binary_expr) => self.lower_binary_expr(binary_expr),
            ast::Expr::Block(block) => self.lower_block(block),
            ast::Expr::Call(call) => self.lower_call(call),
            ast::Expr::IntLiteral(int_literal) => self.lower_int_literal(int_literal),
            ast::Expr::StringLiteral(string_literal) => self.lower_string_literal(string_literal),
        }
    }

    fn lower_binary_expr(&mut self, binary_expr: ast::BinaryExpr) -> Expr {
        let lhs = self.lower_expr(binary_expr.lhs());
        let rhs = self.lower_expr(binary_expr.rhs());

        let operator = match binary_expr.operator() {
            Some(ast::BinaryOperator::Add(_)) => BinaryOperator::Add,
            Some(ast::BinaryOperator::Sub(_)) => BinaryOperator::Sub,
            Some(ast::BinaryOperator::Mul(_)) => BinaryOperator::Mul,
            Some(ast::BinaryOperator::Div(_)) => BinaryOperator::Div,
            None => return Expr::Missing,
        };

        Expr::Binary {
            lhs: self.bodies.exprs.alloc(lhs),
            rhs: self.bodies.exprs.alloc(rhs),
            operator,
        }
    }

    fn lower_block(&mut self, block: ast::Block) -> Expr {
        self.create_new_child_scope();

        let mut statements = Vec::new();

        for statement in block.statements() {
            let statement = self.lower_statement(statement);
            statements.push(self.bodies.statements.alloc(statement));
        }

        let tail_expr = block.tail_expr().map(|tail_expr| {
            let expr = self.lower_expr(Some(tail_expr));
            self.bodies.exprs.alloc(expr)
        });

        self.destroy_current_scope();

        Expr::Block { statements, tail_expr }
    }

    fn lower_call(&mut self, call: ast::Call) -> Expr {
        let ident = match call.name() {
            Some(ident) => ident,
            None => return Expr::Missing,
        };

        let name = ident.text();

        if let Some(idx) = self.look_up_param(name) {
            check_args_for_local(&call, &ident, name, &mut self.diagnostics);
            return Expr::Param { idx };
        }

        if let Some(def) = self.look_up_in_current_scope(name) {
            check_args_for_local(&call, &ident, name, &mut self.diagnostics);
            return Expr::Local(def);
        }

        let name = Name(name.to_string());

        if let Some(function) = self.index.get_function(&name) {
            let arg_list = match call.arg_list() {
                Some(arg_list) => arg_list,
                None => return Expr::Call { name, args: Vec::new() },
            };

            let expected = function.params.len() as u32;
            let got = arg_list.args().count() as u32;

            if expected != got {
                self.diagnostics.push(LoweringDiagnostic {
                    kind: LoweringDiagnosticKind::MismatchedArgCount {
                        name: name.0,
                        expected,
                        got,
                    },
                    range: ident.range(),
                });

                return Expr::Missing;
            }

            let mut args = Vec::new();

            for arg in arg_list.args() {
                let expr = self.lower_expr(arg.value());
                args.push(self.bodies.exprs.alloc(expr));
            }

            return Expr::Call { name, args };
        }

        self.diagnostics.push(LoweringDiagnostic {
            kind: LoweringDiagnosticKind::UndefinedLocal { name: ident.text().to_string() },
            range: ident.range(),
        });

        return Expr::Missing;

        fn check_args_for_local(
            call: &ast::Call,
            ident: &ast::Ident,
            name: &str,
            diagnostics: &mut Vec<LoweringDiagnostic>,
        ) {
            if let Some(arg_list) = call.arg_list() {
                if arg_list.args().count() != 0 {
                    diagnostics.push(LoweringDiagnostic {
                        kind: LoweringDiagnosticKind::CalledLocal { name: name.to_string() },
                        range: ident.range(),
                    });
                }
            }
        }
    }

    fn lower_int_literal(&self, int_literal: ast::IntLiteral) -> Expr {
        int_literal
            .value()
            .and_then(|int| int.text().parse().ok())
            .map_or(Expr::Missing, Expr::IntLiteral)
    }

    fn lower_string_literal(&self, string_literal: ast::StringLiteral) -> Expr {
        match string_literal.value() {
            Some(string) => {
                let text = string.text();

                // trim off quotes
                Expr::StringLiteral(text[1..text.len() - 1].to_string())
            }
            None => Expr::Missing,
        }
    }

    fn insert_into_current_scope(&mut self, name: String, id: Id<LocalDef>) {
        self.current_scope().insert(name, id);
    }

    fn look_up_in_current_scope(&mut self, name: &str) -> Option<Id<LocalDef>> {
        self.current_scope().get(name).copied()
    }

    fn look_up_param(&mut self, name: &str) -> Option<u32> {
        self.params.get(name).copied()
    }

    fn create_new_child_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn destroy_current_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope(&mut self) -> &mut HashMap<String, Id<LocalDef>> {
        let len = self.scopes.len();
        &mut self.scopes[len - 1]
    }
}

impl Bodies {
    pub fn function_body(&self, name: &Name) -> Id<Expr> {
        self.function_bodies[name]
    }
}

impl std::ops::Index<Id<LocalDef>> for Bodies {
    type Output = LocalDef;

    fn index(&self, id: Id<LocalDef>) -> &Self::Output {
        &self.local_defs[id]
    }
}

impl std::ops::Index<Id<Statement>> for Bodies {
    type Output = Statement;

    fn index(&self, id: Id<Statement>) -> &Self::Output {
        &self.statements[id]
    }
}

impl std::ops::Index<Id<Expr>> for Bodies {
    type Output = Expr;

    fn index(&self, id: Id<Expr>) -> &Self::Output {
        &self.exprs[id]
    }
}

impl fmt::Debug for Bodies {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut function_bodies: Vec<_> = self.function_bodies.iter().collect();
        function_bodies.sort_unstable_by_key(|(name, _)| &name.0);

        for (name, expr_id) in function_bodies {
            write!(f, "fnc {} -> ", name.0)?;
            write_expr(*expr_id, self, f, 0)?;
            writeln!(f, ";")?;
        }

        return Ok(());

        fn write_expr(
            id: Id<Expr>,
            bodies: &Bodies,
            f: &mut fmt::Formatter<'_>,
            mut indentation: usize,
        ) -> fmt::Result {
            match &bodies[id] {
                Expr::Missing => write!(f, "<missing>")?,

                Expr::IntLiteral(n) => write!(f, "{}", n)?,

                Expr::StringLiteral(s) => write!(f, "\"{}\"", s)?,

                Expr::Binary { lhs, rhs, operator } => {
                    write_expr(*lhs, bodies, f, indentation)?;

                    write!(f, " ")?;

                    match operator {
                        BinaryOperator::Add => write!(f, "+")?,
                        BinaryOperator::Sub => write!(f, "-")?,
                        BinaryOperator::Mul => write!(f, "*")?,
                        BinaryOperator::Div => write!(f, "/")?,
                    }

                    write!(f, " ")?;

                    write_expr(*rhs, bodies, f, indentation)?;
                }

                Expr::Block { statements, tail_expr: None } if statements.is_empty() => {
                    write!(f, "{{}}")?;
                }

                Expr::Block { statements, tail_expr: Some(tail_expr) } if statements.is_empty() => {
                    write!(f, "{{ ")?;
                    write_expr(*tail_expr, bodies, f, indentation + 4)?;
                    write!(f, " }}")?;
                }

                Expr::Block { statements, tail_expr } => {
                    indentation += 4;

                    writeln!(f, "{{")?;

                    for statement in statements.clone() {
                        write!(f, "{}", " ".repeat(indentation))?;
                        write_statement(statement, bodies, f, indentation)?;
                        writeln!(f)?;
                    }

                    if let Some(tail_expr) = tail_expr {
                        write!(f, "{}", " ".repeat(indentation))?;
                        write_expr(*tail_expr, bodies, f, indentation)?;
                        writeln!(f)?;
                    }

                    indentation -= 4;
                    write!(f, "{}", " ".repeat(indentation))?;

                    write!(f, "}}")?;
                }

                Expr::Local(id) => write!(f, "l{}", unsafe { std::mem::transmute::<_, u32>(*id) })?,

                Expr::Param { idx } => write!(f, "p{}", idx)?,

                Expr::Call { name, args } => {
                    write!(f, "{}", name.0)?;

                    for (idx, arg) in args.iter().enumerate() {
                        if idx == 0 {
                            write!(f, " ")?;
                        } else {
                            write!(f, ", ")?;
                        }

                        write_expr(*arg, bodies, f, indentation)?;
                    }
                }
            }

            Ok(())
        }

        fn write_statement(
            id: Id<Statement>,
            bodies: &Bodies,
            f: &mut fmt::Formatter<'_>,
            indentation: usize,
        ) -> fmt::Result {
            match &bodies[id] {
                Statement::Expr(expr_id) => {
                    write_expr(*expr_id, bodies, f, indentation)?;
                    write!(f, ";")
                }
                Statement::LocalDef(local_def_id) => {
                    write!(f, "let l{} = ", unsafe {
                        std::mem::transmute::<_, u32>(*local_def_id)
                    })?;
                    write_expr(bodies[*local_def_id].value, bodies, f, indentation)?;
                    write!(f, ";")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::index;
    use ast::AstNode;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check<const N: usize>(
        input: &str,
        expect: Expect,
        expected_diagnostics: [(LoweringDiagnosticKind, std::ops::Range<u32>); N],
    ) {
        let tokens = lexer::lex(input);
        let parse = parser::parse_source_file(&tokens);
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (index, _) = index(&root);
        let (bodies, actual_diagnostics) = lower(&root, &index);

        expect.assert_eq(&format!("{:?}", bodies));

        let expected_diagnostics: Vec<_> = expected_diagnostics
            .into_iter()
            .map(|(kind, range)| LoweringDiagnostic {
                kind,
                range: TextRange::new(range.start.into(), range.end.into()),
            })
            .collect();

        assert_eq!(expected_diagnostics, actual_diagnostics);
    }

    #[test]
    fn empty() {
        check("", expect![["\n"]], []);
    }

    #[test]
    fn int_literal() {
        check(
            r#"
                fnc a: s32 -> 1;
            "#,
            expect![[r#"
                fnc a -> 1;
            "#]],
            [],
        );
    }

    #[test]
    fn out_of_range_int_literal() {
        check(
            r#"
                fnc a: s32 -> 999999999999999;
            "#,
            expect![[r#"
                fnc a -> <missing>;
            "#]],
            [],
        );
    }

    #[test]
    fn binary_expr() {
        check(
            r#"
                fnc sum: s32 -> 2 + 2;
            "#,
            expect![[r#"
                fnc sum -> 2 + 2;
            "#]],
            [],
        );
    }

    #[test]
    fn string_literal() {
        check(
            r#"
                fnc crab: string -> "🦀";
            "#,
            expect![[r#"
                fnc crab -> "🦀";
            "#]],
            [],
        );
    }

    #[test]
    fn nested_binary_exprs() {
        check(
            r#"
                fnc a: s32 -> 1 + 2 * 3 + 4 * 5;
            "#,
            expect![[r#"
                fnc a -> 1 + 2 * 3 + 4 * 5;
            "#]],
            [],
        );
    }

    #[test]
    fn empty_block() {
        check(
            r#"
                fnc nil -> {};
            "#,
            expect![[r#"
                fnc nil -> {};
            "#]],
            [],
        );
    }

    #[test]
    fn block_with_one_expr() {
        check(
            r#"
                fnc one: s32 -> { 1 };
            "#,
            expect![[r#"
                fnc one -> { 1 };
            "#]],
            [],
        );
    }

    #[test]
    fn block_with_one_local_def() {
        check(
            r#"
                fnc foo -> { let a = 7; };
            "#,
            expect![[r#"
                fnc foo -> {
                    let l0 = 7;
                };
            "#]],
            [],
        );
    }

    #[test]
    fn block_with_one_expr_statement() {
        check(
            r#"
                fnc a -> { 100; };
            "#,
            expect![[r#"
                fnc a -> {
                    100;
                };
            "#]],
            [],
        );
    }

    #[test]
    fn block_with_local_def_and_usage() {
        check(
            r#"
                fnc magic_number: s32 -> { let n = 42; n };
            "#,
            expect![[r#"
                fnc magic_number -> {
                    let l0 = 42;
                    l0
                };
            "#]],
            [],
        );
    }

    #[test]
    fn block_with_multiple_local_defs() {
        check(
            r#"
                fnc f -> {
                    let a = 1;
                    let b = 2;
                    let c = 3;
                    let d = 4;
                };
            "#,
            expect![[r#"
                fnc f -> {
                    let l0 = 1;
                    let l1 = 2;
                    let l2 = 3;
                    let l3 = 4;
                };
            "#]],
            [],
        );
    }

    #[test]
    fn multiple_functions() {
        check(
            r#"
                fnc a -> {};
                fnc d -> {};
                fnc b -> {};
                fnc e -> {};
                fnc c -> {};
            "#,
            expect![[r#"
                fnc a -> {};
                fnc b -> {};
                fnc c -> {};
                fnc d -> {};
                fnc e -> {};
            "#]],
            [],
        );
    }

    #[test]
    fn undefined_local() {
        check(
            r#"
                fnc foo -> bar;
            "#,
            expect![[r#"
                fnc foo -> <missing>;
            "#]],
            [(LoweringDiagnosticKind::UndefinedLocal { name: "bar".to_string() }, 28..31)],
        );
    }

    #[test]
    fn scope_locals_to_blocks() {
        check(
            r#"
                fnc a: s32 -> {
                    { let foo = 5; };
                    foo
                }
            "#,
            expect![[r#"
                fnc a -> {
                    {
                        let l0 = 5;
                    };
                    <missing>
                };
            "#]],
            [(LoweringDiagnosticKind::UndefinedLocal { name: "foo".to_string() }, 91..94)],
        );
    }

    #[test]
    fn binary_expr_with_missing_operand() {
        check(
            r#"
                fnc number: s32 -> 1 +;
            "#,
            expect![[r#"
                fnc number -> 1 + <missing>;
            "#]],
            [],
        );
    }

    #[test]
    fn unused_params() {
        check(
            r#"
                fnc add(x: s32, y: s32) -> {};
            "#,
            expect![[r#"
                fnc add -> {};
            "#]],
            [],
        );
    }

    #[test]
    fn used_params() {
        check(
            r#"
                fnc add(x: s32, y: s32): s32 -> x + y;
            "#,
            expect![[r#"
                fnc add -> p0 + p1;
            "#]],
            [],
        );
    }

    #[test]
    fn call_with_no_args() {
        check(
            r#"
                fnc five: s32 -> 5;
                fnc ten: s32 -> five + five;
            "#,
            expect![[r#"
                fnc five -> 5;
                fnc ten -> five + five;
            "#]],
            [],
        );
    }

    #[test]
    fn call_with_args() {
        check(
            r#"
                fnc multiply(x: s32, y: s32): s32 -> x * y;
                fnc ten: s32 -> multiply 2, 5;
            "#,
            expect![[r#"
                fnc multiply -> p0 * p1;
                fnc ten -> multiply 2, 5;
            "#]],
            [],
        );
    }

    #[test]
    fn mismatched_arg_count() {
        check(
            r#"
                fnc greeting: string -> id "Hello", "World";
                fnc id(s: string): string -> s;
            "#,
            expect![[r#"
                fnc greeting -> <missing>;
                fnc id -> p0;
            "#]],
            [(
                LoweringDiagnosticKind::MismatchedArgCount {
                    name: "id".to_string(),
                    expected: 1,
                    got: 2,
                },
                41..43,
            )],
        );
    }

    #[test]
    fn locals_takes_precedence_over_functions() {
        check(
            r#"
                fnc foo -> {};
                fnc bar(n: s32) -> {};

                fnc main -> {
                    let foo = 0;
                    let bar = 1;
                    foo;
                    bar;
                };
            "#,
            expect![[r#"
                fnc bar -> {};
                fnc foo -> {};
                fnc main -> {
                    let l0 = 0;
                    let l1 = 1;
                    l0;
                    l1;
                };
            "#]],
            [],
        );
    }

    #[test]
    fn local_with_args() {
        check(
            r#"
                fnc a -> {
                    let s = "foo";
                    s 1, 2, 3
                };
            "#,
            expect![[r#"
                fnc a -> {
                    let l0 = "foo";
                    l0
                };
            "#]],
            [(LoweringDiagnosticKind::CalledLocal { name: "s".to_string() }, 83..84)],
        );
    }

    #[test]
    fn nested_blocks() {
        check(
            r#"
                fnc main -> {
                    let foo = {
                        let bar = {
                            let baz = 9;
                            baz * 10
                        };
                        bar - 1
                    };
                    foo + 3
                };
            "#,
            expect![[r#"
                fnc main -> {
                    let l2 = {
                        let l1 = {
                            let l0 = 9;
                            l0 * 10
                        };
                        l1 - 1
                    };
                    l2 + 3
                };
            "#]],
            [],
        );
    }

    #[test]
    fn self_recursive_function() {
        check(
            r#"
                fnc die(n: s32) -> die n;
            "#,
            expect![[r#"
                fnc die -> die p0;
            "#]],
            [],
        );
    }

    #[test]
    fn mutually_recursive_functions() {
        check(
            r#"
                fnc a -> b;
                fnc b -> a;
            "#,
            expect![[r#"
                fnc a -> b;
                fnc b -> a;
            "#]],
            [],
        );
    }
}
