use crate::{Fqn, Function, GetFunctionError, Index, Name, WorldIndex};
use arena::{Arena, ArenaMap, Id};
use ast::{AstNode, AstToken};
use interner::{Interner, Key};
use std::collections::{HashMap, HashSet};
use syntax::SyntaxTree;
use text_size::TextRange;

#[derive(Clone)]
pub struct Bodies {
    local_defs: Arena<LocalDef>,
    statements: Arena<Statement>,
    exprs: Arena<Expr>,
    expr_ranges: ArenaMap<Id<Expr>, TextRange>,
    function_bodies: HashMap<Name, Id<Expr>>,
    other_module_references: HashSet<Fqn>,
    symbol_map: HashMap<ast::Ident, Symbol>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Missing,
    IntLiteral(u32),
    StringLiteral(String),
    Binary { lhs: Id<Expr>, rhs: Id<Expr>, operator: BinaryOperator },
    Block { statements: Vec<Id<Statement>>, tail_expr: Option<Id<Expr>> },
    Local(Id<LocalDef>),
    Param { idx: u32 },
    Call { path: Path, args: Vec<Id<Expr>> },
}

#[derive(Debug, Clone, Copy)]
pub enum Path {
    ThisModule(Name),
    OtherModule(Fqn),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Id<Expr>),
    LocalDef(Id<LocalDef>),
}

#[derive(Clone)]
pub struct LocalDef {
    pub value: Id<Expr>,
    pub ast: ast::LocalDef,
}

impl std::fmt::Debug for LocalDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LocalDef").field("value", &self.value).finish()
    }
}

#[derive(Debug, Clone, Copy)]
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
    OutOfRangeIntLiteral,
    UndefinedLocal { name: Key },
    UndefinedModule { name: Key },
    MismatchedArgCount { name: Key, expected: u32, got: u32 },
    CalledLocal { name: Key },
}

#[derive(Clone, Copy)]
pub enum Symbol {
    Local(Id<LocalDef>),
    Param(ast::Param),
    Function(Path),
    Module(Name),
}

pub fn lower(
    root: ast::Root,
    tree: &SyntaxTree,
    index: &Index,
    world_index: &WorldIndex,
    interner: &mut Interner,
) -> (Bodies, Vec<LoweringDiagnostic>) {
    let mut ctx = Ctx::new(index, world_index, interner, tree);

    for def in root.defs(tree) {
        match def {
            ast::Def::Function(function) => ctx.lower_function(function),
        }
    }

    ctx.bodies.shrink_to_fit();

    (ctx.bodies, ctx.diagnostics)
}

struct Ctx<'a> {
    bodies: Bodies,
    index: &'a Index,
    world_index: &'a WorldIndex,
    interner: &'a mut Interner,
    tree: &'a SyntaxTree,
    diagnostics: Vec<LoweringDiagnostic>,
    scopes: Vec<HashMap<Key, Id<LocalDef>>>,
    params: HashMap<Key, (u32, ast::Param)>,
}

impl<'a> Ctx<'a> {
    fn new(
        index: &'a Index,
        world_index: &'a WorldIndex,
        interner: &'a mut Interner,
        tree: &'a SyntaxTree,
    ) -> Self {
        Self {
            bodies: Bodies {
                local_defs: Arena::new(),
                statements: Arena::new(),
                exprs: Arena::new(),
                expr_ranges: ArenaMap::default(),
                function_bodies: HashMap::new(),
                other_module_references: HashSet::new(),
                symbol_map: HashMap::new(),
            },
            index,
            world_index,
            interner,
            tree,
            diagnostics: Vec::new(),
            scopes: vec![HashMap::new()],
            params: HashMap::new(),
        }
    }

    fn lower_function(&mut self, function: ast::Function) {
        let name = match function.name(self.tree) {
            Some(ident) => Name(self.interner.intern(ident.text(self.tree))),
            None => return,
        };

        // if we’ve already seen a function with this name,
        // we ignore all other functions with that name
        //
        // we don’t have to worry about emitting a diagnostic here
        // because indexing already handles this
        if self.bodies.function_bodies.contains_key(&name) {
            return;
        }

        if let Some(param_list) = function.param_list(self.tree) {
            for (idx, param) in param_list.params(self.tree).enumerate() {
                if let Some(ident) = param.name(self.tree) {
                    self.params
                        .insert(self.interner.intern(ident.text(self.tree)), (idx as u32, param));
                }
            }
        }

        let body = self.lower_expr(function.body(self.tree));
        self.params.clear();
        self.bodies.function_bodies.insert(name, body);
    }

    fn lower_statement(&mut self, statement: ast::Statement) -> Statement {
        match statement {
            ast::Statement::LocalDef(local_def) => self.lower_local_def(local_def),
            ast::Statement::ExprStatement(expr_statement) => {
                let expr = self.lower_expr(expr_statement.expr(self.tree));
                Statement::Expr(expr)
            }
        }
    }

    fn lower_local_def(&mut self, local_def: ast::LocalDef) -> Statement {
        let value = self.lower_expr(local_def.value(self.tree));
        let id = self.bodies.local_defs.alloc(LocalDef { value, ast: local_def });

        if let Some(ident) = local_def.name(self.tree) {
            let name = self.interner.intern(ident.text(self.tree));
            self.insert_into_current_scope(name, id);
        }

        Statement::LocalDef(id)
    }

    fn lower_expr(&mut self, expr: Option<ast::Expr>) -> Id<Expr> {
        let expr_ast = match expr {
            Some(expr) => expr,
            None => return self.bodies.exprs.alloc(Expr::Missing),
        };

        let range = expr_ast.range(self.tree);

        let expr = match expr_ast {
            ast::Expr::Binary(binary_expr) => self.lower_binary_expr(binary_expr),
            ast::Expr::Block(block) => self.lower_block(block),
            ast::Expr::Call(call) => self.lower_local_or_call(call),
            ast::Expr::IntLiteral(int_literal) => self.lower_int_literal(int_literal),
            ast::Expr::StringLiteral(string_literal) => self.lower_string_literal(string_literal),
        };

        let id = self.bodies.exprs.alloc(expr);
        self.bodies.expr_ranges.insert(id, range);

        id
    }

    fn lower_binary_expr(&mut self, binary_expr: ast::BinaryExpr) -> Expr {
        let lhs = self.lower_expr(binary_expr.lhs(self.tree));
        let rhs = self.lower_expr(binary_expr.rhs(self.tree));

        let operator = match binary_expr.operator(self.tree) {
            Some(ast::BinaryOperator::Add(_)) => BinaryOperator::Add,
            Some(ast::BinaryOperator::Sub(_)) => BinaryOperator::Sub,
            Some(ast::BinaryOperator::Mul(_)) => BinaryOperator::Mul,
            Some(ast::BinaryOperator::Div(_)) => BinaryOperator::Div,
            None => return Expr::Missing,
        };

        Expr::Binary { lhs, rhs, operator }
    }

    fn lower_block(&mut self, block: ast::Block) -> Expr {
        self.create_new_child_scope();

        let mut statements = Vec::new();

        for statement in block.statements(self.tree) {
            let statement = self.lower_statement(statement);
            statements.push(self.bodies.statements.alloc(statement));
        }

        let tail_expr =
            block.tail_expr(self.tree).map(|tail_expr| self.lower_expr(Some(tail_expr)));

        self.destroy_current_scope();

        Expr::Block { statements, tail_expr }
    }

    fn lower_local_or_call(&mut self, call: ast::Call) -> Expr {
        let ident = match call.top_level_name(self.tree) {
            Some(ident) => ident,
            None => return Expr::Missing,
        };

        if let Some(function_name_token) = call.nested_name(self.tree) {
            let module_name_token = ident;

            let module_name = self.interner.intern(module_name_token.text(self.tree));
            let function_name = self.interner.intern(function_name_token.text(self.tree));

            let fqn = Fqn { module: Name(module_name), function: Name(function_name) };

            match self.world_index.get_function(fqn) {
                Ok(function) => {
                    let path = Path::OtherModule(fqn);

                    self.bodies.other_module_references.insert(fqn);
                    self.bodies
                        .symbol_map
                        .insert(module_name_token, Symbol::Module(Name(module_name)));
                    self.bodies.symbol_map.insert(function_name_token, Symbol::Function(path));

                    return self.lower_call(call, function, path, function_name_token);
                }

                Err(GetFunctionError::UnknownModule) => {
                    self.diagnostics.push(LoweringDiagnostic {
                        kind: LoweringDiagnosticKind::UndefinedModule { name: module_name },
                        range: module_name_token.range(self.tree),
                    });

                    return Expr::Missing;
                }

                Err(GetFunctionError::UnknownFunction) => {
                    self.diagnostics.push(LoweringDiagnostic {
                        kind: LoweringDiagnosticKind::UndefinedLocal { name: function_name },
                        range: function_name_token.range(self.tree),
                    });
                    self.bodies
                        .symbol_map
                        .insert(module_name_token, Symbol::Module(Name(module_name)));

                    return Expr::Missing;
                }
            }
        }

        let name = self.interner.intern(ident.text(self.tree));

        if let Some(def) = self.look_up_in_current_scope(name) {
            check_args_for_local(call, ident, self.tree, name, &mut self.diagnostics);
            self.bodies.symbol_map.insert(ident, Symbol::Local(def));
            return Expr::Local(def);
        }

        if let Some((idx, ast)) = self.look_up_param(name) {
            check_args_for_local(call, ident, self.tree, name, &mut self.diagnostics);
            self.bodies.symbol_map.insert(ident, Symbol::Param(ast));
            return Expr::Param { idx };
        }

        let name = Name(name);
        if let Some(function) = self.index.get_function(name) {
            let path = Path::ThisModule(name);
            self.bodies.symbol_map.insert(ident, Symbol::Function(path));
            return self.lower_call(call, function, path, ident);
        }

        self.diagnostics.push(LoweringDiagnostic {
            kind: LoweringDiagnosticKind::UndefinedLocal { name: name.0 },
            range: ident.range(self.tree),
        });

        return Expr::Missing;

        fn check_args_for_local(
            call: ast::Call,
            ident: ast::Ident,
            tree: &SyntaxTree,
            name: Key,
            diagnostics: &mut Vec<LoweringDiagnostic>,
        ) {
            if let Some(arg_list) = call.arg_list(tree) {
                if arg_list.args(tree).count() != 0 {
                    diagnostics.push(LoweringDiagnostic {
                        kind: LoweringDiagnosticKind::CalledLocal { name },
                        range: ident.range(tree),
                    });
                }
            }
        }
    }

    fn lower_call(
        &mut self,
        call: ast::Call,
        function: &Function,
        path: Path,
        ident: ast::Ident,
    ) -> Expr {
        let arg_list = call.arg_list(self.tree);

        let expected = function.params.len() as u32;
        let got = match &arg_list {
            Some(al) => al.args(self.tree).count() as u32,
            None => 0,
        };

        if expected != got {
            let name = match path {
                Path::ThisModule(function) => function.0,
                Path::OtherModule(fqn) => fqn.function.0,
            };

            self.diagnostics.push(LoweringDiagnostic {
                kind: LoweringDiagnosticKind::MismatchedArgCount { name, expected, got },
                range: ident.range(self.tree),
            });

            return Expr::Missing;
        }

        let mut args = Vec::new();

        if let Some(arg_list) = arg_list {
            for arg in arg_list.args(self.tree) {
                let expr = self.lower_expr(arg.value(self.tree));
                args.push(expr);
            }
        }

        Expr::Call { path, args }
    }

    fn lower_int_literal(&mut self, int_literal: ast::IntLiteral) -> Expr {
        let value = int_literal.value(self.tree).and_then(|int| int.text(self.tree).parse().ok());

        if let Some(value) = value {
            return Expr::IntLiteral(value);
        }

        self.diagnostics.push(LoweringDiagnostic {
            kind: LoweringDiagnosticKind::OutOfRangeIntLiteral,
            range: int_literal.range(self.tree),
        });

        Expr::Missing
    }

    fn lower_string_literal(&self, string_literal: ast::StringLiteral) -> Expr {
        match string_literal.value(self.tree) {
            Some(string) => {
                let text = string.text(self.tree);

                // trim off quotes
                Expr::StringLiteral(text[1..text.len() - 1].to_string())
            }
            None => Expr::Missing,
        }
    }

    fn insert_into_current_scope(&mut self, name: Key, id: Id<LocalDef>) {
        self.scopes.last_mut().unwrap().insert(name, id);
    }

    fn look_up_in_current_scope(&mut self, name: Key) -> Option<Id<LocalDef>> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.get(&name) {
                return Some(*def);
            }
        }

        None
    }

    fn look_up_param(&mut self, name: Key) -> Option<(u32, ast::Param)> {
        self.params.get(&name).copied()
    }

    fn create_new_child_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn destroy_current_scope(&mut self) {
        self.scopes.pop();
    }
}

impl Bodies {
    pub fn function_body(&self, name: Name) -> Id<Expr> {
        self.function_bodies[&name]
    }

    pub fn range_for_expr(&self, expr: Id<Expr>) -> TextRange {
        self.expr_ranges[expr]
    }

    pub fn other_module_references(&self) -> &HashSet<Fqn> {
        &self.other_module_references
    }

    pub fn symbol(&self, ident: ast::Ident) -> Option<Symbol> {
        self.symbol_map.get(&ident).copied()
    }

    fn shrink_to_fit(&mut self) {
        let Self {
            local_defs,
            statements,
            exprs,
            expr_ranges,
            function_bodies,
            other_module_references,
            symbol_map,
        } = self;

        local_defs.shrink_to_fit();
        statements.shrink_to_fit();
        exprs.shrink_to_fit();
        expr_ranges.shrink_to_fit();
        function_bodies.shrink_to_fit();
        other_module_references.shrink_to_fit();
        symbol_map.shrink_to_fit();
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

impl Bodies {
    pub fn debug(&self, interner: &Interner) -> String {
        let mut s = String::new();

        let mut function_bodies: Vec<_> = self.function_bodies.iter().collect();
        function_bodies.sort_unstable_by_key(|(name, _)| *name);

        for (name, expr_id) in function_bodies {
            s.push_str(&format!("fnc {} -> ", interner.lookup(name.0)));
            write_expr(*expr_id, self, &mut s, interner, 0);
            s.push_str(";\n");
        }

        if !self.other_module_references.is_empty() {
            let mut other_module_references: Vec<_> = self.other_module_references.iter().collect();
            other_module_references.sort_unstable();

            s.push_str("\nReferences to other modules:\n");
            for fqn in &other_module_references {
                s.push_str(&format!(
                    "- {}.{}\n",
                    interner.lookup(fqn.module.0),
                    interner.lookup(fqn.function.0)
                ));
            }
        }

        return s;

        fn write_expr(
            id: Id<Expr>,
            bodies: &Bodies,
            s: &mut String,
            interner: &Interner,
            mut indentation: usize,
        ) {
            match &bodies[id] {
                Expr::Missing => s.push_str("<missing>"),

                Expr::IntLiteral(n) => s.push_str(&format!("{}", n)),

                Expr::StringLiteral(content) => s.push_str(&format!("\"{}\"", content)),

                Expr::Binary { lhs, rhs, operator } => {
                    write_expr(*lhs, bodies, s, interner, indentation);

                    s.push(' ');

                    match operator {
                        BinaryOperator::Add => s.push('+'),
                        BinaryOperator::Sub => s.push('-'),
                        BinaryOperator::Mul => s.push('*'),
                        BinaryOperator::Div => s.push('/'),
                    }

                    s.push(' ');

                    write_expr(*rhs, bodies, s, interner, indentation);
                }

                Expr::Block { statements, tail_expr: None } if statements.is_empty() => {
                    s.push_str("{}");
                }

                Expr::Block { statements, tail_expr: Some(tail_expr) } if statements.is_empty() => {
                    s.push_str("{ ");
                    write_expr(*tail_expr, bodies, s, interner, indentation + 4);
                    s.push_str(" }");
                }

                Expr::Block { statements, tail_expr } => {
                    indentation += 4;

                    s.push_str("{\n");

                    for statement in statements.clone() {
                        s.push_str(&" ".repeat(indentation));
                        write_statement(statement, bodies, s, interner, indentation);
                        s.push('\n');
                    }

                    if let Some(tail_expr) = tail_expr {
                        s.push_str(&" ".repeat(indentation));
                        write_expr(*tail_expr, bodies, s, interner, indentation);
                        s.push('\n');
                    }

                    indentation -= 4;
                    s.push_str(&" ".repeat(indentation));

                    s.push('}');
                }

                Expr::Local(id) => s.push_str(&format!("l{}", id.to_raw())),

                Expr::Param { idx } => s.push_str(&format!("p{}", idx)),

                Expr::Call { path, args } => {
                    match path {
                        Path::ThisModule(function) => s.push_str(interner.lookup(function.0)),
                        Path::OtherModule(fqn) => s.push_str(&format!(
                            "{}.{}",
                            interner.lookup(fqn.module.0),
                            interner.lookup(fqn.function.0)
                        )),
                    }

                    for (idx, arg) in args.iter().enumerate() {
                        if idx == 0 {
                            s.push(' ');
                        } else {
                            s.push_str(", ");
                        }

                        write_expr(*arg, bodies, s, interner, indentation);
                    }
                }
            }
        }

        fn write_statement(
            id: Id<Statement>,
            bodies: &Bodies,
            s: &mut String,
            interner: &Interner,
            indentation: usize,
        ) {
            match &bodies[id] {
                Statement::Expr(expr_id) => {
                    write_expr(*expr_id, bodies, s, interner, indentation);
                    s.push(';');
                }
                Statement::LocalDef(local_def_id) => {
                    s.push_str(&format!("let l{} = ", local_def_id.to_raw()));
                    write_expr(bodies[*local_def_id].value, bodies, s, interner, indentation);
                    s.push(';');
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
        expected_diagnostics: impl Fn(
            &mut Interner,
        ) -> [(LoweringDiagnosticKind, std::ops::Range<u32>); N],
    ) {
        let modules = utils::split_multi_module_test_data(input);
        let mut interner = Interner::default();
        let mut world_index = WorldIndex::default();

        for (name, text) in &modules {
            if *name == "main" {
                continue;
            }

            let tokens = lexer::lex(text);
            let tree = parser::parse_source_file(&tokens, text).into_syntax_tree();
            let root = ast::Root::cast(tree.root(), &tree).unwrap();
            let (index, _) = index(root, &tree, &WorldIndex::default(), &mut interner);

            world_index.add_module(Name(interner.intern(name)), index);
        }

        let text = &modules["main"];
        let tokens = lexer::lex(text);
        let tree = parser::parse_source_file(&tokens, text).into_syntax_tree();
        let root = ast::Root::cast(tree.root(), &tree).unwrap();
        let (index, _) = index(root, &tree, &WorldIndex::default(), &mut interner);

        let (bodies, actual_diagnostics) = lower(root, &tree, &index, &world_index, &mut interner);

        expect.assert_eq(&bodies.debug(&interner));

        let expected_diagnostics: Vec<_> = expected_diagnostics(&mut interner)
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
        check("", expect![["\n"]], |_| []);
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
            |_| [],
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
            |_| [(LoweringDiagnosticKind::OutOfRangeIntLiteral, 31..46)],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
                fnc d -> {};
                fnc b -> {};
                fnc e -> {};
                fnc c -> {};
            "#]],
            |_| [],
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
            |i| [(LoweringDiagnosticKind::UndefinedLocal { name: i.intern("bar") }, 28..31)],
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
            |i| [(LoweringDiagnosticKind::UndefinedLocal { name: i.intern("foo") }, 91..94)],
        );
    }

    #[test]
    fn reference_local_from_block() {
        check(
            r#"
                fnc f -> {
                    let a = 7;
                    { a };
                }
            "#,
            expect![[r#"
                fnc f -> {
                    let l0 = 7;
                    { l0 };
                };
            "#]],
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |i| {
                [(
                    LoweringDiagnosticKind::MismatchedArgCount {
                        name: i.intern("id"),
                        expected: 1,
                        got: 2,
                    },
                    41..43,
                )]
            },
        );
    }

    #[test]
    fn call_with_no_args_when_some_were_expected() {
        check(
            r#"
                fnc a: string -> id;
                fnc id(s: string): string -> s;
            "#,
            expect![[r#"
                fnc a -> <missing>;
                fnc id -> p0;
            "#]],
            |i| {
                [(
                    LoweringDiagnosticKind::MismatchedArgCount {
                        name: i.intern("id"),
                        expected: 1,
                        got: 0,
                    },
                    34..36,
                )]
            },
        );
    }

    #[test]
    fn locals_take_precedence_over_functions() {
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
                fnc foo -> {};
                fnc bar -> {};
                fnc main -> {
                    let l0 = 0;
                    let l1 = 1;
                    l0;
                    l1;
                };
            "#]],
            |_| [],
        );
    }

    #[test]
    fn locals_take_precedence_over_params() {
        check(
            r#"
                fnc id(x: s32): s32 -> {
                    let x = 3;
                    x
                };
            "#,
            expect![[r#"
                fnc id -> {
                    let l0 = 3;
                    l0
                };
            "#]],
            |_| [],
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
            |i| [(LoweringDiagnosticKind::CalledLocal { name: i.intern("s") }, 83..84)],
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
            |_| [],
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
            |_| [],
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
            |_| [],
        );
    }

    #[test]
    fn functions_from_other_module() {
        check(
            r#"
                #- main
                fnc a: s32 -> foo.id foo.constant;
                #- foo
                fnc id(n: s32): s32 -> n;
                fnc constant: s32 -> 42;
            "#,
            expect![[r#"
                fnc a -> foo.id foo.constant;

                References to other modules:
                - foo.id
                - foo.constant
            "#]],
            |_| [],
        );
    }

    #[test]
    fn function_from_undefined_module() {
        check(
            r#"
                fnc main -> foo.bar;
            "#,
            expect![[r#"
                fnc main -> <missing>;
            "#]],
            |i| [(LoweringDiagnosticKind::UndefinedModule { name: i.intern("foo") }, 29..32)],
        );
    }

    #[test]
    fn function_from_undefined_module_with_same_name_as_local_function() {
        check(
            r#"
                fnc a -> a.foo;
            "#,
            expect![[r#"
                fnc a -> <missing>;
            "#]],
            |i| [(LoweringDiagnosticKind::UndefinedModule { name: i.intern("a") }, 26..27)],
        );
    }

    #[test]
    fn undefined_function_from_other_module() {
        check(
            r#"
                #- main
                fnc trim(s: string): string -> utils.strip s, " ";
                #- utils
            "#,
            expect![[r#"
                fnc trim -> <missing>;
            "#]],
            |i| [(LoweringDiagnosticKind::UndefinedLocal { name: i.intern("strip") }, 53..58)],
        );
    }

    #[test]
    fn mismatched_arg_count_for_function_from_other_module() {
        check(
            r#"
                #- main
                fnc the_answer: s32 -> math.add 14, 14, 14;
                #- math
                fnc add(x: s32, y: s32): s32 -> x + y;
            "#,
            expect![[r#"
                fnc the_answer -> <missing>;

                References to other modules:
                - math.add
            "#]],
            |i| {
                [(
                    LoweringDiagnosticKind::MismatchedArgCount {
                        name: i.intern("add"),
                        expected: 2,
                        got: 3,
                    },
                    44..47,
                )]
            },
        );
    }

    #[test]
    fn functions_with_same_name() {
        check(
            r#"
                #- main
                fnc foo: s32 -> 10;
                fnc foo: s32 -> 92;
                fnc foo: string -> "bar";
            "#,
            expect![[r#"
                fnc foo -> 10;
            "#]],
            |_| [], // indexing already emits a diagnostic for this
        );
    }
}
