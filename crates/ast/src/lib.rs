pub mod validation;

use syntax::{NodeKind, SyntaxNode, SyntaxToken, SyntaxTree, TokenKind};
use text_size::TextRange;

pub trait AstNode: Copy + Sized {
    fn cast(node: SyntaxNode, tree: &SyntaxTree) -> Option<Self>;

    fn syntax(self) -> SyntaxNode;

    fn text(self, tree: &SyntaxTree) -> &str {
        self.syntax().text(tree)
    }

    fn range(self, tree: &SyntaxTree) -> TextRange {
        self.syntax().range(tree)
    }
}

pub trait AstToken: Sized {
    fn cast(token: SyntaxToken, tree: &SyntaxTree) -> Option<Self>;

    fn syntax(self) -> SyntaxToken;

    fn text(self, tree: &SyntaxTree) -> &str {
        self.syntax().text(tree)
    }

    fn range(self, tree: &SyntaxTree) -> TextRange {
        self.syntax().range(tree)
    }
}

macro_rules! def_ast_node {
    ($kind:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $kind(SyntaxNode);

        impl AstNode for $kind {
            fn cast(node: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
                if node.kind(tree) == NodeKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(self) -> SyntaxNode {
                self.0
            }
        }
    };
}

macro_rules! def_ast_token {
    ($kind:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $kind(SyntaxToken);

        impl AstToken for $kind {
            fn cast(token: SyntaxToken, tree: &SyntaxTree) -> Option<Self> {
                if token.kind(tree) == TokenKind::$kind {
                    Some(Self(token))
                } else {
                    None
                }
            }

            fn syntax(self) -> SyntaxToken {
                self.0
            }
        }
    };
}

def_ast_node!(Root);

impl Root {
    pub fn defs(self, tree: &SyntaxTree) -> impl Iterator<Item = Def> + '_ {
        nodes(self, tree)
    }

    pub fn statements(self, tree: &SyntaxTree) -> impl Iterator<Item = Statement> + '_ {
        nodes(self, tree)
    }

    pub fn tail_expr(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Def {
    Function(Function),
}

impl AstNode for Def {
    fn cast(node: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
        match node.kind(tree) {
            NodeKind::Function => Some(Self::Function(Function(node))),

            _ => None,
        }
    }

    fn syntax(self) -> SyntaxNode {
        match self {
            Self::Function(function) => function.syntax(),
        }
    }
}

def_ast_node!(Function);

impl Function {
    pub fn docs(self, tree: &SyntaxTree) -> Option<Docs> {
        node(self, tree)
    }

    pub fn name(self, tree: &SyntaxTree) -> Option<Ident> {
        token(self, tree)
    }

    pub fn param_list(self, tree: &SyntaxTree) -> Option<ParamList> {
        node(self, tree)
    }

    pub fn return_ty(self, tree: &SyntaxTree) -> Option<ReturnTy> {
        node(self, tree)
    }

    pub fn body(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Statement {
    LocalDef(LocalDef),
    ExprStatement(ExprStatement),
}

impl AstNode for Statement {
    fn cast(node: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
        match node.kind(tree) {
            NodeKind::LocalDef => Some(Self::LocalDef(LocalDef(node))),
            NodeKind::ExprStatement => Some(Self::ExprStatement(ExprStatement(node))),
            _ => None,
        }
    }

    fn syntax(self) -> SyntaxNode {
        match self {
            Self::LocalDef(local_def) => local_def.syntax(),
            Self::ExprStatement(expr) => expr.syntax(),
        }
    }
}

def_ast_node!(LocalDef);

impl LocalDef {
    pub fn name(self, tree: &SyntaxTree) -> Option<Ident> {
        token(self, tree)
    }

    pub fn value(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }
}

def_ast_node!(ParamList);

impl ParamList {
    pub fn params(self, tree: &SyntaxTree) -> impl Iterator<Item = Param> + '_ {
        nodes(self, tree)
    }
}

def_ast_node!(ReturnTy);

impl ReturnTy {
    pub fn ty(self, tree: &SyntaxTree) -> Option<Ty> {
        node(self, tree)
    }
}

def_ast_node!(Param);

impl Param {
    pub fn name(self, tree: &SyntaxTree) -> Option<Ident> {
        token(self, tree)
    }

    pub fn ty(self, tree: &SyntaxTree) -> Option<Ty> {
        node(self, tree)
    }
}

def_ast_node!(Ty);

impl Ty {
    pub fn name(self, tree: &SyntaxTree) -> Option<Ident> {
        token(self, tree)
    }
}

def_ast_node!(Docs);

impl Docs {
    pub fn doc_comments(self, tree: &SyntaxTree) -> impl Iterator<Item = DocComment> + '_ {
        nodes(self, tree)
    }
}

def_ast_node!(DocComment);

impl DocComment {
    pub fn contents(self, tree: &SyntaxTree) -> Option<DocCommentContents> {
        token(self, tree)
    }
}

def_ast_node!(ExprStatement);

impl ExprStatement {
    pub fn expr(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Expr {
    Binary(BinaryExpr),
    Block(Block),
    Call(Call),
    IntLiteral(IntLiteral),
    StringLiteral(StringLiteral),
}

impl AstNode for Expr {
    fn cast(node: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
        match node.kind(tree) {
            NodeKind::BinaryExpr => Some(Self::Binary(BinaryExpr(node))),
            NodeKind::Block => Some(Self::Block(Block(node))),
            NodeKind::Call => Some(Self::Call(Call(node))),
            NodeKind::IntLiteral => Some(Self::IntLiteral(IntLiteral(node))),
            NodeKind::StringLiteral => Some(Self::StringLiteral(StringLiteral(node))),
            _ => None,
        }
    }

    fn syntax(self) -> SyntaxNode {
        match self {
            Self::Binary(binary_expr) => binary_expr.syntax(),
            Self::Block(block) => block.syntax(),
            Self::Call(call) => call.syntax(),
            Self::IntLiteral(int_literal) => int_literal.syntax(),
            Self::StringLiteral(string_literal) => string_literal.syntax(),
        }
    }
}

def_ast_node!(BinaryExpr);

impl BinaryExpr {
    pub fn lhs(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }

    pub fn rhs(self, tree: &SyntaxTree) -> Option<Expr> {
        nodes(self, tree).nth(1)
    }

    pub fn operator(self, tree: &SyntaxTree) -> Option<BinaryOperator> {
        token(self, tree)
    }
}

def_ast_node!(Block);

impl Block {
    pub fn statements(self, tree: &SyntaxTree) -> impl Iterator<Item = Statement> + '_ {
        nodes(self, tree)
    }

    pub fn tail_expr(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }
}

def_ast_node!(Call);

impl Call {
    pub fn top_level_name(self, tree: &SyntaxTree) -> Option<Ident> {
        token(self, tree)
    }

    pub fn nested_name(self, tree: &SyntaxTree) -> Option<Ident> {
        self.syntax().child_tokens(tree).filter_map(|t| Ident::cast(t, tree)).nth(1)
    }

    pub fn arg_list(self, tree: &SyntaxTree) -> Option<ArgList> {
        node(self, tree)
    }
}

def_ast_node!(ArgList);

impl ArgList {
    pub fn args(self, tree: &SyntaxTree) -> impl Iterator<Item = Arg> + '_ {
        nodes(self, tree)
    }
}

def_ast_node!(Arg);

impl Arg {
    pub fn value(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }
}

def_ast_node!(IntLiteral);

impl IntLiteral {
    pub fn value(self, tree: &SyntaxTree) -> Option<Int> {
        token(self, tree)
    }
}

def_ast_node!(StringLiteral);

impl StringLiteral {
    pub fn components(self, tree: &SyntaxTree) -> impl Iterator<Item = StringComponent> + '_ {
        tokens(self, tree)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add(Plus),
    Sub(Hyphen),
    Mul(Asterisk),
    Div(Slash),
}

impl AstToken for BinaryOperator {
    fn cast(token: SyntaxToken, tree: &SyntaxTree) -> Option<Self> {
        match token.kind(tree) {
            TokenKind::Plus => Some(Self::Add(Plus(token))),
            TokenKind::Hyphen => Some(Self::Sub(Hyphen(token))),
            TokenKind::Asterisk => Some(Self::Mul(Asterisk(token))),
            TokenKind::Slash => Some(Self::Div(Slash(token))),
            _ => None,
        }
    }

    fn syntax(self) -> SyntaxToken {
        match self {
            Self::Add(plus) => plus.syntax(),
            Self::Sub(hyphen) => hyphen.syntax(),
            Self::Mul(asterisk) => asterisk.syntax(),
            Self::Div(slash) => slash.syntax(),
        }
    }
}

def_ast_token!(Plus);
def_ast_token!(Hyphen);
def_ast_token!(Asterisk);
def_ast_token!(Slash);
def_ast_token!(Ident);
def_ast_token!(Int);

pub enum StringComponent {
    Escape(Escape),
    Contents(StringContents),
}

impl AstToken for StringComponent {
    fn cast(token: SyntaxToken, tree: &SyntaxTree) -> Option<Self> {
        match token.kind(tree) {
            TokenKind::Escape => Some(Self::Escape(Escape(token))),
            TokenKind::StringContents => Some(Self::Contents(StringContents(token))),
            _ => None,
        }
    }

    fn syntax(self) -> SyntaxToken {
        match self {
            Self::Escape(escape) => escape.syntax(),
            Self::Contents(contents) => contents.syntax(),
        }
    }
}

def_ast_token!(Escape);
def_ast_token!(StringContents);

def_ast_token!(DocCommentContents);

fn nodes<Parent: AstNode, Child: AstNode>(
    node: Parent,
    tree: &SyntaxTree,
) -> impl Iterator<Item = Child> + '_ {
    node.syntax().child_nodes(tree).filter_map(|n| Child::cast(n, tree))
}

fn node<Parent: AstNode, Child: AstNode>(node: Parent, tree: &SyntaxTree) -> Option<Child> {
    node.syntax().child_nodes(tree).find_map(|n| Child::cast(n, tree))
}

fn tokens<Node: AstNode, Token: AstToken>(
    node: Node,
    tree: &SyntaxTree,
) -> impl Iterator<Item = Token> + '_ {
    node.syntax().child_tokens(tree).filter_map(|t| Token::cast(t, tree))
}

fn token<Node: AstNode, Token: AstToken>(node: Node, tree: &SyntaxTree) -> Option<Token> {
    node.syntax().child_tokens(tree).find_map(|t| Token::cast(t, tree))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> (SyntaxTree, Root) {
        let tree = parser::parse_repl_line(&lexer::lex(input), input).into_syntax_tree();
        let root = Root::cast(tree.root(), &tree).unwrap();

        (tree, root)
    }

    #[test]
    fn cast_root() {
        parse("");
    }

    #[test]
    fn get_statements() {
        let (tree, root) = parse("let a = b; a;");
        assert_eq!(root.statements(&tree).count(), 2);
    }

    #[test]
    fn inspect_statement_kind() {
        let (tree, root) = parse("let foo = bar; baz * quuz;");
        let mut statements = root.statements(&tree);

        assert!(matches!(statements.next(), Some(Statement::LocalDef(_))));
        assert!(matches!(statements.next(), Some(Statement::ExprStatement(_))));
        assert!(statements.next().is_none());
    }

    #[test]
    fn get_name_of_local_def() {
        let (tree, root) = parse("let a = 10;");
        let statement = root.statements(&tree).next().unwrap();

        let local_def = match statement {
            Statement::LocalDef(local_def) => local_def,
            _ => unreachable!(),
        };

        assert_eq!(local_def.name(&tree).unwrap().text(&tree), "a");
    }

    #[test]
    fn get_value_of_local_def() {
        let (tree, root) = parse("let foo = 5;");
        let statement = root.statements(&tree).next().unwrap();

        let local_def = match statement {
            Statement::LocalDef(local_def) => local_def,
            _ => unreachable!(),
        };

        assert!(matches!(local_def.value(&tree), Some(Expr::IntLiteral(_))));
    }

    #[test]
    fn get_lhs_and_rhs_of_binary_expr() {
        let (tree, root) = parse("foo * 2");
        assert!(root.statements(&tree).next().is_none());

        let binary_expr = match root.tail_expr(&tree) {
            Some(Expr::Binary(binary_expr)) => binary_expr,
            _ => unreachable!(),
        };

        assert!(matches!(binary_expr.lhs(&tree), Some(Expr::Call(_))));
        assert!(matches!(binary_expr.rhs(&tree), Some(Expr::IntLiteral(_))));
    }

    #[test]
    fn get_operator_of_binary_expr() {
        let (tree, root) = parse("a + b");

        let binary_expr = match root.tail_expr(&tree) {
            Some(Expr::Binary(binary_expr)) => binary_expr,
            _ => unreachable!(),
        };

        assert!(matches!(binary_expr.operator(&tree), Some(BinaryOperator::Add(_))));
    }

    #[test]
    fn get_name_of_call() {
        let (tree, root) = parse("idx");

        let call = match root.tail_expr(&tree) {
            Some(Expr::Call(call)) => call,
            _ => unreachable!(),
        };

        assert_eq!(call.top_level_name(&tree).unwrap().text(&tree), "idx");
    }

    #[test]
    fn get_name_of_call_in_nested_module() {
        let (tree, root) = parse("foo.bar");

        let call = match root.tail_expr(&tree) {
            Some(Expr::Call(call)) => call,
            _ => unreachable!(),
        };

        assert_eq!(call.top_level_name(&tree).unwrap().text(&tree), "foo");
        assert_eq!(call.nested_name(&tree).unwrap().text(&tree), "bar");
    }

    #[test]
    fn get_args_of_call() {
        let (tree, root) = parse("mul 10, 20");

        let call = match root.tail_expr(&tree) {
            Some(Expr::Call(call)) => call,
            _ => unreachable!(),
        };

        let mut args = call.arg_list(&tree).unwrap().args(&tree);

        assert_eq!(args.next().unwrap().value(&tree).unwrap().syntax().text(&tree), "10");
        assert_eq!(args.next().unwrap().value(&tree).unwrap().syntax().text(&tree), "20");
        assert!(args.next().is_none());
    }

    #[test]
    fn get_value_of_int_literal() {
        let (tree, root) = parse("92");

        let int_literal = match root.tail_expr(&tree) {
            Some(Expr::IntLiteral(int_literal)) => int_literal,
            _ => unreachable!(),
        };

        assert_eq!(int_literal.value(&tree).unwrap().text(&tree), "92");
    }

    #[test]
    fn get_components_of_string_literal() {
        let (tree, root) = parse(r#""\"👀\"""#);

        let string_literal = match root.tail_expr(&tree) {
            Some(Expr::StringLiteral(string_literal)) => string_literal,
            _ => unreachable!(),
        };

        let mut components = string_literal.components(&tree);

        let escaped_quote = match components.next() {
            Some(StringComponent::Escape(escape)) => escape,
            _ => unreachable!(),
        };
        assert_eq!(escaped_quote.text(&tree), "\\\"");

        let eyes = match components.next() {
            Some(StringComponent::Contents(contents)) => contents,
            _ => unreachable!(),
        };
        assert_eq!(eyes.text(&tree), "👀");

        let escaped_quote = match components.next() {
            Some(StringComponent::Escape(escape)) => escape,
            _ => unreachable!(),
        };
        assert_eq!(escaped_quote.text(&tree), "\\\"");

        assert!(components.next().is_none());
    }

    #[test]
    fn get_block_statements_and_tail_expr() {
        let (tree, root) = parse("{ let a = 10; let b = a * {a - 1}; b + 5 }");

        let block = match root.tail_expr(&tree) {
            Some(Expr::Block(block)) => block,
            _ => unreachable!(),
        };

        let mut statements = block.statements(&tree);

        assert!(matches!(statements.next(), Some(Statement::LocalDef(_))));
        assert!(matches!(statements.next(), Some(Statement::LocalDef(_))));
        assert!(statements.next().is_none());

        assert!(matches!(block.tail_expr(&tree), Some(Expr::Binary(_))));
    }

    #[test]
    fn get_function_name() {
        let (tree, root) = parse("fnc a -> {};");
        let def = root.defs(&tree).next().unwrap();

        let Def::Function(function) = def;

        assert_eq!(function.name(&tree).unwrap().text(&tree), "a");
    }

    #[test]
    fn get_function_params() {
        let (tree, root) = parse("fnc add(x: s32, y: s32) -> {};");
        let def = root.defs(&tree).next().unwrap();

        let Def::Function(function) = def;

        let mut params = function.param_list(&tree).unwrap().params(&tree);

        let param = params.next().unwrap();
        assert_eq!(param.name(&tree).unwrap().text(&tree), "x");
        assert_eq!(param.ty(&tree).unwrap().name(&tree).unwrap().text(&tree), "s32");

        let param = params.next().unwrap();
        assert_eq!(param.name(&tree).unwrap().text(&tree), "y");
        assert_eq!(param.ty(&tree).unwrap().name(&tree).unwrap().text(&tree), "s32");

        assert!(params.next().is_none());
    }

    #[test]
    fn get_function_return_ty() {
        let (tree, root) = parse("fnc four: s32 -> 4;");
        let def = root.defs(&tree).next().unwrap();

        let Def::Function(function) = def;

        assert_eq!(
            function.return_ty(&tree).unwrap().ty(&tree).unwrap().name(&tree).unwrap().text(&tree),
            "s32"
        );
    }

    #[test]
    fn get_function_body() {
        let (tree, root) = parse("fnc nothing -> {};");
        let def = root.defs(&tree).next().unwrap();

        let Def::Function(function) = def;

        let block = match function.body(&tree).unwrap() {
            Expr::Block(block) => block,
            _ => unreachable!(),
        };

        assert!(block.statements(&tree).next().is_none());
    }

    #[test]
    fn get_function_docs() {
        let (tree, root) = parse(
            "
                ## Definitely prints something to the screen.
                ##
                ## Does not actually print something to the screen.
                fnc print(s: string) -> {};
            ",
        );
        let def = root.defs(&tree).next().unwrap();

        let Def::Function(function) = def;

        let docs = function.docs(&tree).unwrap();
        let mut doc_comments = docs.doc_comments(&tree);
        assert_eq!(
            doc_comments.next().unwrap().contents(&tree).unwrap().text(&tree),
            " Definitely prints something to the screen."
        );
        assert!(doc_comments.next().unwrap().contents(&tree).is_none());
        assert_eq!(
            doc_comments.next().unwrap().contents(&tree).unwrap().text(&tree),
            " Does not actually print something to the screen."
        );
        assert!(doc_comments.next().is_none());
    }
}
