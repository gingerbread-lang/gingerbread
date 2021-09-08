pub mod validation;

use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use text_size::TextRange;

pub trait AstNode: Sized {
    fn cast(node: SyntaxNode) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode;
}

pub trait AstToken: Sized {
    fn cast(token: SyntaxToken) -> Option<Self>;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }

    fn range(&self) -> TextRange {
        self.syntax().text_range()
    }
}

macro_rules! def_ast_node {
    ($kind:ident) => {
        pub struct $kind(SyntaxNode);

        impl AstNode for $kind {
            fn cast(node: SyntaxNode) -> Option<Self> {
                (node.kind() == SyntaxKind::$kind).then(|| Self(node))
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };
}

macro_rules! def_ast_token {
    ($kind:ident) => {
        pub struct $kind(SyntaxToken);

        impl AstToken for $kind {
            fn cast(token: SyntaxToken) -> Option<Self> {
                (token.kind() == SyntaxKind::$kind).then(|| Self(token))
            }

            fn syntax(&self) -> &SyntaxToken {
                &self.0
            }
        }
    };
}

def_ast_node!(Root);

impl Root {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        nodes(self)
    }
}

pub enum Stmt {
    VarDef(VarDef),
    Expr(Expr),
}

impl AstNode for Stmt {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::VarDef => Some(Self::VarDef(VarDef(node))),
            SyntaxKind::BinExpr => Some(Self::Expr(Expr::Bin(BinExpr(node)))),
            SyntaxKind::ParenExpr => Some(Self::Expr(Expr::Paren(ParenExpr(node)))),
            SyntaxKind::VarRef => Some(Self::Expr(Expr::VarRef(VarRef(node)))),
            SyntaxKind::IntLiteral => Some(Self::Expr(Expr::IntLiteral(IntLiteral(node)))),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::VarDef(var_def) => var_def.syntax(),
            Self::Expr(expr) => expr.syntax(),
        }
    }
}

def_ast_node!(VarDef);

impl VarDef {
    pub fn name(&self) -> Option<Ident> {
        token(self)
    }

    pub fn value(&self) -> Option<Expr> {
        node(self)
    }
}

pub enum Expr {
    Bin(BinExpr),
    Paren(ParenExpr),
    VarRef(VarRef),
    IntLiteral(IntLiteral),
}

impl AstNode for Expr {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::BinExpr => Some(Self::Bin(BinExpr(node))),
            SyntaxKind::ParenExpr => Some(Self::Paren(ParenExpr(node))),
            SyntaxKind::VarRef => Some(Self::VarRef(VarRef(node))),
            SyntaxKind::IntLiteral => Some(Self::IntLiteral(IntLiteral(node))),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Bin(bin_expr) => bin_expr.syntax(),
            Self::Paren(paren_expr) => paren_expr.syntax(),
            Self::VarRef(var_ref) => var_ref.syntax(),
            Self::IntLiteral(int_literal) => int_literal.syntax(),
        }
    }
}

def_ast_node!(BinExpr);

impl BinExpr {
    pub fn lhs(&self) -> Option<Expr> {
        node(self)
    }

    pub fn rhs(&self) -> Option<Expr> {
        nodes(self).nth(1)
    }

    pub fn op(&self) -> Option<Op> {
        token(self)
    }
}

def_ast_node!(ParenExpr);

impl ParenExpr {
    pub fn inner(&self) -> Option<Expr> {
        node(self)
    }
}

def_ast_node!(VarRef);

impl VarRef {
    pub fn name(&self) -> Option<Ident> {
        token(self)
    }
}

def_ast_node!(IntLiteral);

impl IntLiteral {
    pub fn value(&self) -> Option<Int> {
        token(self)
    }
}

pub enum Op {
    Add(Plus),
    Sub(Hyphen),
    Mul(Asterisk),
    Div(Slash),
}

impl AstToken for Op {
    fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            SyntaxKind::Plus => Some(Self::Add(Plus(token))),
            SyntaxKind::Hyphen => Some(Self::Sub(Hyphen(token))),
            SyntaxKind::Asterisk => Some(Self::Mul(Asterisk(token))),
            SyntaxKind::Slash => Some(Self::Div(Slash(token))),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxToken {
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

fn nodes<Parent: AstNode, Child: AstNode>(node: &Parent) -> impl Iterator<Item = Child> {
    node.syntax().children().filter_map(Child::cast)
}

fn node<Parent: AstNode, Child: AstNode>(node: &Parent) -> Option<Child> {
    node.syntax().children().find_map(Child::cast)
}

fn token<Node: AstNode, Token: AstToken>(node: &Node) -> Option<Token> {
    node.syntax().children_with_tokens().filter_map(SyntaxElement::into_token).find_map(Token::cast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cast_root() {
        let syntax = parser::parse(lexer::lex("")).syntax_node();
        assert!(Root::cast(syntax).is_some());
    }

    #[test]
    fn get_stmts() {
        let syntax = parser::parse(lexer::lex("let a = b\na")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        assert_eq!(root.stmts().count(), 2);
    }

    #[test]
    fn inspect_stmt_and_expr_kind() {
        let syntax = parser::parse(lexer::lex("let foo = bar\nbaz * quuz")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        let mut stmts = root.stmts();
        let var_def = stmts.next().unwrap();
        let expr = stmts.next().unwrap();
        assert!(stmts.next().is_none());

        match var_def {
            Stmt::VarDef(_) => {}
            _ => unreachable!(),
        }

        match expr {
            Stmt::Expr(Expr::Bin(_)) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn get_name_of_var_def() {
        let syntax = parser::parse(lexer::lex("let a = 10")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();

        let var_def = match stmt {
            Stmt::VarDef(var_def) => var_def,
            _ => unreachable!(),
        };

        assert_eq!(var_def.name().unwrap().text(), "a");
    }

    #[test]
    fn get_value_of_var_def() {
        let syntax = parser::parse(lexer::lex("let foo = 5")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();

        let var_def = match stmt {
            Stmt::VarDef(var_def) => var_def,
            _ => unreachable!(),
        };

        match var_def.value() {
            Some(Expr::IntLiteral(_)) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn get_inner_expr_of_paren_expr() {
        let syntax = parser::parse(lexer::lex("(1)")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();

        let paren_expr = match stmt {
            Stmt::Expr(Expr::Paren(paren_expr)) => paren_expr,
            _ => unreachable!(),
        };

        match paren_expr.inner() {
            Some(Expr::IntLiteral(_)) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn get_lhs_and_rhs_of_bin_expr() {
        let syntax = parser::parse(lexer::lex("foo * 2")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();

        let bin_expr = match stmt {
            Stmt::Expr(Expr::Bin(bin_expr)) => bin_expr,
            _ => unreachable!(),
        };

        match bin_expr.lhs() {
            Some(Expr::VarRef(_)) => {}
            _ => unreachable!(),
        }

        match bin_expr.rhs() {
            Some(Expr::IntLiteral(_)) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn get_operator_of_bin_expr() {
        let syntax = parser::parse(lexer::lex("a + b")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();

        let bin_expr = match stmt {
            Stmt::Expr(Expr::Bin(bin_expr)) => bin_expr,
            _ => unreachable!(),
        };

        match bin_expr.op() {
            Some(Op::Add(_)) => {}
            _ => unreachable!(),
        }
    }

    #[test]
    fn get_name_of_var_ref() {
        let syntax = parser::parse(lexer::lex("idx")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();

        let var_ref = match stmt {
            Stmt::Expr(Expr::VarRef(var_ref)) => var_ref,
            _ => unreachable!(),
        };

        assert_eq!(var_ref.name().unwrap().text(), "idx");
    }

    #[test]
    fn get_value_of_int_literal() {
        let syntax = parser::parse(lexer::lex("92")).syntax_node();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();

        let int_literal = match stmt {
            Stmt::Expr(Expr::IntLiteral(int_literal)) => int_literal,
            _ => unreachable!(),
        };

        assert_eq!(int_literal.value().unwrap().text(), "92");
    }
}
