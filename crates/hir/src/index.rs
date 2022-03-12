use crate::WorldIndex;
use ast::{AstNode, AstToken};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt;
use syntax::SyntaxTree;
use text_size::TextRange;

#[derive(Clone)]
pub struct Index {
    pub(crate) functions: HashMap<Name, Function>,
}

impl Index {
    pub fn get_function(&self, name: &Name) -> Option<&Function> {
        self.functions.get(name)
    }

    pub fn functions(&self) -> impl Iterator<Item = &Name> {
        self.functions.keys()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub params: Vec<Param>,
    pub return_ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Option<Name>,
    pub ty: Ty,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ty {
    Unknown,
    S32,
    String,
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(pub String);

pub fn index(
    root: ast::Root,
    tree: &SyntaxTree,
    world_index: &WorldIndex,
) -> (Index, Vec<IndexingDiagnostic>) {
    let mut functions = HashMap::new();
    let mut diagnostics = Vec::new();

    for def in root.defs(tree) {
        match def {
            ast::Def::Function(function) => {
                let name = match function.name(tree) {
                    Some(ident) => Name(ident.text(tree).to_string()),
                    None => continue,
                };

                let mut params = Vec::new();

                if let Some(param_list) = function.param_list(tree) {
                    for param in param_list.params(tree) {
                        let name = param.name(tree).map(|ident| Name(ident.text(tree).to_string()));
                        let ty = lower_ty(param.ty(tree), tree, world_index, &mut diagnostics);

                        params.push(Param { name, ty })
                    }
                }

                let return_ty = match function.return_ty(tree) {
                    Some(return_ty) => {
                        lower_ty(return_ty.ty(tree), tree, world_index, &mut diagnostics)
                    }
                    None => Ty::Unit,
                };

                let name_string_clone = name.0.clone();
                match functions.entry(name) {
                    Entry::Occupied(_) => diagnostics.push(IndexingDiagnostic {
                        kind: IndexingDiagnosticKind::FunctionAlreadyDefined {
                            name: name_string_clone,
                        },
                        range: function.range(tree),
                    }),
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(Function { params, return_ty });
                    }
                }
            }
        }
    }

    (Index { functions }, diagnostics)
}

fn lower_ty(
    ty: Option<ast::Ty>,
    tree: &SyntaxTree,
    world_index: &WorldIndex,
    diagnostics: &mut Vec<IndexingDiagnostic>,
) -> Ty {
    let ident = match ty.and_then(|ty| ty.name(tree)) {
        Some(ident) => ident,
        None => return Ty::Unknown,
    };

    let name = Name(ident.text(tree).to_string());
    if let Some(kind) = world_index.get_ty(&name) {
        return kind;
    }

    diagnostics.push(IndexingDiagnostic {
        kind: IndexingDiagnosticKind::UndefinedTy { name: name.0 },
        range: ident.range(tree),
    });

    Ty::Unknown
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexingDiagnostic {
    pub kind: IndexingDiagnosticKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IndexingDiagnosticKind {
    FunctionAlreadyDefined { name: String },
    UndefinedTy { name: String },
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut functions: Vec<_> = self.functions.iter().collect();
        functions.sort_unstable_by_key(|(name, _)| &name.0);

        for (name, function) in functions {
            write!(f, "fnc {}", name.0)?;

            if !function.params.is_empty() {
                write!(f, "(")?;

                for (idx, param) in function.params.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}: {}", param.name.as_ref().map_or("?", |name| &name.0), param.ty)?;
                }

                write!(f, ")")?;
            }

            if function.return_ty != Ty::Unit {
                write!(f, ": {}", function.return_ty)?;
            }

            writeln!(f, ";")?;
        }

        Ok(())
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown => write!(f, "?"),
            Self::S32 => write!(f, "s32"),
            Self::String => write!(f, "string"),
            Self::Unit => write!(f, "unit"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use expect_test::{expect, Expect};

    fn check<const N: usize>(
        input: &str,
        expect: Expect,
        expected_diagnostics: [(IndexingDiagnosticKind, std::ops::Range<u32>); N],
    ) {
        let tokens = lexer::lex(input);
        let tree = parser::parse_source_file(&tokens, input).into_syntax_tree();
        let root = ast::Root::cast(tree.root(), &tree).unwrap();
        let (index, actual_diagnostics) = index(root, &tree, &WorldIndex::default());

        expect.assert_eq(&format!("{:?}", index));

        let expected_diagnostics: Vec<_> = expected_diagnostics
            .into_iter()
            .map(|(kind, range)| IndexingDiagnostic {
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
    fn simple_function() {
        check(
            r#"
                fnc nil -> {};
            "#,
            expect![[r#"
                fnc nil;
            "#]],
            [],
        );
    }

    #[test]
    fn function_with_params() {
        check(
            r#"
                fnc foo(x: s32, y: s32) -> {};
            "#,
            expect![[r#"
                fnc foo(x: s32, y: s32);
            "#]],
            [],
        );
    }

    #[test]
    fn multiple_functions() {
        check(
            r#"
                fnc b -> {};
                fnc e -> {};
                fnc c -> {};
                fnc a -> {};
                fnc d -> {};
            "#,
            expect![[r#"
                fnc a;
                fnc b;
                fnc c;
                fnc d;
                fnc e;
            "#]],
            [],
        );
    }

    #[test]
    fn function_with_missing_name() {
        check(
            r#"
                fnc: s32 -> 10;
            "#,
            expect![["\n"]],
            [],
        );
    }

    #[test]
    fn function_with_missing_param_name() {
        check(
            r#"
                fnc hello(: string): string -> "Hello, world!";
            "#,
            expect![[r#"
                fnc hello(?: string): string;
            "#]],
            [],
        );
    }

    #[test]
    fn function_with_missing_param_ty() {
        check(
            r#"
                fnc foo(x:) -> {};
            "#,
            expect![[r#"
                fnc foo(x: ?);
            "#]],
            [],
        );
    }

    #[test]
    fn function_with_return_ty() {
        check(
            r#"
                fnc five: s32 -> 5;
            "#,
            expect![[r#"
                fnc five: s32;
            "#]],
            [],
        );
    }

    #[test]
    fn function_with_missing_return_ty() {
        check(
            r#"
                fnc foo: ;
            "#,
            expect![[r#"
                fnc foo: ?;
            "#]],
            [],
        );
    }

    #[test]
    fn function_with_undefined_return_ty() {
        check(
            r#"
                fnc foo: bar;
            "#,
            expect![[r#"
                fnc foo: ?;
            "#]],
            [(IndexingDiagnosticKind::UndefinedTy { name: "bar".to_string() }, 26..29)],
        );
    }

    #[test]
    fn functions_with_same_name() {
        check(
            r#"
                fnc a -> {};
                fnc a: string -> "hello";
                fnc a(x: s32): s32 -> x;
            "#,
            expect![[r#"
                fnc a;
            "#]],
            [
                (IndexingDiagnosticKind::FunctionAlreadyDefined { name: "a".to_string() }, 46..71),
                (IndexingDiagnosticKind::FunctionAlreadyDefined { name: "a".to_string() }, 88..112),
            ],
        );
    }
}
