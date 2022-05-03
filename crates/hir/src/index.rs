use crate::WorldIndex;
use ast::{AstNode, AstToken};
use interner::{Interner, Key};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::fmt;
use syntax::SyntaxTree;
use text_size::TextRange;

#[derive(Clone)]
pub struct Index {
    pub(crate) functions: FxHashMap<Name, Function>,
    pub(crate) range_info: FxHashMap<Name, RangeInfo>,
    tys: FxHashSet<ast::Ident>,
}

impl Index {
    pub fn get_function(&self, name: Name) -> Option<&Function> {
        self.functions.get(&name)
    }

    pub fn range_info(&self, name: Name) -> RangeInfo {
        self.range_info[&name]
    }

    pub fn functions(&self) -> impl Iterator<Item = Name> + '_ {
        self.functions.keys().copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Name, RangeInfo)> + '_ {
        self.range_info.iter().map(|(n, r)| (*n, *r))
    }

    pub fn is_ident_ty(&self, ident: ast::Ident) -> bool {
        self.tys.contains(&ident)
    }

    fn shrink_to_fit(&mut self) {
        let Self { functions, range_info, tys } = self;
        functions.shrink_to_fit();
        range_info.shrink_to_fit();
        tys.shrink_to_fit();
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub params: Vec<Param>,
    pub return_ty: Ty,
}

#[derive(Debug, Clone, Copy)]
pub struct RangeInfo {
    pub whole: TextRange,
    pub name: TextRange,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(pub Key);

pub fn index(
    root: ast::Root,
    tree: &SyntaxTree,
    world_index: &WorldIndex,
    interner: &mut Interner,
) -> (Index, Vec<IndexingDiagnostic>) {
    let mut functions = FxHashMap::default();
    let mut range_info = FxHashMap::default();
    let mut tys = FxHashSet::default();
    let mut diagnostics = Vec::new();

    for def in root.defs(tree) {
        match def {
            ast::Def::Function(function) => {
                let name_token = match function.name(tree) {
                    Some(ident) => ident,
                    None => continue,
                };

                let name = Name(interner.intern(name_token.text(tree)));

                let mut params = Vec::new();

                if let Some(param_list) = function.param_list(tree) {
                    for param in param_list.params(tree) {
                        let name =
                            param.name(tree).map(|ident| Name(interner.intern(ident.text(tree))));

                        let ty = lower_ty(
                            param.ty(tree),
                            tree,
                            world_index,
                            &mut tys,
                            interner,
                            &mut diagnostics,
                        );

                        params.push(Param { name, ty })
                    }
                }

                let return_ty = match function.return_ty(tree) {
                    Some(return_ty) => lower_ty(
                        return_ty.ty(tree),
                        tree,
                        world_index,
                        &mut tys,
                        interner,
                        &mut diagnostics,
                    ),
                    None => Ty::Unit,
                };

                match functions.entry(name) {
                    Entry::Occupied(_) => diagnostics.push(IndexingDiagnostic {
                        kind: IndexingDiagnosticKind::FunctionAlreadyDefined { name: name.0 },
                        range: function.range(tree),
                    }),
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(Function { params, return_ty });
                        range_info.insert(
                            name,
                            RangeInfo { whole: function.range(tree), name: name_token.range(tree) },
                        );
                    }
                }
            }
        }
    }

    let mut index = Index { functions, range_info, tys };
    index.shrink_to_fit();

    (index, diagnostics)
}

fn lower_ty(
    ty: Option<ast::Ty>,
    tree: &SyntaxTree,
    world_index: &WorldIndex,
    tys: &mut FxHashSet<ast::Ident>,
    interner: &mut Interner,
    diagnostics: &mut Vec<IndexingDiagnostic>,
) -> Ty {
    let ident = match ty.and_then(|ty| ty.name(tree)) {
        Some(ident) => ident,
        None => return Ty::Unknown,
    };

    let name = Name(interner.intern(ident.text(tree)));
    if let Some(kind) = world_index.get_ty(name) {
        tys.insert(ident);
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
    FunctionAlreadyDefined { name: Key },
    UndefinedTy { name: Key },
}

impl Index {
    pub fn debug(&self, interner: &Interner) -> String {
        let mut s = String::new();

        let mut functions: Vec<_> = self.functions.iter().collect();
        functions.sort_unstable_by_key(|(name, _)| *name);

        for (name, function) in functions {
            s.push_str(&format!("fnc {}", interner.lookup(name.0)));

            if !function.params.is_empty() {
                s.push('(');

                for (idx, param) in function.params.iter().enumerate() {
                    if idx != 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&format!(
                        "{}: {}",
                        param.name.as_ref().map_or("?", |name| interner.lookup(name.0)),
                        param.ty
                    ));
                }

                s.push(')');
            }

            if function.return_ty != Ty::Unit {
                s.push_str(&format!(": {}", function.return_ty));
            }

            s.push_str(";\n");
        }

        s
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
        expected_diagnostics: impl Fn(
            &mut Interner,
        ) -> [(IndexingDiagnosticKind, std::ops::Range<u32>); N],
    ) {
        let mut interner = Interner::default();
        let tokens = lexer::lex(input);
        let tree = parser::parse_source_file(&tokens, input).into_syntax_tree();
        let root = ast::Root::cast(tree.root(), &tree).unwrap();
        let (index, actual_diagnostics) = index(root, &tree, &WorldIndex::default(), &mut interner);

        expect.assert_eq(&index.debug(&interner));

        let expected_diagnostics: Vec<_> = expected_diagnostics(&mut interner)
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
        check("", expect![["\n"]], |_| []);
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
            |_| [],
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
            |_| [],
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
                fnc b;
                fnc e;
                fnc c;
                fnc a;
                fnc d;
            "#]],
            |_| [],
        );
    }

    #[test]
    fn function_with_missing_name() {
        check(
            r#"
                fnc: s32 -> 10;
            "#,
            expect![["\n"]],
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |_| [],
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
            |i| [(IndexingDiagnosticKind::UndefinedTy { name: i.intern("bar") }, 26..29)],
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
            |i| {
                [
                    (
                        IndexingDiagnosticKind::FunctionAlreadyDefined { name: i.intern("a") },
                        46..71,
                    ),
                    (
                        IndexingDiagnosticKind::FunctionAlreadyDefined { name: i.intern("a") },
                        88..112,
                    ),
                ]
            },
        );
    }
}
