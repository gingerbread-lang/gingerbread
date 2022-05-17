use crate::{Name, Ty, WorldIndex};
use ast::{AstNode, AstToken};
use interner::{Interner, Key};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::fmt;
use syntax::SyntaxTree;
use text_size::TextRange;

#[derive(Clone)]
pub struct Index {
    pub(crate) definitions: FxHashMap<Name, Definition>,
    pub(crate) range_info: FxHashMap<Name, RangeInfo>,
    docs: FxHashMap<Name, Docs>,
    tys: FxHashSet<ast::Ident>,
}

impl Index {
    pub fn functions(&self) -> impl Iterator<Item = (Name, &Function)> {
        self.definitions.iter().filter_map(|(name, definition)| match definition {
            Definition::Function(f) => Some((*name, f)),
            Definition::Record(_) => None,
        })
    }

    pub fn get_definition(&self, name: Name) -> Option<&Definition> {
        self.definitions.get(&name)
    }

    pub fn range_info(&self, name: Name) -> RangeInfo {
        self.range_info[&name]
    }

    pub fn definition_names(&self) -> impl Iterator<Item = Name> + '_ {
        self.definitions.keys().copied()
    }

    pub fn function_names(&self) -> impl Iterator<Item = Name> + '_ {
        self.definitions.iter().filter_map(|(name, def)| match def {
            Definition::Function(_) => Some(*name),
            Definition::Record(_) => None,
        })
    }

    pub fn ranges(&self) -> impl Iterator<Item = (Name, RangeInfo)> + '_ {
        self.range_info.iter().map(|(n, r)| (*n, *r))
    }

    pub fn is_ident_ty(&self, ident: ast::Ident) -> bool {
        self.tys.contains(&ident)
    }

    fn shrink_to_fit(&mut self) {
        let Self { definitions, range_info, docs, tys } = self;
        definitions.shrink_to_fit();
        range_info.shrink_to_fit();
        docs.shrink_to_fit();
        tys.shrink_to_fit();
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Function(Function),
    Record(Record),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub params: Vec<Param>,
    pub return_ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub fields: Vec<Field>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Option<Name>,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
struct Docs {
    paras: Vec<String>,
}

pub fn index(
    root: ast::Root,
    tree: &SyntaxTree,
    world_index: &WorldIndex,
    interner: &mut Interner,
) -> (Index, Vec<IndexingDiagnostic>) {
    let mut ctx = Ctx {
        index: Index {
            definitions: FxHashMap::default(),
            range_info: FxHashMap::default(),
            docs: FxHashMap::default(),
            tys: FxHashSet::default(),
        },
        diagnostics: Vec::new(),
        tree,
        interner,
        world_index,
    };

    for def in root.defs(tree) {
        ctx.index_def(def);
    }

    ctx.index.shrink_to_fit();

    (ctx.index, ctx.diagnostics)
}

struct Ctx<'a> {
    index: Index,
    diagnostics: Vec<IndexingDiagnostic>,
    tree: &'a SyntaxTree,
    interner: &'a mut Interner,
    world_index: &'a WorldIndex,
}

impl Ctx<'_> {
    fn index_def(&mut self, def: ast::Def) {
        let result = match def {
            ast::Def::Function(function) => self.index_function(function),
            ast::Def::Record(record) => self.index_record(record),
        };

        let (definition, name, name_token, docs) = match result {
            IndexDefinitionResult::Ok { definition, name, name_token, docs } => {
                (definition, name, name_token, docs)
            }
            IndexDefinitionResult::NoName => return,
        };

        match self.index.definitions.entry(name) {
            Entry::Occupied(_) => self.diagnostics.push(IndexingDiagnostic {
                kind: IndexingDiagnosticKind::AlreadyDefined { name: name.0 },
                range: name_token.range(self.tree),
            }),
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(definition);
                self.index.range_info.insert(
                    name,
                    RangeInfo { whole: def.range(self.tree), name: name_token.range(self.tree) },
                );
            }
        }

        if let Some(d) = docs {
            let mut paras = vec![String::new()];

            for doc_comment in d.doc_comments(self.tree) {
                match doc_comment.contents(self.tree) {
                    Some(contents) => {
                        let contents = contents.text(self.tree).trim();
                        let last_para = &mut paras.last_mut().unwrap();

                        if !last_para.is_empty() {
                            last_para.push(' ');
                        }

                        last_para.push_str(contents);
                    }
                    None => paras.push(String::new()),
                }
            }

            self.index.docs.insert(name, Docs { paras });
        }
    }

    fn index_function(&mut self, function: ast::Function) -> IndexDefinitionResult {
        let name_token = match function.name(self.tree) {
            Some(ident) => ident,
            None => return IndexDefinitionResult::NoName,
        };
        let name = Name(self.interner.intern(name_token.text(self.tree)));

        let mut params = Vec::new();

        if let Some(param_list) = function.param_list(self.tree) {
            for param in param_list.params(self.tree) {
                let name = param
                    .name(self.tree)
                    .map(|ident| Name(self.interner.intern(ident.text(self.tree))));

                let ty = self.lower_ty(param.ty(self.tree));

                params.push(Param { name, ty });
            }
        }

        let return_ty = match function.return_ty(self.tree) {
            Some(return_ty) => self.lower_ty(return_ty.ty(self.tree)),
            None => Ty::Unit,
        };

        IndexDefinitionResult::Ok {
            definition: Definition::Function(Function { params, return_ty }),
            name,
            name_token,
            docs: function.docs(self.tree),
        }
    }

    fn index_record(&mut self, record: ast::Record) -> IndexDefinitionResult {
        let name_token = match record.name(self.tree) {
            Some(ident) => ident,
            None => return IndexDefinitionResult::NoName,
        };
        let name = Name(self.interner.intern(name_token.text(self.tree)));

        let mut fields = Vec::new();

        for field in record.fields(self.tree) {
            let name = field
                .name(self.tree)
                .map(|ident| Name(self.interner.intern(ident.text(self.tree))));

            let ty = self.lower_ty(field.ty(self.tree));

            fields.push(Field { name, ty });
        }

        IndexDefinitionResult::Ok {
            definition: Definition::Record(Record { fields }),
            name,
            name_token,
            docs: record.docs(self.tree),
        }
    }

    fn lower_ty(&mut self, ty: Option<ast::Ty>) -> Ty {
        let ident = match ty.and_then(|ty| ty.name(self.tree)) {
            Some(ident) => ident,
            None => return Ty::Unknown,
        };

        let name = Name(self.interner.intern(ident.text(self.tree)));

        if let Some(kind) = self.world_index.get_ty(name) {
            self.index.tys.insert(ident);
            return kind;
        }

        self.diagnostics.push(IndexingDiagnostic {
            kind: IndexingDiagnosticKind::UndefinedTy { name: name.0 },
            range: ident.range(self.tree),
        });

        Ty::Unknown
    }
}

enum IndexDefinitionResult {
    Ok { definition: Definition, name: Name, name_token: ast::Ident, docs: Option<ast::Docs> },
    NoName,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexingDiagnostic {
    pub kind: IndexingDiagnosticKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IndexingDiagnosticKind {
    AlreadyDefined { name: Key },
    UndefinedTy { name: Key },
}

impl Index {
    pub fn debug(&self, interner: &Interner) -> String {
        let mut s = String::new();

        let mut definitions: Vec<_> = self.definitions.iter().collect();
        definitions.sort_unstable_by_key(|(name, _)| *name);

        for (idx, (name, definition)) in definitions.iter().enumerate() {
            if let Some(docs) = self.docs.get(name) {
                if idx != 0 {
                    s.push('\n');
                }
                debug_docs(&mut s, docs);
            }

            match definition {
                Definition::Function(function) => {
                    debug_function(&mut s, **name, function, interner)
                }

                Definition::Record(record) => debug_record(&mut s, **name, record, interner),
            }
        }

        return s;

        fn debug_docs(s: &mut String, docs: &Docs) {
            s.push_str("# docs:\n");

            for (i, para) in docs.paras.iter().enumerate() {
                if i != 0 {
                    s.push_str("#\n");
                }

                for line in textwrap::wrap(para, 66) {
                    s.push_str(&format!("# {line}\n"));
                }
            }
        }

        fn debug_function(s: &mut String, name: Name, function: &Function, interner: &Interner) {
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

        fn debug_record(s: &mut String, name: Name, record: &Record, interner: &Interner) {
            s.push_str(&format!("rec {} {{", interner.lookup(name.0)));

            if !record.fields.is_empty() {
                s.push(' ');
                for (idx, field) in record.fields.iter().enumerate() {
                    if idx != 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&format!(
                        "{}: {}",
                        field.name.as_ref().map_or("?", |name| interner.lookup(name.0)),
                        field.ty
                    ));
                }
                s.push(' ');
            }

            s.push_str("};\n");
        }
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
    fn definitions_with_same_name() {
        check(
            r#"
                fnc a -> {};
                fnc a: string -> "hello";
                rec a {};
                fnc a(x: s32): s32 -> x;
            "#,
            expect![[r#"
                fnc a;
            "#]],
            |i| {
                [
                    (IndexingDiagnosticKind::AlreadyDefined { name: i.intern("a") }, 50..51),
                    (IndexingDiagnosticKind::AlreadyDefined { name: i.intern("a") }, 92..93),
                    (IndexingDiagnosticKind::AlreadyDefined { name: i.intern("a") }, 118..119),
                ]
            },
        );
    }

    #[test]
    fn empty_record() {
        check(
            r#"
                rec a {};
            "#,
            expect![[r#"
                rec a {};
            "#]],
            |_| [],
        );
    }

    #[test]
    fn record_with_fields() {
        check(
            r#"
                rec point { x: s32, y: s32 };
            "#,
            expect![[r#"
                rec point { x: s32, y: s32 };
            "#]],
            |_| [],
        );
    }

    #[test]
    fn record_with_missing_field_name() {
        check(
            r#"
                rec r { : string };
            "#,
            expect![[r#"
                rec r { ?: string };
            "#]],
            |_| [],
        );
    }

    #[test]
    fn definitions_with_docs() {
        check(
            r#"
                ## Increments a signed 32-bit integer.
                fnc inc(n: s32): s32 -> n + 1;

                ## An extremely useful record.
                rec signed_32_bit_integer { n: s32 };
            "#,
            expect![[r#"
                # docs:
                # Increments a signed 32-bit integer.
                fnc inc(n: s32): s32;

                # docs:
                # An extremely useful record.
                rec signed_32_bit_integer { n: s32 };
            "#]],
            |_| [],
        );
    }

    #[test]
    fn join_and_trim_lines_in_docs() {
        check(
            r#"
                ## Addition (usually signified by the plus symbol +)
                ## is one of the four basic operations of arithmetic,
                ##the other three being subtraction, multiplication and division.
                ##      The addition of two whole numbers results in
                ##    the total amount or sum of those values combined.
                ##
                ##The example in the adjacent image shows
                ##  a combination of three apples and two apples,
                ##making a total of five apples.
                fnc add(x: s32, y: s32): s32 -> x + y;
            "#,
            expect![[r#"
                # docs:
                # Addition (usually signified by the plus symbol +) is one of
                # the four basic operations of arithmetic, the other three being
                # subtraction, multiplication and division. The addition of two
                # whole numbers results in the total amount or sum of those values
                # combined.
                #
                # The example in the adjacent image shows a combination of three
                # apples and two apples, making a total of five apples.
                fnc add(x: s32, y: s32): s32;
            "#]],
            |_| [],
        );
    }
}
