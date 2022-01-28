use ast::{AstNode, AstToken};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt;
use text_size::TextRange;

pub struct Index {
    functions: HashMap<Name, Function>,
}

impl Index {
    pub fn get_function(&self, name: &Name) -> Option<&Function> {
        self.functions.get(name)
    }
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Param>,
    pub return_ty: Ty,
}

#[derive(Debug)]
pub struct Param {
    pub name: Option<Name>,
    pub ty: Ty,
}

#[derive(Debug, PartialEq)]
pub enum Ty {
    Missing,
    Named(Name),
    Unit,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Name(String);

pub fn index(root: &ast::Root) -> (Index, Vec<IndexingError>) {
    let mut functions = HashMap::new();
    let mut errors = Vec::new();

    let lower_ty = |ty: Option<ast::Ty>| match ty.and_then(|ty| ty.name()) {
        Some(ident) => Ty::Named(Name(ident.text().to_string())),
        None => Ty::Missing,
    };

    for def in root.defs() {
        match def {
            ast::Def::Function(function) => {
                let name = match function.name() {
                    Some(ident) => Name(ident.text().to_string()),
                    None => continue,
                };

                let mut params = Vec::new();

                if let Some(param_list) = function.param_list() {
                    for param in param_list.params() {
                        let name = param.name().map(|ident| Name(ident.text().to_string()));
                        let ty = lower_ty(param.ty());

                        params.push(Param { name, ty })
                    }
                }

                let return_ty = match function.return_ty() {
                    Some(return_ty) => lower_ty(return_ty.ty()),
                    None => Ty::Unit,
                };

                let name_string_clone = name.0.clone();
                match functions.entry(name) {
                    Entry::Occupied(_) => errors.push(IndexingError {
                        kind: IndexingErrorKind::FunctionAlreadyDefined { name: name_string_clone },
                        range: function.range(),
                    }),
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(Function { params, return_ty });
                    }
                }
            }
        }
    }

    (Index { functions }, errors)
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexingError {
    pub kind: IndexingErrorKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IndexingErrorKind {
    FunctionAlreadyDefined { name: String },
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // There is no need to preserve the order of items in the index,
        // so we just use a regular HashMap. To preserve stability,
        // we sort all definitions alphabetically for debug output.

        let mut functions: Vec<_> = self.functions.iter().collect();
        functions.sort_unstable_by_key(|(name, _)| &name.0);

        let display_ty = |ty: &Ty| match ty {
            Ty::Missing => "?".to_string(),
            Ty::Named(name) => name.0.clone(),
            Ty::Unit => "unit".to_string(),
        };

        for (name, function) in functions {
            write!(f, "fnc {}", name.0)?;

            if !function.params.is_empty() {
                write!(f, "(")?;

                for (idx, param) in function.params.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }

                    write!(
                        f,
                        "{}: {}",
                        param.name.as_ref().map_or("?", |name| &name.0),
                        display_ty(&param.ty)
                    )?;
                }

                write!(f, ")")?;
            }

            if function.return_ty != Ty::Unit {
                write!(f, ": {}", display_ty(&function.return_ty))?;
            }

            writeln!(f, ";")?;
        }

        Ok(())
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
        expected_errors: [(IndexingErrorKind, std::ops::Range<u32>); N],
    ) {
        let tokens = lexer::lex(input);
        let parse = parser::parse_source_file(&tokens);
        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let (index, actual_errors) = index(&root);

        expect.assert_eq(&format!("{:?}", index));

        let expected_errors: Vec<_> = expected_errors
            .into_iter()
            .map(|(kind, range)| IndexingError {
                kind,
                range: TextRange::new(range.start.into(), range.end.into()),
            })
            .collect();

        assert_eq!(expected_errors, actual_errors);
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
                (IndexingErrorKind::FunctionAlreadyDefined { name: "a".to_string() }, 46..71),
                (IndexingErrorKind::FunctionAlreadyDefined { name: "a".to_string() }, 88..112),
            ],
        );
    }
}
