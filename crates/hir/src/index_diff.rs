use crate::{Index, Name};
use interner::Interner;
use std::collections::HashSet;

pub fn diff(old_index: &Index, new_index: &Index) -> Diff {
    let mut deleted_or_changed = HashSet::new();

    for (&name, old_function) in &old_index.functions {
        match new_index.get_function(name) {
            // the function is in both the old and new indexes, and has not been changed
            Some(new_function) if old_function == new_function => {}

            // the function is in both the old and new indexes, and has been changed
            // or
            // the function is only in the old index (it has been deleted)
            Some(_) | None => {
                deleted_or_changed.insert(name);
            }
        }
    }

    Diff { deleted_or_changed }
}

pub struct Diff {
    deleted_or_changed: HashSet<Name>,
}

impl Diff {
    pub fn deleted_or_changed(&self) -> &HashSet<Name> {
        &self.deleted_or_changed
    }

    pub fn debug(&self, interner: &Interner) -> String {
        let mut s = String::new();

        let mut deleted_or_changed: Vec<_> = self.deleted_or_changed.iter().collect();
        deleted_or_changed.sort_unstable();

        for (idx, function) in deleted_or_changed.iter().enumerate() {
            if idx != 0 {
                s.push_str(", ");
            }

            s.push_str(interner.lookup(function.0));
        }

        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{index, WorldIndex};
    use ast::AstNode;
    use expect_test::{expect, Expect};

    fn check(original: &str, changed: &str, expect: Expect) {
        let mut interner = Interner::default();

        let mut index_input = |input| {
            let tokens = lexer::lex(input);
            let tree = parser::parse_source_file(&tokens, input).into_syntax_tree();
            let root = ast::Root::cast(tree.root(), &tree).unwrap();
            let (index, _) = index(root, &tree, &WorldIndex::default(), &mut interner);
            index
        };

        let old = index_input(original);
        let new = index_input(changed);

        expect.assert_eq(&diff(&old, &new).debug(&interner))
    }

    #[test]
    fn empty() {
        check("", "", expect![[""]]);
    }

    #[test]
    fn add() {
        check(
            r#"
                fnc a -> {};
            "#,
            r#"
                fnc a -> {};
                fnc b -> {};
                fnc c -> {};
            "#,
            expect![[""]],
        );
    }

    #[test]
    fn delete() {
        check(
            r#"
                fnc b -> {};
                fnc a -> {};
                fnc d -> {};
                fnc c -> {};
            "#,
            r#"
                fnc a -> {};
            "#,
            expect![["b, d, c"]],
        );
    }

    #[test]
    fn change() {
        check(
            r#"
                fnc foo: s32 -> 1;
                fnc bar(x: s32): s32 -> x;
            "#,
            r#"
                fnc foo -> {};
                fnc bar(x: string): string -> x;
            "#,
            expect![["foo, bar"]],
        );
    }

    #[test]
    fn change_body() {
        check(
            r#"
                fnc foo: s32 -> 1;
                fnc bar: s32 -> 1;
                "#,
            r#"
                fnc foo: s32 -> 2;
                fnc bar: s32 -> 1;
            "#,
            expect![[""]],
        );
    }
}
