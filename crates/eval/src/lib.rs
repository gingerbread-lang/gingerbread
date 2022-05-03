mod codegen;
use self::codegen::Ctx;

use rustc_hash::FxHashMap;

pub fn eval(
    fqn: hir::Fqn,
    bodies_map: FxHashMap<hir::Name, hir::Bodies>,
    tys_map: FxHashMap<hir::Name, hir_ty::InferenceResult>,
    world_index: &hir::WorldIndex,
) -> Val {
    let entry_point_return_ty = {
        let function = world_index.get_function(fqn).unwrap();
        function.return_ty
    };

    let ctx = Ctx::new(bodies_map, tys_map, world_index, fqn);

    let mut store = wasmtime::Store::<()>::default();
    let module = wasmtime::Module::new(store.engine(), ctx.finish()).unwrap();
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let main = instance.get_func(&mut store, "main").unwrap();

    let num_results = if entry_point_return_ty == hir::Ty::Unit { 0 } else { 1 };
    let mut results = vec![wasmtime::Val::I32(0); num_results];
    main.call(&mut store, &[], &mut results).unwrap();

    match results.get(0).cloned() {
        Some(wasmtime::Val::I32(n)) => match entry_point_return_ty {
            hir::Ty::S32 => Val::S32(n),
            hir::Ty::String => {
                let mut len = [0; std::mem::size_of::<i32>()];
                instance
                    .get_memory(&mut store, "memory")
                    .unwrap()
                    .read(&mut store, n as usize, &mut len)
                    .unwrap();

                let len = i32::from_le_bytes(len);
                let len = len.try_into().unwrap();
                let mut string = vec![0; len];
                instance
                    .get_memory(&mut store, "memory")
                    .unwrap()
                    .read(&mut store, n as usize + std::mem::size_of::<i32>(), &mut string)
                    .unwrap();

                Val::String(String::from_utf8(string).unwrap())
            }
            _ => unreachable!(),
        },

        None => {
            assert_eq!(entry_point_return_ty, hir::Ty::Unit);
            Val::Nil
        }
        _ => unreachable!(),
    }
}

pub fn compile(
    fqn: hir::Fqn,
    bodies_map: FxHashMap<hir::Name, hir::Bodies>,
    tys_map: FxHashMap<hir::Name, hir_ty::InferenceResult>,
    world_index: &hir::WorldIndex,
) -> Vec<u8> {
    Ctx::new(bodies_map, tys_map, world_index, fqn).finish()
}

#[derive(Debug)]
pub enum Val {
    Nil,
    S32(i32),
    String(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use expect_test::{expect, Expect};
    use interner::Interner;

    fn check<const N: usize>(modules: [(&str, &str); N], expect: Expect) {
        let mut analysis_results = FxHashMap::default();
        let mut interner = Interner::default();
        let mut world_index = hir::WorldIndex::default();

        for (module, text) in &modules {
            let tokens = lexer::lex(text);
            let parse = parser::parse_source_file(&tokens, text);
            assert!(parse.errors().is_empty());

            let tree = parse.into_syntax_tree();
            let root = ast::Root::cast(tree.root(), &tree).unwrap();
            assert!(ast::validation::validate(root, &tree).is_empty());

            let (index, d) = hir::index(root, &tree, &world_index, &mut interner);
            assert!(d.is_empty());

            world_index.add_module(hir::Name(interner.intern(module)), index.clone());
            analysis_results.insert(module, (tree, root, index));
        }

        let mut bodies_map = FxHashMap::default();
        let mut tys_map = FxHashMap::default();

        for (module, (tree, root, index)) in analysis_results {
            let (bodies, _) = hir::lower(root, &tree, &index, &world_index, &mut interner);

            let (inference, d) = hir_ty::infer_all(&bodies, &index, &world_index);
            assert!(d.is_empty());

            bodies_map.insert(hir::Name(interner.intern(module)), bodies);
            tys_map.insert(hir::Name(interner.intern(module)), inference);
        }

        let result = eval(
            hir::Fqn {
                module: hir::Name(interner.intern("main")),
                function: hir::Name(interner.intern("main")),
            },
            bodies_map,
            tys_map,
            &world_index,
        );

        expect.assert_eq(&format!("{:?}", result));
    }

    #[test]
    fn empty() {
        check(
            [(
                "main",
                r#"
                    fnc main -> {};
                "#,
            )],
            expect![["Nil"]],
        );
    }

    #[test]
    fn return_constant_s32() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> 10;
                "#,
            )],
            expect![["S32(10)"]],
        );
    }

    #[test]
    fn binary_expr() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> 1 + 2 * 5;
                "#,
            )],
            expect![["S32(11)"]],
        );
    }

    #[test]
    fn local() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> {
                        let foo = 5;
                        foo
                    };
                "#,
            )],
            expect![["S32(5)"]],
        );
    }

    #[test]
    fn nested_blocks() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> {
                        let d = {
                            let c = {
                                let b = {
                                    let a = 10;
                                    a - 5
                                };
                                b + 22
                            };
                            c / 2
                        };
                        d * 5
                    };
                "#,
            )],
            expect![["S32(65)"]],
        );
    }

    #[test]
    fn call_with_no_params_or_return_ty() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> {
                        nil;
                        1
                    };
                    fnc nil -> {};
                "#,
            )],
            expect![["S32(1)"]],
        );
    }

    #[test]
    fn call_with_no_params_but_with_return_ty() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> n;
                    fnc n: s32 -> 5;
                "#,
            )],
            expect![["S32(5)"]],
        );
    }

    #[test]
    fn call_with_params_and_return_ty() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> add 10, 20;
                    fnc add(x: s32, y: s32): s32 -> x + y;
                "#,
            )],
            expect![["S32(30)"]],
        );
    }

    #[test]
    fn create_string() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> {
                        let s = "foo";
                        0
                    };
                "#,
            )],
            expect![["S32(0)"]],
        );
    }

    #[test]
    fn return_string() {
        check(
            [(
                "main",
                r#"
                    fnc main: string -> "hello";
                "#,
            )],
            expect![[r#"String("hello")"#]],
        );
    }

    #[test]
    fn multiple_strings() {
        check(
            [(
                "main",
                r#"
                    fnc main: string -> {
                        let a = "foo";
                        let b = "bar";
                        let c = "baz";
                        let d = "quux";
                        c
                    };
                "#,
            )],
            expect![[r#"String("baz")"#]],
        );
    }
}
