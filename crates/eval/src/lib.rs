mod codegen;
use self::codegen::Ctx;

use std::collections::HashMap;

pub fn eval(
    name: (hir::Name, hir::Name),
    bodies_map: HashMap<hir::Name, hir::Bodies>,
    tys_map: HashMap<hir::Name, hir_ty::InferenceResult>,
    world_index: hir::WorldIndex,
) -> Option<wasmtime::Val> {
    let ctx = Ctx::new(bodies_map, tys_map, world_index, name);

    let mut store = wasmtime::Store::<()>::default();
    let module = wasmtime::Module::new(store.engine(), ctx.finish()).unwrap();
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let main = instance.get_func(&mut store, "main").unwrap();

    let mut results = vec![wasmtime::Val::I32(0); 1];
    main.call(&mut store, &[], &mut results).unwrap();

    let result = results.get(0).cloned();

    match result {
        Some(wasmtime::Val::I32(n)) => {
            let mut buffer = [0; 16];
            instance
                .get_memory(&mut store, "memory")
                .unwrap()
                .read(store, n as usize, &mut buffer)
                .unwrap();
            dbg!(buffer);
        }
        _ => unreachable!(),
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use expect_test::{expect, Expect};

    fn check<const N: usize>(modules: [(&str, &str); N], expect: Expect) {
        let mut analysis_results = HashMap::with_capacity(modules.len());
        let mut world_index = hir::WorldIndex::default();

        for (module, text) in &modules {
            let tokens = lexer::lex(text);
            let parse = parser::parse_source_file(&tokens);
            assert!(parse.errors().is_empty());

            let root = ast::Root::cast(parse.syntax_node()).unwrap();
            assert!(ast::validation::validate(&root).is_empty());

            let (index, d) = hir::index(&root, &world_index);
            assert!(d.is_empty());

            world_index.add_module(hir::Name(module.to_string()), index.clone());
            analysis_results.insert(module, (root, index));
        }

        let mut bodies_map = HashMap::with_capacity(modules.len());
        let mut tys_map = HashMap::with_capacity(modules.len());

        for (module, (root, index)) in analysis_results {
            let (bodies, _) = hir::lower(&root, &index, &world_index);

            let (inference, d) = hir_ty::infer_all(&bodies, &index, &world_index);
            assert!(d.is_empty());

            bodies_map.insert(hir::Name(module.to_string()), bodies);
            tys_map.insert(hir::Name(module.to_string()), inference);
        }

        let result = eval(
            (hir::Name("main".to_string()), hir::Name("main".to_string())),
            bodies_map,
            tys_map,
            world_index,
        );

        expect.assert_eq(&format!("{:?}", result));
    }

    // #[test]
    // fn empty() {
    //     check(
    //         [(
    //             "main",
    //             r#"
    //                 fnc main -> {};
    //             "#,
    //         )],
    //         expect![["None"]],
    //     );
    // }

    #[test]
    fn return_constant_s32() {
        check(
            [(
                "main",
                r#"
                    fnc main: s32 -> 10;
                "#,
            )],
            expect![["Some(I32(10))"]],
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
            expect![["Some(I32(11))"]],
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
            expect![["Some(I32(5))"]],
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
            expect![["Some(I32(65))"]],
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
            expect![["Some(I32(1))"]],
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
            expect![["Some(I32(5))"]],
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
            expect![["Some(I32(30))"]],
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
            expect![["Some(I32(0))"]],
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
            expect![["Some(I32(0))"]],
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
            expect![["Some(I32(6))"]],
        );
    }
}
