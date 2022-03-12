use ast::AstNode;
use std::env;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() {
    let mut args = env::args();
    args.next();

    match args.next().as_deref() {
        Some("parser") => {
            let input = gen::gen(4 << 10 << 10); // 4 MiB
            let tokens = lexer::lex(&input);
            parser::parse_source_file(&tokens);
        }

        Some("short") => {
            for _ in 0..100_000 {
                compile(
                    r#"
                        fnc add(x: s32, y: s32): s32 -> x + y;
                        fnc main -> add 10, 20 + 30;
                    "#,
                );
            }
        }

        Some("long") => compile(&gen::gen(16 << 10 << 10)), // 16 MiB

        Some(_) => eprintln!("Unrecognized benchmark name"),

        None => eprintln!("Please provide a benchmark name"),
    }
}

fn compile(input: &str) {
    let world_index = hir::WorldIndex::default();
    let tokens = lexer::lex(input);
    let tree = parser::parse_repl_line(&tokens, input).into_syntax_tree();
    let root = ast::Root::cast(tree.root(), &tree).unwrap();
    let _diagnostics = ast::validation::validate(root, &tree);
    let (index, _diagnostics) = hir::index(root, &tree, &world_index);
    let (bodies, _diagnostics) = hir::lower(root, &tree, &index, &world_index);
    let (_inference, _diagnostics) = hir_ty::infer_all(&bodies, &index, &world_index);
}
