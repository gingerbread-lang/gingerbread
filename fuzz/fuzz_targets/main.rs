#![no_main]

use ast::AstNode;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|s: &str| {
    let world_index = hir::WorldIndex::default();

    let tokens = lexer::lex(s);
    let parse = parser::parse_repl_line(&tokens);
    let root = ast::Root::cast(parse.syntax_node()).unwrap();
    let _diagnostics = ast::validation::validate(&root);
    let (index, _diagnostics) = hir::index(&root, &world_index);
    let (bodies, _diagnostics) = hir::lower(&root, &index, &world_index);
    let (_inference, _diagnostics) = hir_ty::infer_all(&bodies, &index, &world_index);
});
