#![no_main]

use ast::AstNode;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|s: &str| {
    let world_index = hir::WorldIndex::default();

    let tokens = lexer::lex(s);
    let parse = parser::parse_repl_line(&tokens);
    let tree = parse.syntax_tree();
    let root = ast::Root::cast(tree.root(), tree).unwrap();
    let _diagnostics = ast::validation::validate(root, tree);
    let (index, _diagnostics) = hir::index(root, tree, &world_index);
    let (bodies, _diagnostics) = hir::lower(root, tree, &index, &world_index);
    let (_inference, _diagnostics) = hir_ty::infer_all(&bodies, &index, &world_index);
});
