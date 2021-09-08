#![no_main]

use ast::AstNode;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|s: &str| {
    let tokens = lexer::lex(s);
    let parse = parser::parse(tokens);
    let root = ast::Root::cast(parse.syntax_node()).unwrap();
    let _program = hir_lower::lower(&root);
});
