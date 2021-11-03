#![no_main]

use ast::AstNode;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|s: &str| {
    let tokens = lexer::lex(s);
    let parse = parser::parse_repl_line(&tokens);
    let root = ast::Root::cast(parse.syntax_node()).unwrap();
    let _validation_errors = ast::validation::validate(&root);
    let (program, _source_map, _lower_errors, _local_def_names) = hir_lower::lower(&root);
    let _infer_result = hir_ty::infer(&program);
    let mut evaluator = eval::Evaluator::default();
    let _result = evaluator.eval(program);
});
