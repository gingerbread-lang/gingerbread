use ast::AstNode;
use eval::Evaluator;
use std::collections::HashMap;
use std::io;

fn main() -> anyhow::Result<()> {
    let stdin = io::stdin();
    let mut input = String::new();
    let mut evaluator = Evaluator::default();
    let mut var_tys = HashMap::new();

    loop {
        stdin.read_line(&mut input)?;

        let tokens = lexer::lex(&input);
        dbg!(lexer::lex(&input).collect::<Vec<_>>());

        let parse = parser::parse(tokens);
        dbg!(&parse);

        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let validation_errors = ast::validation::validate(&root);

        for error in validation_errors {
            println!("{}", error);
        }

        let (program, source_map) = hir_lower::lower(&root);
        dbg!(&program, &source_map);

        let infer_result = hir_ty::infer_with_var_tys(&program, var_tys);
        dbg!(&infer_result);
        var_tys = infer_result.var_tys;

        for error in infer_result.errors {
            dbg!(&source_map.expr_map[error.expr], &error.kind);
        }

        let result = evaluator.eval(program);
        dbg!(result);

        input.clear();
    }
}
