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

        let program = hir_lower::lower(&root);
        dbg!(&program);

        let infer_result = hir_ty::infer_with_var_tys(&program, var_tys);
        dbg!(&infer_result);
        var_tys = infer_result.var_tys;

        let result = evaluator.eval(program);
        dbg!(result);

        input.clear();
    }
}
