use ast::AstNode;
use std::io;

fn main() -> anyhow::Result<()> {
    let stdin = io::stdin();
    let mut input = String::new();

    loop {
        stdin.read_line(&mut input)?;

        let tokens = lexer::lex(&input);
        dbg!(lexer::lex(&input).collect::<Vec<_>>());

        let parse = parser::parse(tokens);
        dbg!(&parse);

        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let program = hir_lower::lower(root);
        dbg!(program);

        input.clear();
    }
}
