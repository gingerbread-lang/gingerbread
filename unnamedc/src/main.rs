use std::io;

fn main() -> anyhow::Result<()> {
    let stdin = io::stdin();
    let mut input = String::new();

    loop {
        stdin.read_line(&mut input)?;

        let tokens = unnamedc::lex(&input);
        dbg!(tokens.collect::<Vec<_>>());

        input.clear();
    }
}
