#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() {
    let input = gen::gen(4 << 10 << 10); // 4 MiB
    let tokens = lexer::lex(&input);
    parser::parse_source_file(&tokens);
}
