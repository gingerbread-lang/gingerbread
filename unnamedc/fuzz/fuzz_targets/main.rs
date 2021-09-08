#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|s: &str| {
    let tokens = lexer::lex(s);
    let _parse = parser::parse(tokens);
});
