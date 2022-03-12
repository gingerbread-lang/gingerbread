use crate::Parse;
use expect_test::expect_file;
use std::ffi::OsStr;
use std::{env, fs};
use token::Token;

#[test]
fn source_file() {
    run_parser_tests("source_file", crate::parse_source_file);
}

#[test]
fn repl_line() {
    run_parser_tests("repl_line", crate::parse_repl_line);
}

fn run_parser_tests(tests_dir: &str, parsing_fn: fn(&[Token], &str) -> Parse) {
    let tests_dir = {
        let current_dir = env::current_dir().unwrap();
        current_dir.join(format!("src/tests/{}", tests_dir))
    };

    let mut did_any_test_fail = false;

    for file in fs::read_dir(tests_dir).unwrap() {
        let path = file.unwrap().path();

        if path.extension() != Some(OsStr::new("test")) {
            continue;
        }

        let did_panic = std::panic::catch_unwind(|| {
            let test_content = fs::read_to_string(&path).unwrap();
            let (input, _expected_parse) = test_content.split_once("\n===\n").unwrap();

            let actual_parse = {
                let tokens = lexer::lex(input);
                parsing_fn(&tokens, input)
            };

            let expected_test_content = format!("{}\n===\n{:?}\n", input, actual_parse);

            expect_file![path].assert_eq(&expected_test_content);
        })
        .is_err();

        if did_panic {
            did_any_test_fail = true;
        }
    }

    if did_any_test_fail {
        panic!("At least one parser test failed");
    }
}
