use expect_test::expect_file;
use std::ffi::OsStr;
use std::{env, fs};

#[test]
fn parser_tests() {
    let tests_dir = {
        let current_dir = env::current_dir().unwrap();
        current_dir.join("src/tests/source_file")
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
                crate::parse(&tokens)
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
