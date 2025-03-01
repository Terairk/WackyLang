mod common;

use std::path::Path;
use std::panic;
use common::*;

#[test]
fn run_full_compiler_tests() {
    let tests_dir = Path::new("../test_cases/valid");

    let mut passed_count = 0;
    let mut total_count = 0;
    let mut compiled_count = 0;

    match get_test_files(tests_dir) {
        Ok(test_files) => {
            for test_file in test_files {
                let test_name = test_file.display();
                total_count += 1;

                let result = panic::catch_unwind(|| compile_single_test(&test_file));

                if result.is_err() {
                    println!("Compilation failed with a crash: {test_name}");
                    continue;
                }

                compiled_count += 1;
                match result.unwrap() {
                    Ok(_) => match compare_test_result(&test_file) {
                        Ok(_) => {
                            passed_count += 1;
                            println!("Test passed: {test_name}");
                        }
                        Err(error_msg) => println!("Test failed: {test_name} with cause {error_msg}"),
                    },
                    Err(error_msg) => {
                        println!("Test compilation gracefully failed: {test_name} with cause {error_msg}");
                    }
                }
            }
        }
        Err(e) => eprintln!("Failed to collect test files: {e}"),
    }
    println!("Compiled {compiled_count} out of {total_count}, passed {passed_count} out of {total_count} tests!");
    assert_eq!(passed_count, total_count);
}