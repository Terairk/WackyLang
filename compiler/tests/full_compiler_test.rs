mod common;

use common::*;
use std::panic;
use std::path::Path;

#[cfg(test)]
mod full_compiler_tests {
    use super::*;
    #[test]
    fn run_full_compiler_tests() {
        let tests_dir = Path::new("../test_cases/valid");

        let mut passed_count = 0;
        let mut total_count = 0;
        let mut compiled_count = 0;
        let mut assemble_count = 0;

        match get_test_files(tests_dir) {
            Ok(test_files) => {
                for test_file in test_files {
                    let test_name = test_file.display();
                    total_count += 1;

                    let result = panic::catch_unwind(|| compile_single_test(&test_file));

                    let Ok(result) = result else {
                        println!("Compilation failed with a crash: {test_name}");
                        continue;
                    };

                    compiled_count += 1;
                    // Result is a Result<Result<>> so we need this unwrap
                    match result {
                        Ok(CompileSingleTestOutput {
                            has_read_instructions,
                            ..
                        }) => match compare_test_result(&test_file, has_read_instructions) {
                            Ok(_) => {
                                assemble_count += 1;
                                passed_count += 1;
                                println!("Test passed: {test_name}");
                            }
                            Err(error_msg) => {
                                if !error_msg.starts_with("failed to assemble") {
                                    assemble_count += 1;
                                }
                                println!("Test failed: {test_name} with cause {error_msg}")
                            }
                        },
                        Err(error_msg) => {
                            println!(
                                "Test compilation gracefully failed: {test_name} with cause {error_msg}"
                            );
                        }
                    }
                }
            }
            Err(e) => eprintln!("Failed to collect test files: {e}"),
        }
        println!(
            "Compiled {}, assembled {}, passed {} out of {} tests!",
            compiled_count, assemble_count, passed_count, total_count
        );
        assert_eq!(passed_count, total_count);
    }
}
