mod common;

use std::path::Path;

#[test]
fn run_full_compiler_tests() {
    let tests_dir = Path::new("../test_cases/valid");

    let mut passed_count = 0;
    let mut total_count = 0;

    match common::get_test_files(tests_dir) {
        Ok(test_files) => {
            for test_file in test_files {
                let test_name = test_file.display();
                match common::compile_single_test(&test_file) {
                    Ok(_) => {
                        println!("Test compiled: {}", test_name);
                        passed_count += 1;
                        println!("Test passed: {test_name}");
                    }
                    Err(error_msg) => {
                        println!("Test compilation failed: {test_name} with cause {error_msg}");
                    }
                }
                total_count += 1;
            }
        }
        Err(e) => eprintln!("Failed to collect test files: {e}"),
    }
    println!("Passed {passed_count} out of {total_count} tests!");
    assert_eq!(passed_count, total_count);
}