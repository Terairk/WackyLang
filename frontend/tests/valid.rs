mod common;

#[cfg(test)]
pub mod tests {
    use crate::common::{get_test_files, run_single_test};
    use std::path::Path;

    #[test]
    fn run_valid_tests() {
        let tests_dir = Path::new("../test_cases/valid");

        let mut passed_count = 0;
        let mut total_count = 0;

        match get_test_files(tests_dir) {
            Ok(test_files) => {
                for test_file in test_files {
                    let test_name = test_file.display();
                    match run_single_test(&test_file) {
                        Ok(_) => {
                            passed_count += 1;
                            println!("Test passed: {test_name}");
                        }
                        Err(error_msg) => {
                            println!("Test failed: {test_name} with cause {error_msg}");
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
}
