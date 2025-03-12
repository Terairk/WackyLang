mod common;

#[cfg(test)]
mod tests {
    use crate::common::*;
    use std::path::Path;
    static SEMANTIC_ERR_STR: &str = "Semantic error(s) found!";
    #[test]
    fn run_failed_semantic_tests() {
        let tests_dir = Path::new("../test_cases/invalid/semanticErr");

        let mut passed_count = 0;
        let mut total_count = 0;

        match get_test_files(tests_dir) {
            Ok(test_files) => {
                for test_file in test_files {
                    let test_name = test_file.display();
                    match run_single_test(&test_file) {
                        Ok(_) => {
                            println!("Test failed: error not detected in {test_name}");
                        }
                        Err(error_msg) => {
                            if error_msg == SEMANTIC_ERR_STR {
                                println!("Test passed: {test_name}");
                                passed_count += 1;
                            } else {
                                println!("Test failed: {test_name} with cause {error_msg}");
                            }
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

