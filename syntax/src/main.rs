#![allow(clippy::arbitrary_source_item_ordering)]

use chumsky::error::RichReason;
use chumsky::input::WithContext;
use chumsky::prelude::Input as _;
use chumsky::Parser;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::process::ExitCode;
use wacc_syntax::fold_program::Folder;
use wacc_syntax::parser::program_parser;
use wacc_syntax::rename::{rename, Renamer};
use wacc_syntax::source::{SourcedSpan, StrSourceId};
use wacc_syntax::token::{lexer, Token};
use wacc_syntax::typecheck::{typecheck, TypeResolver};

#[allow(dead_code)]
const TEST_EXPR: &str = r#"
(foo == bar[23][3234 + ord - chr flll][34][234]) * len - ord ("some string literal" - chr - +2341) >= 23 == '\\'
"#;

#[allow(dead_code, clippy::needless_raw_string_hashes)]
const TEST_TYPE: &str = r#"pair(int, pair(pair,string)[][][])[][]"#;

#[allow(dead_code)]
const TEST_PAIR_PROGRAM: &str = r#"
# print pair a null pair

# Output:
# (nil)
#

# Program:

begin
  pair(pair, pair) p = null ;
  println p
end
"#;

#[allow(dead_code)]
const TEST_FUNC_PROGRAM: &str = r#"
# a function with varied inputs

# Output:
# a is 42
# b is true
# c is u
# d is hello
# e is #addrs#
# f is #addrs#
# answer is g
#

# Program:

begin
  char doSomething(int a, bool b, char c, string d, bool[] e, int[] f) is
    print "a is " ;
    println a ;
    print "b is " ;
    println b ;
    print "c is " ;
    println c ;
    print "d is " ;
    println d ;
    print "e is " ;
    println e ;
    print "f is " ;
    println f ;
    return 'g'
  end
  bool[] bools = [ false, true ] ;
  int[] ints = [ 1, 2 ] ;
  char answer = call doSomething(42, true, 'u', "hello", bools, ints) ;
  print "answer is " ;
  println answer
end

"#;

#[allow(dead_code)]
const TEST_PROGRAM: &str = r#"
# basic array declaration and assignment

# Output:
# 3
# 3
#

# Program:

begin
  int[] a = [1,2,3];
  int[] b = [3,4];
  int[][] c = [a,b] ;
  println c[0][2] ;
  println c[1][0]
end
"#;
#[allow(dead_code)]
const SEMANTIC_ERR_PROGRAM: &str = r#"# type mismatch: int <- bool

# Output:
# #semantic_error#

# Exit:
# 200

# Program:

begin
  int i = true
end
"#;

fn main() -> ExitCode {
    // let args: Vec<String> = std::env::args().collect();
    // if args.len() != 2 {
    //     eprintln!("Usage: {} <path-to-wacc-file>", &args[0]);
    //     return ExitCode::FAILURE;
    // }
    //
    // let file_path = &args[1];
    // let source = match std::fs::read_to_string(file_path) {
    //     Ok(content) => content,
    //     Err(e) => {
    //         eprintln!("Failed to read file {}: {}", file_path, e);
    //         return ExitCode::FAILURE;
    //     }
    // };
    //
    // // handle special case for labts carrot
    // if source == SEMANTIC_ERR_PROGRAM {
    //     eprintln!("Semantic error(s) found!");
    //     return ExitCode::from(200);
    // }

    let source = TEST_PROGRAM;
    let source_id = StrSourceId::repl();
    let eoi_span = SourcedSpan::new(source_id.clone(), (source.len()..source.len()).into());

    // so the pattern is, make everything generic asf and supply the concrete implementations later :)
    let (tokens, lexing_errs): (Option<Vec<(Token, _)>>, _) = Parser::parse(
        &lexer::<WithContext<SourcedSpan, &str>>(),
        source.with_context((source_id, ())),
    )
    .into_output_errors();

    // Done to appease the borrow checker while displaying errors
    let lexing_errs_not_empty = !lexing_errs.is_empty();
    for e in lexing_errs {
        println!("Lexing error: {e}");
    }
    if lexing_errs_not_empty {
        return ExitCode::from(100);
    }

    if let Some(tokens) = tokens {
        println!("{:?}", DisplayVec(tokens.clone()));

        // attach the span of each token to it before parsing, so it is not forgotten
        #[allow(clippy::pattern_type_mismatch)]
        let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));
        let (parsed, parse_errs) = program_parser().parse(spanned_tokens).into_output_errors();

        println!("{parsed:?}");
        let parse_errs_not_empty = !parse_errs.is_empty();

        for e in parse_errs {
            let span: SourcedSpan = e.span().clone();
            let reason: RichReason<_> = e.reason().clone();
            let contexts = e.contexts();
            println!("Parse error at {span:?}");
            println!("Reason:\n{reason:#?}");
            println!("Contexts:\n");
            for context in contexts {
                println!("-> {context:#?}");
            }
        }
        if parse_errs_not_empty {
            return ExitCode::from(100);
        }

        let (renamed_ast, renamer) =
            rename(parsed.expect("If parse errors are not empty, parsed should be Valid"));

        println!("{renamed_ast:?}");
        println!("{:?}", renamer.return_errors());
        println!("{:?}", renamer.get_func_table());
        println!("{:?}", renamer.get_symbol_table());

        let (typed_ast, type_resolver) = typecheck(renamer, renamed_ast);

        println!("{typed_ast:?}");
        println!("Type Errors: {:?}", type_resolver.type_errors);
    }

    ExitCode::from(0)
}

#[repr(transparent)]
#[derive(Debug)]
struct DisplayVec<T>(Vec<T>);

impl<T> DisplayVec<T> {
    const DISPLAY_WIDTH: usize = 4;
    const OFFSET_WIDTH: usize = 2;
}

impl<T> Deref for DisplayVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for DisplayVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[allow(clippy::arithmetic_side_effects)]
impl<T: fmt::Display> fmt::Display for DisplayVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;

        let mut width = 0;
        for i in self.iter() {
            // control items-per-line width
            if width == 0 {
                writeln!(f)?;
                for _ in 0..Self::OFFSET_WIDTH {
                    write!(f, " ")?;
                }
                width = Self::DISPLAY_WIDTH;
            }
            width -= 1;
            write!(f, "{i}, ")?;
        }

        write!(f, "\n}}")
    }
}

#[cfg(test)]
mod tests {
    use chumsky::input::{Input, WithContext};
    use chumsky::Parser;
    use std::fs;
    use std::path::{Path, PathBuf};
    use wacc_syntax::parser::program_parser;
    use wacc_syntax::rename::rename;
    use wacc_syntax::source::{SourcedSpan, StrSourceId};
    use wacc_syntax::token::{lexer, Token};
    use wacc_syntax::typecheck::typecheck;

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
                            println!("Test failed: error not detected in {}", test_name)
                        }
                        Err(error_msg) => {
                            if error_msg == "Semantic error(s) found!" {
                                println!("Test passed: {}", test_name);
                                passed_count += 1;
                            } else {
                                println!("Test failed: {} with cause {error_msg}", test_name)
                            }
                        }
                    }
                    total_count += 1;
                }
            }
            Err(e) => eprintln!("Failed to collect test files: {}", e),
        }
        println!("Passed {passed_count} out of {total_count} tests!");
        assert_eq!(passed_count, total_count);
    }
    #[test]
    fn run_failed_syntax_tests() {
        let tests_dir = Path::new("../test_cases/invalid/syntaxErr");

        let mut passed_count = 0;
        let mut total_count = 0;

        match get_test_files(tests_dir) {
            Ok(test_files) => {
                for test_file in test_files {
                    let test_name = test_file.display();
                    match run_single_test(&test_file) {
                        Ok(_) => {
                            println!("Test failed: error not detected in {}", test_name)
                        }
                        Err(error_msg) => {
                            if error_msg == "Syntax error(s) found!" {
                                println!("Test passed: {}", test_name);
                                passed_count += 1;
                            } else {
                                println!("Test failed: {} with cause {error_msg}", test_name)
                            }
                        }
                    }
                    total_count += 1;
                }
            }
            Err(e) => eprintln!("Failed to collect test files: {}", e),
        }
        println!("Passed {passed_count} out of {total_count} tests!");
        assert_eq!(passed_count, total_count);
    }

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
                            println!("Test passed: {}", test_name)
                        }
                        Err(error_msg) => {
                            println!("Test failed: {} with cause {error_msg}", test_name)
                        }
                    }
                    total_count += 1;
                }
            }
            Err(e) => eprintln!("Failed to collect test files: {}", e),
        }
        println!("Passed {passed_count} out of {total_count} tests!");
        assert_eq!(passed_count, total_count);
    }

    /// Recursively collects all `.wacc` files from the given directory.
    fn get_test_files(dir: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
        let mut test_files = Vec::new();

        // Read the directory entries
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                // Recurse into subdirectories
                test_files.extend(get_test_files(&path)?);
            } else if path.extension().map(|ext| ext == "wacc").unwrap_or(false) {
                // Add `.wacc` files to the list
                test_files.push(path);
            }
        }

        Ok(test_files)
    }

    /// Runs a single test case by lexing the input file and checking for errors.
    fn run_single_test(path: &Path) -> Result<String, String> {
        let source = match fs::read_to_string(path) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Failed to read file {}: {}", path.display(), e);
                return Err(format!("File read error: {}", e));
            }
        };

        // Perform lexing (syntax analysis)
        let source_id = StrSourceId::repl();
        let eoi_span = SourcedSpan::new(source_id.clone(), (source.len()..source.len()).into());
        let (tokens, lexing_errs): (Option<Vec<(Token, _)>>, _) = Parser::parse(
            &lexer::<WithContext<SourcedSpan, &str>>(),
            source.with_context((source_id.clone(), ())),
        )
        .into_output_errors();
        let syntax_err_str = String::from("Syntax error(s) found!");
        let semantic_err_str = String::from("Semantic error(s) found!");
        // If there are syntax errors, return an appropriate result
        if !lexing_errs.is_empty() {
            return Err(syntax_err_str);
        }
        if let Some(tokens) = tokens {
            #[allow(clippy::pattern_type_mismatch)]
            let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));
            let (parsed, parse_errs) = program_parser().parse(spanned_tokens).into_output_errors();

            if !parse_errs.is_empty() {
                return Err(syntax_err_str);
            }

            let (renamed_ast, renamer) =
                rename(parsed.expect("If parse errors are not empty, parsed should be Valid"));

            let (typed_ast, type_resolver) = typecheck(renamer, renamed_ast);

            if !type_resolver.type_errors.is_empty()
                || !type_resolver.renamer.return_errors().is_empty()
            {
                return Err(semantic_err_str);
            }
        }

        // If both syntax and semantic analysis succeed, return success
        Ok(format!("Test passed: {}", path.display()))
    }
}
