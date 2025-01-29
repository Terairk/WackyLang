#![allow(clippy::arbitrary_source_item_ordering)]

use chumsky::error::RichReason;
use chumsky::input::WithContext;
use chumsky::prelude::Input as _;
use chumsky::Parser;
use std::fmt;
use std::ops::{Deref, DerefMut};
use wacc_syntax::parser::program_parser;
use wacc_syntax::source::{SourcedSpan, StrSourceId};
use wacc_syntax::token::{lexer, Token};

#[allow(dead_code)]
const TEST_EXPR: &str = r#"
(foo == bar[23][3234 + ord - chr flll][34][234]) * len - ord ("some string literal" - chr - +2341) >= 23 == '\\'
"#;

#[allow(dead_code, clippy::needless_raw_string_hashes)]
const TEST_TYPE: &str = r#"pair(int, pair(pair,string)[][][])[][]"#;

#[allow(dead_code)]
const TEST_PROGRAM: &str = r#"
# The program reads n (number of integers), then n integers. After each input,
# it insert the integer into a binary search tree. At the end, it prints out
# the content in the binary search tree so that we have a sorted list of
# integer.
#
# We represent a node in the binary search tree using two pair elements. The
# first element has a type <int, pair>, the int is the integer stored in the
# node, the pair is the pointer to the second pair element. The second pair
# element has a type <pair, pair> which is the pointer to the two children
# nodes in the binary search tree.

begin

  # Create a new node of a binary search tree having the given integer value
  # and points to the two given pairs.
  pair(int, pair) createNewNode(int value, pair(int, pair) left, pair(int, pair) right) is
    pair(pair, pair) p = newpair(left, right) ;
    pair(int, pair) q = newpair(value, p) ;
    return q
  end

  # Given a root of a binary search tree and an integer to insert, the function
  # inserts the integer into the tree and returns the new root of the tree.
  pair(int, pair) insert(pair(int, pair) root, int n) is
    if root == null then
      root = call createNewNode(n, null, null)
    else
      pair(pair, pair) p = snd root ;
      int current = fst root ;
      pair(int, pair) q = null ;
      if n < current then
      	q = fst p ;
        fst p = call insert(q, n)
      else
      	q = snd p ;
        snd p = call insert(q, n)
      fi
    fi ;
    return root
  end

  # Print the integers in the binary search tree in the increasing order.
  int printTree(pair(int, pair) root) is
    if root == null then
      return 0
    else
      pair(pair, pair) body = snd root ;
      pair(int, pair) p = fst body ;
      int temp = call printTree(p) ;
      temp = fst root ;
      print temp ;
      print ' ' ;
      p = snd body ;
      temp = call printTree(p) ;
      return 0
    fi
  end

  # The main function
  int n = 0 ;
  print "Please enter the number of integers to insert: " ;
  read n ;
  print "There are " ;
  print n ;
  println " integers." ;
  int i = 0 ;
  pair(int, pair) root = null ;
  while i < n do
    int x = 0 ;
    print "Please enter the number at position " ;
    print i + 1 ;
    print " : " ;
    read x ;
    root = call insert(root, x) ;
    i = i + 1
  done ;
  print "Here are the numbers sorted: " ;
  i = call printTree(root) ;
  println "\t\n\0\f\r"
end

# this should be EOF comment"#;

fn main() {
    let source = TEST_PROGRAM;
    let source_id = StrSourceId::repl();
    let eoi_span = SourcedSpan::new(source_id.clone(), (source.len()..source.len()).into());

    // so the pattern is, make everything generic asf and supply the concrete implementations later :)
    let (tokens, lexing_errs): (Option<Vec<(Token, _)>>, _) = Parser::parse(
        &lexer::<WithContext<SourcedSpan, &str>>(),
        source.with_context((source_id, ())),
    )
    .into_output_errors();

    if let Some(tokens) = tokens {
        println!("{:?}", DisplayVec(tokens.clone()));

        // attach the span of each token to it before parsing, so it is not forgotten
        #[allow(clippy::pattern_type_mismatch)]
        let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));
        let (parsed, parse_errs) = program_parser().parse(spanned_tokens).into_output_errors();

        println!("{parsed:?}");

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
    }

    for e in lexing_errs {
        println!("Lexing error: {e}");
    }
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
    use std::fs;
    use std::path::{Path, PathBuf};
    use chumsky::input::{Input, WithContext};
    use chumsky::Parser;
    use wacc_syntax::parser::program_parser;
    use wacc_syntax::source::{SourcedSpan, StrSourceId};
    use wacc_syntax::token::{lexer, Token};

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
                        },
                        Err(error_msg) => {
                            if error_msg == "Syntax error(s) found!" {
                                println!("Test passed: {}", test_name);
                                passed_count += 1;
                            } else {
                                println!("Test failed: {} with cause {error_msg}", test_name)
                            }
                        },
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
                        },
                        Err(error_msg) => {
                            println!("Test failed: {} with cause {error_msg}", test_name)
                        },
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

        if let Some(tokens) = tokens {
            #[allow(clippy::pattern_type_mismatch)]
            let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));
            let (_parsed, parse_errs) = program_parser().parse(spanned_tokens).into_output_errors();


            if !parse_errs.is_empty() {
                return Err(String::from("Syntax error(s) found!"));
            }
        }
        // If there are syntax errors, return an appropriate result
        if !lexing_errs.is_empty() {
            return Err(String::from("Syntax error(s) found!"));
        }

        // TODO: semantic analysis
        let semantic_errors:Vec<i32> = Vec::new();

        if !semantic_errors.is_empty() {
            return Err(String::from("Semantic error(s) found!"));
        }

        // If both syntax and semantic analysis succeed, return success
        Ok(format!("Test passed: {}", path.display()))
    }
}


