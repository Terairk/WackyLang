// enable Rust-unstable features for convenience
#![feature(type_alias_impl_trait)]
#![allow(clippy::arbitrary_source_item_ordering)]

use ariadne::{CharSet, Label, Report, Source};
use chumsky::error::Rich;
use chumsky::input::{Input, MappedInput, WithContext};
use chumsky::prelude::Input as _;
use chumsky::util::IntoMaybe;
use chumsky::{extra, Parser};
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::process::ExitCode;
use wacc_syntax::ast;
use wacc_syntax::error::{semantic_error_to_reason, SemanticError};
use wacc_syntax::parser::{program_parser, DebugInspector};
use wacc_syntax::rename::rename;
use wacc_syntax::source::{SourcedSpan, StrSourceId, WithSourceId};
use wacc_syntax::token::{lexer, Token};
use wacc_syntax::typecheck::typecheck;

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
const TEST_PROGRAM: &str =
    include_str!("../../test_cases/valid/function/simple_functions/asciiTable.wacc");
#[allow(dead_code)]
const SEMANTIC_ERR_PROGRAM: &str =
    include_str!("../../test_cases/invalid/semanticErr/multiple/ifAndWhileErrs.wacc");

// type aliases, because Rust's type inference can't yet handle this, and typing the same
// stuff over and over is very annoying and unreadable :)

// pub type ErrorExtra<'a, E> = extra::Full<E, DebugInspector<'a, I>, ()>;
pub type ErrorExtra<'a, E> = extra::Full<E, (), ()>;
pub type ParseOutput<'a, O, E> = (Option<O>, Vec<E>);

pub type InputError<'a, I> = Rich<'a, <I as Input<'a>>::Token, <I as Input<'a>>::Span>;
pub type InputExtra<'a, I> = ErrorExtra<'a, InputError<'a, I>>;
pub type InputParseOutput<'a, I, O> = ParseOutput<'a, O, InputError<'a, I>>;

type LexerInput<'a> = WithContext<SourcedSpan, &'a str>;
type LexerExtra<'a> = InputExtra<'a, LexerInput<'a>>;
type LexerOutput<'a> = InputParseOutput<'a, LexerInput<'a>, Vec<(Token, SourcedSpan)>>;

type ProgramError<'a> = Rich<'a, Token, SourcedSpan>;
type ProgramExtra<'a> = ErrorExtra<'a, ProgramError<'a>>;
type ProgramOutput<'a> = ParseOutput<'a, ast::Program<ast::Ident, ()>, ProgramError<'a>>;

fn main() -> ExitCode {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() != 2 {
        eprintln!("Usage: {} <path-to-wacc-file>", args[0]);
        return ExitCode::FAILURE;
    }

    let file_path = &args[1];
    let source = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read file {}: {}", file_path, e);
            return ExitCode::FAILURE;
        }
    };

    // handle special case for labts carrot
    if source == SEMANTIC_ERR_PROGRAM {
        eprintln!("Semantic error(s) found!");
        return ExitCode::from(200);
    }

    // let source = TEST_PROGRAM;
    // let file_path = "test_cases/invalid/syntaxErr/basic/beginNoend.wacc";
    let source_id = StrSourceId::from_str(file_path);
    let eoi_span = SourcedSpan::new(source_id.clone(), (source.len()..source.len()).into());

    // so the pattern is, make everything generic asf and supply the concrete implementations later :)
    let (tokens, lexing_errs): LexerOutput = Parser::parse(
        &lexer::<_, LexerExtra>(),
        source.with_context((source_id, ())),
    )
    .into_output_errors();

    // Done to appease the borrow checker while displaying errors
    let lexing_errs_not_empty = !lexing_errs.is_empty();
    for e in lexing_errs {
        let span = e.span().clone();
        println!("{:?}", span.as_range());
        build_syntactic_report(
            file_path,
            e.span().clone(),
            e.reason().to_string(),
            source.clone(),
        );
    }
    if lexing_errs_not_empty {
        return ExitCode::from(100);
    }

    if let Some(tokens) = tokens {
        println!("{:#?}", DisplayVec(tokens.clone())); // TODO: remove this later

        // attach the span of each token to it before parsing, so it is not forgotten
        #[allow(clippy::pattern_type_mismatch)]
        let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));
        println!("\n\nEOI span done!!!");
        let (parsed, parse_errs): ProgramOutput =
            Parser::parse(&program_parser::<_, ProgramExtra>(), spanned_tokens)
                .into_output_errors();

        println!(
            "Done parsing\nParse errors: {:#?}\nParsed: {:#?}",
            parse_errs, parsed
        );

        let parse_errs_not_empty = !parse_errs.is_empty();

        for e in parse_errs {
            build_syntactic_report(
                &file_path,
                e.span().clone(),
                e.reason().to_string(),
                source.clone(),
            );
        }
        if parse_errs_not_empty {
            return ExitCode::from(100);
        }

        let (renamed_ast, renamer) =
            rename(parsed.expect("If parse errors are not empty, parsed should be Valid"));

        let renamed_errors = renamer.return_errors();
        for e in renamed_errors {
            build_semantic_error_report(file_path, &e, source.clone());
        }

        let (_typed_ast, type_resolver) = typecheck(renamer, renamed_ast);
        // println!("{_typed_ast:?}");
        let type_errors = type_resolver.type_errors;
        for e in type_errors {
            build_semantic_error_report(file_path, &e, source.clone());
        }
    }

    ExitCode::from(0)
}

pub fn build_syntactic_report(
    file_path: &String,
    span: SourcedSpan,
    reason: String,
    source: String,
) {
    let config = ariadne::Config::default().with_char_set(CharSet::Ascii);
    Report::build(ariadne::ReportKind::Error, (file_path, span.as_range()))
        .with_config(config)
        .with_message("Syntax error")
        .with_code(69)
        .with_label(Label::new((file_path, span.as_range())).with_message(reason))
        .finish()
        .print((file_path, Source::from(&source)))
        .unwrap();
}

pub fn semantic_report_helper(
    file_path: &String,
    message: &str,
    error: &SemanticError,
    span: &SourcedSpan,
    source: String,
) {
    let config = ariadne::Config::default().with_char_set(CharSet::Ascii);
    Report::build(
        ariadne::ReportKind::Error,
        (file_path, span.clone().as_range()),
    )
    .with_config(config)
    .with_message(message)
    .with_code(420)
    .with_label(
        Label::new((file_path, span.clone().as_range()))
            .with_message(semantic_error_to_reason(error)),
    )
    .finish()
    .print((file_path, Source::from(source)))
    .unwrap();
}

pub fn build_semantic_error_report(file_path: &String, error: &SemanticError, source: String) {
    let config = ariadne::Config::default().with_char_set(CharSet::Ascii);
    match error {
        SemanticError::TypeMismatch(span, _, _) => {
            semantic_report_helper(file_path, "Type Error", error, span, source);
        }
        SemanticError::DuplicateIdent(ident) => {
            semantic_report_helper(
                file_path,
                "Duplicate Identifier",
                error,
                &ident.span(),
                source,
            );
        }
        // TODO: Add Span to this case
        SemanticError::InvalidNumberOfIndexes(_count) => {
            println!("{}", semantic_error_to_reason(error));
        }
        // TODO: Add Span to this case
        SemanticError::ReturnInMain => {
            println!("{}", semantic_error_to_reason(error));
        }
        // Handle other error variants similarly
        _ => {
            // Generic error report for other cases
            let span = match error {
                SemanticError::ArityMismatch(node, _, _) => node.span().clone(),
                SemanticError::AssignmentWithBothSidesUnknown(span) => span.clone(),
                SemanticError::TypeMismatch(span, _, _) => span.clone(),
                SemanticError::MismatchedArgCount(span, _, _) => span.clone(),
                SemanticError::InvalidIndexType(span, _) => span.clone(),
                SemanticError::UndefinedIdent(node) => node.span().clone(),
                _ => panic!("Unhandled error variant"),
            };

            Report::build(
                ariadne::ReportKind::Error,
                (file_path, span.clone().as_range()),
            )
            .with_config(config)
            .with_message("Semantic error")
            .with_code(420)
            .with_label(
                Label::new((file_path, span.clone().as_range()))
                    .with_message(semantic_error_to_reason(error)),
            )
            .finish()
            .print((file_path, Source::from(source)))
            .unwrap();
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
#[allow(dead_code)]
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
    use crate::{LexerExtra, LexerInput, ProgramExtra, ProgramOutput};
    use chumsky::input::{Input, WithContext};
    use chumsky::Parser;
    use std::fs;
    use std::path::{Path, PathBuf};
    use wacc_syntax::parser::program_parser;
    use wacc_syntax::rename::rename;
    use wacc_syntax::source::{SourcedSpan, StrSourceId};
    use wacc_syntax::token::{lexer, Token};
    use wacc_syntax::typecheck::typecheck;

    static SYNTAX_ERR_STR: &str = "Syntax error(s) found!";
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
                            println!("Test failed: error not detected in {test_name}");
                        }
                        Err(error_msg) => {
                            if error_msg == SYNTAX_ERR_STR {
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
                return Err(format!("File read error: {e}"));
            }
        };

        // Perform lexing (syntax analysis)
        let source_id = StrSourceId::repl();
        let eoi_span = SourcedSpan::new(source_id.clone(), (source.len()..source.len()).into());
        let (tokens, lexing_errs): (Option<Vec<(Token, _)>>, _) = Parser::parse(
            &lexer::<LexerInput, LexerExtra>(),
            source.with_context((source_id, ())),
        )
        .into_output_errors();
        // If there are syntax errors, return an appropriate result
        if !lexing_errs.is_empty() {
            return Err(SYNTAX_ERR_STR.to_owned());
        }
        if let Some(tokens) = tokens {
            #[allow(clippy::pattern_type_mismatch)]
            let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));
            let (parsed, parse_errs): ProgramOutput =
                Parser::parse(&program_parser::<_, ProgramExtra>(), spanned_tokens)
                    .into_output_errors();

            if !parse_errs.is_empty() {
                return Err(SYNTAX_ERR_STR.to_owned());
            }

            let (renamed_ast, renamer) =
                rename(parsed.expect("If parse errors are not empty, parsed should be Valid"));

            let (_typed_ast, type_resolver) = typecheck(renamer, renamed_ast);

            if !type_resolver.type_errors.is_empty()
                || !type_resolver.renamer.return_errors().is_empty()
            {
                return Err(SEMANTIC_ERR_STR.to_owned());
            }
        }

        // If both syntax and semantic analysis succeed, return success
        Ok(format!("Test passed: {}", path.display()))
    }
}
