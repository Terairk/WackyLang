#![allow(clippy::arbitrary_source_item_ordering)]

use ariadne::{CharSet, Label, Report, Source};
use chumsky::error::Rich;
use chumsky::input::{Input, WithContext};
use chumsky::prelude::Input as _;
use chumsky::{extra, Parser};
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::process::ExitCode;
use syntax::ast;
use syntax::error::{semantic_error_to_reason, SemanticError};
use syntax::parser::program_parser;
use syntax::rename::rename;
use syntax::source::{SourcedSpan, StrSourceId};
use syntax::token::{lexer, Token};
use syntax::typecheck::typecheck;

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

static SEMANTIC_ERR_CODE: u8 = 200;
static SYNTAX_ERR_CODE: u8 = 100;

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
        build_syntactic_report(e.span().clone(), e, source.clone());
    }

    if lexing_errs_not_empty {
        return ExitCode::from(SYNTAX_ERR_CODE);
    }

    if let Some(tokens) = tokens {
        // println!("{:#?}", DisplayVec(tokens.clone())); // TODO: remove this later

        // attach the span of each token to it before parsing, so it is not forgotten
        #[allow(clippy::pattern_type_mismatch)]
        let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));
        // println!("\n\nEOI span done!!!");
        let (parsed, parse_errs): ProgramOutput =
            Parser::parse(&program_parser::<_, ProgramExtra>(), spanned_tokens)
                .into_output_errors();

        // println!(
        //     "Done parsing\nParse errors: {:#?}\nParsed: {:#?}",
        //     parse_errs, parsed
        // );

        let parse_errs_not_empty = !parse_errs.is_empty();

        for e in parse_errs {
            build_syntactic_report(e, source.clone());
        }
        if parse_errs_not_empty {
            return ExitCode::from(SYNTAX_ERR_CODE);
        }

        let (renamed_ast, renamer) =
            rename(parsed.expect("If parse errors are not empty, parsed should be Valid"));

        let renamed_errors = renamer.return_errors();
        let renamed_errors_not_empty = !renamed_errors.is_empty();
        if renamed_errors_not_empty {
            for e in renamed_errors {
                build_semantic_error_report(file_path, &e, source.clone());
            }
        }

        let (_typed_ast, type_resolver) = typecheck(renamer, renamed_ast);
        // println!("{_typed_ast:?}");
        let type_errors = type_resolver.type_errors;
        if !type_errors.is_empty() {
            for e in type_errors {
                build_semantic_error_report(file_path, &e, source.clone());
            }
            return ExitCode::from(SEMANTIC_ERR_CODE);
        }

        if renamed_errors_not_empty {
            return ExitCode::from(SEMANTIC_ERR_CODE);
        }
    }

    ExitCode::SUCCESS
}

#[allow(clippy::unwrap_used)]
pub fn build_syntactic_report<T>(span: SourcedSpan, error: Rich<T, SourcedSpan>, source: String)
where
    T: fmt::Display,
{
    let config = ariadne::Config::default().with_char_set(CharSet::Ascii);
    Report::build(ariadne::ReportKind::Error, span.clone())
        .with_config(config)
        .with_message(format!("Syntax error"))
        .with_code(69)
        .with_label(Label::new(span.clone()).with_message(error.reason().to_string()))
        .with_labels(error.contexts().map(|(label, span)| {
            Label::new(span.clone()).with_message(format!("while parsing this {label}"))
        }))
        .finish()
        .print((span.source_id().clone(), Source::from(source)))
        .unwrap();
}

#[allow(clippy::unwrap_used)]
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
        SemanticError::InvalidNumberOfIndexes(span, _, _) => {
            semantic_report_helper(file_path, "Wrong number of indexes", error, &span, source);
        }
        // Handle other error variants similarly
        _ => {
            // Generic error report for other cases
            let span = match error {
                SemanticError::ArityMismatch(node, _, _) => node.span().clone(),
                SemanticError::AssignmentWithBothSidesUnknown(span) => span.clone(),
                SemanticError::TypeMismatch(span, _, _) => span.clone(),
                SemanticError::InvalidFreeType(span, _) => span.clone(),
                SemanticError::MismatchedArgCount(span, _, _) => span.clone(),
                SemanticError::InvalidIndexType(span, _) => span.clone(),
                SemanticError::UndefinedIdent(node) => node.span().clone(),
                SemanticError::ReturnInMain(span) => span.clone(),
                _ => panic!("Unhandled error variant"),
            };
            semantic_report_helper(file_path, "Semantic Error", error, &span, source);
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
