#![allow(clippy::arbitrary_source_item_ordering)]

use chumsky::error::Rich;
use chumsky::input::{Input, WithContext};
use chumsky::{Parser, extra};
use clap::{Arg, Parser as ClapParser};
use std::process::ExitCode;
use syntax::ast;
use syntax::parser::program_parser;
use syntax::rename::rename;
use syntax::source::{SourcedSpan, StrSourceId};
use syntax::token::{Token, lexer};
use syntax::typecheck::typecheck;
use syntax::{build_semantic_error_report, build_syntactic_report};

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

#[derive(ClapParser)]
#[command(author, version, about)]
struct Args {
    /// The input WACC file path
    input: String,

    /// Stop after lexing phase and print tokens
    #[arg(long)]
    lexing: bool,

    /// Stop after initial parsing phase and print AST
    #[arg(long)]
    parsing: bool,

    /// Stop after renaming phase and print the renamedAST
    #[arg(long)]
    renaming: bool,

    /// Stop after typechecker phase and print the typedAST
    #[arg(long)]
    typechecking: bool,

    /// Stop after "Wacky IR" phase and print the IR
    #[arg(long)]
    wacky: bool,

    /// Stop after the assembly phase and print the AssemblyAST
    #[arg(long)]
    assembly: bool,

    /// Stop after replacing pseudo registers
    #[arg(long)]
    pseudo: bool,

    /// Stop after fixing mov instructions
    #[arg(long)]
    fixing: bool,

    /// Stop after code generation phase and print the final assembly code
    #[arg(long)]
    codegen: bool,
}

#[allow(dead_code)]
const TEST_PROGRAM: &str =
    include_str!("../../test_cases/valid/function/simple_functions/asciiTable.wacc");
#[allow(dead_code)]
const SEMANTIC_ERR_PROGRAM: &str =
    include_str!("../../test_cases/invalid/semanticErr/multiple/ifAndWhileErrs.wacc");

static SEMANTIC_ERR_CODE: u8 = 200;
static SYNTAX_ERR_CODE: u8 = 100;

fn main() -> ExitCode {
    let args = Args::parse();

    // Read the source file.
    let file_path = &args.input;

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

    // -------------------------------------------------------------------------
    //                          Lexing Phase
    // -------------------------------------------------------------------------

    // so the pattern is, make everything generic asf and supply the concrete implementations later :)
    let (tokens, lexing_errs): LexerOutput = Parser::parse(
        &lexer::<_, LexerExtra>(),
        source.with_context((source_id, ())),
    )
    .into_output_errors();

    // Done to appease the borrow checker while displaying errors
    if !lexing_errs.is_empty() {
        for e in &lexing_errs {
            build_syntactic_report(e, source.clone());
        }
        return ExitCode::from(SYNTAX_ERR_CODE);
    }

    if args.lexing {
        println!("{tokens:#?}");
        return ExitCode::SUCCESS;
    }
    let tokens = tokens.expect("If lexing errors are not empty, tokens should be Valid");
    // attach the span of each token to it before parsing, so it is not forgotten
    let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));

    // -------------------------------------------------------------------------
    //                          Parsing Phase
    // -------------------------------------------------------------------------

    #[allow(clippy::pattern_type_mismatch)]
    let (parsed, parse_errs): ProgramOutput =
        Parser::parse(&program_parser::<_, ProgramExtra>(), spanned_tokens).into_output_errors();

    if !parse_errs.is_empty() {
        for e in &parse_errs {
            build_syntactic_report(&e, source.clone());
        }
        return ExitCode::from(SYNTAX_ERR_CODE);
    }

    if args.parsing {
        println!("{parsed:#?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Renaming Phase
    // -------------------------------------------------------------------------

    let (renamed_ast, renamer) =
        rename(parsed.expect("If parse errors are not empty, parsed should be Valid"));

    if args.renaming {
        println!("{renamed_ast:#?}");
        return ExitCode::from(SEMANTIC_ERR_CODE);
    }
    let renamed_errors = renamer.return_errors();
    // we'll need this info later on so we return if error
    // but we still continue so we can identify type errors
    // as well
    let renamed_errors_not_empty = !renamed_errors.is_empty();
    if renamed_errors_not_empty {
        for e in &renamed_errors {
            build_semantic_error_report(file_path, e, source.clone());
        }
    }

    // -------------------------------------------------------------------------
    //                          Typechecking Phase
    // -------------------------------------------------------------------------

    let (typed_ast, type_resolver) = typecheck(renamer, renamed_ast);

    if args.typechecking {
        println!("{typed_ast:?}");
        return ExitCode::from(SEMANTIC_ERR_CODE);
    }
    let type_errors = type_resolver.type_errors;
    if !type_errors.is_empty() {
        for e in &type_errors {
            build_semantic_error_report(file_path, e, source.clone());
        }
        return ExitCode::from(SEMANTIC_ERR_CODE);
    }

    if renamed_errors_not_empty {
        return ExitCode::from(SEMANTIC_ERR_CODE);
    }

    // -------------------------------------------------------------------------
    //                          Assembly Pass
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //                      Replace Pseudoreg Pass
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //                      Fixing Instructions Pass
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //                          Code Generation Pass
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    //                          Full Pipeline
    // -------------------------------------------------------------------------

    // Writes to file

    ExitCode::SUCCESS
}
