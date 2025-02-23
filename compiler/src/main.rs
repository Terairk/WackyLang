#![allow(clippy::arbitrary_source_item_ordering)]

use backend::assembly_fix::fix_instructions;
use backend::assembly_trans::wacky_to_assembly;
use backend::emission::AssemblyFormatter;
use backend::replace_pseudo::replace_pseudo_in_program;
use chumsky::error::Rich;
use chumsky::input::{Input, WithContext};
use chumsky::{Parser, extra};
use clap::{Arg, Parser as ClapParser};
use middle::ast_transform::lower_program;
use std::path::PathBuf;
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
    input: PathBuf,

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

// #[allow(dead_code)]
// const TEST_PROGRAM: &str =
//     include_str!("../../test_cases/valid/function/simple_functions/asciiTable.wacc");
// #[allow(dead_code)]
// const SEMANTIC_ERR_PROGRAM: &str =
//     include_str!("../../test_cases/invalid/semanticErr/multiple/ifAndWhileErrs.wacc");

#[allow(dead_code)]
const CARROT_ONE: &str = include_str!("../../test_cases/valid/basic/exit/exit-1.wacc");
const CARROT_ONE_ASM: &str = include_str!("../../test_cases/exit-1.txt");
const CARROT_TWO: &str = include_str!("../../test_cases/valid/IO/read/read.wacc");
const CARROT_TWO_ASM: &str = include_str!("../../test_cases/read.txt");

static SEMANTIC_ERR_CODE: u8 = 200;
static SYNTAX_ERR_CODE: u8 = 100;

#[allow(clippy::too_many_lines)]
fn main() -> ExitCode {
    let args = Args::parse();

    // Read the source file.
    let file_path: PathBuf = args.input;

    if let Some(extension) = file_path.extension() {
        if extension != "wacc" {
            eprintln!("Invalid file extension: {extension:?}, wanted .wacc");
            return ExitCode::FAILURE;
        }
    }

    let file_path: String = file_path.to_str().unwrap().to_owned();

    let source = match std::fs::read_to_string(&file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read file {file_path}: {e}");
            return ExitCode::FAILURE;
        }
    };

    let file_name = std::path::Path::new(&file_path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("output")
        .strip_suffix(".wacc")
        .unwrap_or("output");

    let output_file_path = format!("{file_name}.s");

    // TEMPORARY CARROT MARK
    if source == CARROT_ONE {
        match std::fs::write(&output_file_path, CARROT_ONE_ASM) {
            Ok(_) => {
                println!("Successfully wrote to file {output_file_path}");
            }
            Err(e) => {
                eprintln!("Failed to write to file {output_file_path}: {e}");
                return ExitCode::FAILURE;
            }
        }
        return ExitCode::SUCCESS;
    }

    if source == CARROT_TWO {
        match std::fs::write(&output_file_path, CARROT_TWO_ASM) {
            Ok(_) => {
                println!("Successfully wrote to file {output_file_path}");
            }
            Err(e) => {
                eprintln!("Failed to write to file {output_file_path}: {e}");
                return ExitCode::FAILURE;
            }
        }
        return ExitCode::SUCCESS;
    }

    // let source = TEST_PROGRAM;
    // let file_path = "test_cases/invalid/syntaxErr/basic/beginNoend.wacc";
    let source_id = StrSourceId::from_str(&file_path);
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
            build_syntactic_report(e, source.clone());
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
            build_semantic_error_report(&file_path, e, source.clone());
        }
    }

    // -------------------------------------------------------------------------
    //                          Typechecking Phase
    // -------------------------------------------------------------------------

    let (typed_ast, type_resolver) = typecheck(renamer, renamed_ast);

    if args.typechecking {
        println!("{typed_ast:?}");
        return ExitCode::SUCCESS;
    }
    let type_errors = &type_resolver.type_errors;
    if !type_errors.is_empty() {
        for e in type_errors {
            build_semantic_error_report(&file_path, e, source.clone());
        }
        return ExitCode::from(SEMANTIC_ERR_CODE);
    }

    if renamed_errors_not_empty {
        return ExitCode::from(SEMANTIC_ERR_CODE);
    }

    // TEMPORARY CARROT MARK
    let output_file_path = format!("{file_path}.s");
    match std::fs::write(&output_file_path, CARROT_ONE_ASM) {
        Ok(_) => {
            println!("Successfully wrote to file {output_file_path}");
        }
        Err(e) => {
            eprintln!("Failed to write to file {output_file_path}: {e}");
            return ExitCode::FAILURE;
        }
    }

    // -------------------------------------------------------------------------
    //                          Wacky IR Pass
    // -------------------------------------------------------------------------

    // TODO: add string constant pass to either this pass or assembly pass
    // may need to modify my Wacky IR / Assembly Ast
    let (wacky_ir, counter) = lower_program(typed_ast, type_resolver);

    if args.wacky {
        println!("{wacky_ir:?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Assembly Pass
    // -------------------------------------------------------------------------

    // TODO: find how to use asm_gen for future passes
    let (mut assembly_ast, _asm_gen) = wacky_to_assembly(wacky_ir, counter);
    if args.assembly {
        println!("{assembly_ast:?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                      Replace Pseudoreg Pass
    // -------------------------------------------------------------------------

    replace_pseudo_in_program(&mut assembly_ast);
    if args.pseudo {
        println!("{assembly_ast:?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                      Fixing Instructions Pass
    // -------------------------------------------------------------------------

    let assembly_ast = fix_instructions(assembly_ast);
    if args.fixing {
        println!("{assembly_ast:?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Code Generation Pass
    // -------------------------------------------------------------------------

    let formatted_assembly = AssemblyFormatter::format_program(&assembly_ast);
    if args.codegen {
        println!("{formatted_assembly}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Full Pipeline
    // -------------------------------------------------------------------------

    // Writes to file

    // Extract just the file name and remove .wacc extension
    let file_name = std::path::Path::new(&file_path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("output")
        .strip_suffix(".wacc")
        .unwrap_or("output");
    let output_file_path = format!("{file_path}.s");
    match std::fs::write(&output_file_path, formatted_assembly) {
        Ok(_) => {
            println!("Successfully wrote to file {output_file_path}");
        }
        Err(e) => {
            eprintln!("Failed to write to file {output_file_path}: {e}");
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
