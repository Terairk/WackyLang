use chumsky::error::Rich;
use chumsky::input::{Input, WithContext};
use chumsky::{Parser, extra};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use backend::assembly_fix::fix_program;
use backend::assembly_trans::wacky_to_assembly;
use backend::emission::AssemblyFormatter;
use backend::predefined::generate_predefined;
use backend::replace_pseudo::replace_pseudo_in_program;
use middle::ast_transform::lower_program;
use syntax::{ast, build_semantic_error_report, build_syntactic_report};
use syntax::parser::program_parser;
use syntax::rename::rename;
use syntax::source::{SourcedSpan, StrSourceId};
use syntax::token::{Token, lexer};
use syntax::typecheck::typecheck;

static SYNTAX_ERR_STR: &str = "Syntax error(s) found!";
static SEMANTIC_ERR_STR: &str = "Semantic error(s) found!";
static NO_OUTPUT_STR: &str = "No output/exit to compare if provided in a file";

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

/// Recursively collects all `.wacc` files from the given directory.
pub fn get_test_files(dir: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
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
pub fn compile_single_test(path: &Path) -> Result<String, String> {
    let source = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read file {}: {}", path.display(), e);
            return Err(format!("File read error: {e}"));
        }
    };

    let source_id = StrSourceId::repl();
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
        return Err(SYNTAX_ERR_STR.to_owned());
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
        return Err(SYNTAX_ERR_STR.to_owned());
    }

    // -------------------------------------------------------------------------
    //                          Renaming Phase
    // -------------------------------------------------------------------------

    let (renamed_ast, renamer) =
        rename(parsed.expect("If parse errors are not empty, parsed should be Valid"));

    let renamed_errors = renamer.return_errors();
    // we'll need this info later on so we return if error
    // but we still continue so we can identify type errors
    // as well
    let renamed_errors_not_empty = !renamed_errors.is_empty();
    if renamed_errors_not_empty {
        for e in &renamed_errors {
            build_semantic_error_report(
                &path
                    .to_str()
                    .expect("should always be able to find it")
                    .to_owned(), 
                e, source.clone()
            );
        }
    }

    // -------------------------------------------------------------------------
    //                          Typechecking Phase
    // -------------------------------------------------------------------------

    let (typed_ast, type_resolver) = typecheck(renamer, renamed_ast);
    
    let type_errors = &type_resolver.type_errors;
    if !type_errors.is_empty() {
        for e in type_errors {
            build_semantic_error_report(&path
                .to_str()
                .expect("should always be able to find it")
                .to_owned(), 
                e, source.clone()
            );
        }
        return Err(SEMANTIC_ERR_STR.to_owned());
    }

    if renamed_errors_not_empty {
        return Err(SEMANTIC_ERR_STR.to_owned());
    }

    // // TEMPORARY CARROT MARK
    // let output_file_path = format!("{file_path}.s");
    // match std::fs::write(&output_file_path, CARROT_ONE_ASM) {
    //     Ok(_) => {
    //         println!("Successfully wrote to file {output_file_path}");
    //     }
    //     Err(e) => {
    //         eprintln!("Failed to write to file {output_file_path}: {e}");
    //         return ExitCode::FAILURE;
    //     }
    // }

    // -------------------------------------------------------------------------
    //                          Wacky IR Pass
    // -------------------------------------------------------------------------

    // TODO: add string constant pass to either this pass or assembly pass
    // may need to modify my Wacky IR / Assembly Ast
    let (wacky_ir, counter, symbol_table) = lower_program(typed_ast, type_resolver);


    // -------------------------------------------------------------------------
    //                          Assembly Pass
    // -------------------------------------------------------------------------

    // TODO: find how to use asm_gen for future passes
    let (mut assembly_ast, _asm_gen) = wacky_to_assembly(wacky_ir, counter, symbol_table);

    // -------------------------------------------------------------------------
    //                      Replace Pseudoreg Pass
    // -------------------------------------------------------------------------

    replace_pseudo_in_program(&mut assembly_ast);

    // -------------------------------------------------------------------------
    //                      Fixing Instructions Pass
    // -------------------------------------------------------------------------

    let mut assembly_ast = fix_program(assembly_ast);

    // -------------------------------------------------------------------------
    //                          Code Generation Pass
    // -------------------------------------------------------------------------

    generate_predefined(&mut assembly_ast);
    let formatted_assembly =
        AssemblyFormatter::format_program(&assembly_ast, _asm_gen.str_literals);

    // -------------------------------------------------------------------------
    //                          Full Pipeline
    // -------------------------------------------------------------------------

    // Writes to file

    // Extract just the file name and remove .wacc extension
    let file_name = Path::new(&path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("output")
        .strip_suffix(".wacc")
        .unwrap_or("output");
    let output_file_path = format!("test_artifacts/{file_name}.s");
    match fs::write(&output_file_path, formatted_assembly) {
        Ok(()) => {
            println!("Full: Successfully wrote to file {output_file_path}");
        }
        Err(e) => {
            let err_msg = format!("Failed to write to file {output_file_path}: {e}");
            eprintln!("{}", err_msg);
            return Err(err_msg);
        }
    }


    // If both syntax and semantic analysis succeed, return success
    Ok(format!("Test passed: {}", path.display()))
}

/// Extract expected output from the test file.
fn extract_expected_output(source: &str) -> Vec<String> {
    let mut expected_output = Vec::new();
    let mut in_output_section = false;

    for line in source.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with("# Output:") {
            in_output_section = true;
            continue; // Skip the "# Output:" header
        }

        if in_output_section {
            // Stop if we hit an empty line or another comment section
            if trimmed.is_empty() || trimmed.starts_with("#") {
                break;
            }

            // Remove leading `#` and trim spaces
            expected_output.push(trimmed.trim_start_matches('#').trim().to_string());
        }
    }

    expected_output
}

/// If any output/exit is provided within a file, compare that with an actual test run 
pub fn compare_test_result(path: &Path) -> Result<String, String> {
    use std::process::{Command, Stdio};
    let source = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read file {}: {}", path.display(), e);
            return Err(format!("File read error: {e}"));
        }
    };
    
    let expected_output = extract_expected_output(&source);

    if expected_output.is_empty() {
        return Ok(NO_OUTPUT_STR.to_owned());
    }

    let file_name = path.file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("output");

    let asm_path = format!("test_artifacts/{file_name}.s");
    let bin_path = format!("test_artifacts/{file_name}");
    
    // Step 1: Compile Assembly to Executable
    let compile_status = Command::new("gcc")
        .args(["-o", &bin_path, &asm_path, "-no-pie"])
        .status()
        .map_err(|e| format!("GCC error: {e}"))?;

    if !compile_status.success() {
        return Err("Failed to compile assembly".to_string());
    }

    // Step 2: Run the compiled program and capture output
    let output = Command::new(&bin_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("Execution error: {e}"))?;

    let actual_output = String::from_utf8_lossy(&output.stdout)
        .trim()
        .to_string();
    let expected_output_str = expected_output.join("\n");

    // Step 3: Compare output
    if actual_output == expected_output_str {
        Ok("Test passed!".to_string())
    } else {
        Err(format!(
            "Test failed!\nExpected:\n{}\n\nGot:\n{}",
            expected_output_str, actual_output
        ))
    }
}
