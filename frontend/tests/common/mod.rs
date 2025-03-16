use frontend::StreamType;
use frontend::parsing::lexer::lexing_phase;
use frontend::parsing::parser::parsing_phase;
use frontend::source::StrSourceId;
use frontend::wacc_hir::ast_lowering_phase;
use frontend::wacc_thir::hir_lowering_phase;
use std::fs;
use std::io::stdout;
use std::path::{Path, PathBuf};

const SEMANTIC_ERR_CODE: i32 = 200;
const SYNTAX_ERR_CODE: i32 = 100;

const SYNTAX_ERR_STR: &str = "Syntax error(s) found!";
const SEMANTIC_ERR_STR: &str = "Semantic error(s) found!";

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
pub fn run_single_test(path: &Path) -> Result<String, String> {
    // create `Stdout` reference for printing
    let stdout = &stdout();

    let source = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read file {}: {}", path.display(), e);
            return Err(format!("File read error: {e}"));
        }
    };
    let source = source.as_str(); // reduce repetitive &str borrowings

    // Perform lexing (syntax analysis)
    let source_id = StrSourceId::from_path(path);
    let lexing_result = lexing_phase(
        source,
        source_id.clone(),
        SYNTAX_ERR_CODE,
        StreamType::Stderr,
        stdout,
    );

    // If there are syntax errors, return an appropriate result
    let tokens = match lexing_result {
        Ok(tokens) => tokens,
        Err(_) => return Err(SYNTAX_ERR_STR.to_owned()),
    };

    // Perform parsing (syntax analysis)
    let parsing_result = parsing_phase(
        source,
        source_id,
        tokens,
        SYNTAX_ERR_CODE,
        StreamType::Stderr,
        stdout,
    );

    // If there are syntax errors, return an appropriate result
    let ast_program = match parsing_result {
        Ok(program) => program,
        Err(_) => return Err(SYNTAX_ERR_STR.to_owned()),
    };

    // Perform AST lowering (renaming)
    let lowering_result = ast_lowering_phase(
        source,
        ast_program,
        SEMANTIC_ERR_CODE,
        StreamType::Stderr,
        stdout,
    );

    // If there are semantic errors, return an appropriate result
    let ast_lowering = match lowering_result {
        Ok(lowering) => lowering,
        Err(_) => return Err(SEMANTIC_ERR_STR.to_owned()),
    };

    // Perform HIR lowering (typechecking)
    let lowering_result = hir_lowering_phase(
        source,
        ast_lowering,
        SEMANTIC_ERR_CODE,
        StreamType::Stderr,
        stdout,
        true,
    );

    // If there are semantic errors, return an appropriate result
    match lowering_result {
        Err(_) => return Err(SEMANTIC_ERR_STR.to_owned()),
        _ => {}
    }

    // If both syntax and semantic analysis succeed, return success
    Ok(format!("Test passed: {}", path.display()))
}
