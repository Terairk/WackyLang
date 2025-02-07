use std::fs;
use std::path::{Path, PathBuf};
use chumsky::input::{Input, WithContext};
use chumsky::Parser;
use syntax::parser::program_parser;
use syntax::rename::rename;
use syntax::source::{SourcedSpan, StrSourceId};
use syntax::token::{lexer, Token};
use syntax::typecheck::typecheck;

static SYNTAX_ERR_STR: &str = "Syntax error(s) found!";
static SEMANTIC_ERR_STR: &str = "Semantic error(s) found!";

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
        &lexer::<WithContext<SourcedSpan, &str>>(),
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
        let (parsed, parse_errs) = program_parser().parse(spanned_tokens).into_output_errors();

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