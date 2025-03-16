use backend::assembly_fix::fix_program;
use backend::assembly_trans::wacky_to_assembly;
use backend::emission::AssemblyFormatter;
use backend::predefined::generate_predefined;
use backend::regalloc::allocate_registers_program;
use backend::replace_pseudo::replace_pseudo_in_program;
use frontend::StreamType;
use frontend::parsing::lexer::lexing_phase;
use frontend::parsing::parser::parsing_phase;
use frontend::source::StrSourceId;
use frontend::wacc_hir::ast_lowering_phase;
use frontend::wacc_thir::hir_lowering_phase;
use middle::optimizations::optimize;
use middle::thir_transform::lower_program;
use regex::Regex;
use std::fs;
use std::fs::create_dir_all;
use std::io::{Write, stdout};
use std::ops::Add;
use std::path::{Path, PathBuf};
use util::gen_flags::reset_flags_gbl;
use util::opt_flags::OptimizationConfig;

const SEMANTIC_ERR_CODE: i32 = 200;
const SYNTAX_ERR_CODE: i32 = 100;

const SYNTAX_ERR_STR: &str = "Syntax error(s) found!";
const SEMANTIC_ERR_STR: &str = "Semantic error(s) found!";
const NO_OUTPUT_STR: &str = "No output/exit to compare if provided in a file";

#[cfg(feature = "fold")]
const CONST_FOLD: bool = true;

#[cfg(not(feature = "fold"))]
const CONST_FOLD: bool = false;

#[cfg(feature = "copy-prop")]
const COPY_PROP: bool = true;

#[cfg(not(feature = "copy-prop"))]
const COPY_PROP: bool = false;

#[cfg(feature = "rm-unreachable")]
const RM_UNREACHABLE: bool = true;

#[cfg(not(feature = "rm-unreachable"))]
const RM_UNREACHABLE: bool = false;

#[cfg(feature = "rm-dead-stores")]
const RM_DEAD_STORES: bool = true;

#[cfg(not(feature = "rm-dead-stores"))]
const RM_DEAD_STORES: bool = false;

#[cfg(feature = "reg-alloc")]
const REG_ALLOC: bool = true;

#[cfg(not(feature = "reg-alloc"))]
const REG_ALLOC: bool = false;

#[cfg(feature = "reg-coalesce")]
const REG_COALESCE: bool = true;

#[cfg(not(feature = "reg-coalesce"))]
const REG_COALESCE: bool = false;

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

/// Output of compiling single test
#[allow(dead_code)]
pub struct CompileSingleTestOutput {
    pub message: String,
    pub has_read_instructions: bool,
}

#[allow(clippy::too_many_lines)]
pub fn compile_single_test(path: &Path) -> Result<CompileSingleTestOutput, String> {
    reset_flags_gbl();
    let opt_config = OptimizationConfig::builder()
        .fold_constants(CONST_FOLD)
        .copy_propagation(COPY_PROP)
        .eliminate_unreachable_code(RM_UNREACHABLE)
        .eliminate_dead_stores(RM_DEAD_STORES)
        .print_cfg(false)
        .reg_alloc(REG_ALLOC)
        .reg_coalesce(REG_COALESCE)
        .tailrec(true)
        .build();

    // println!("Compiling: {}", path.display());
    // println!("Optimization Config: {:#?}", opt_config);

    // create `Stdout` reference for printing
    let stdout = &stdout();

    // load source, and source-ID
    let source = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read file {}: {}", path.display(), e);
            return Err(format!("File read error: {e}"));
        }
    };
    let source = source.as_str(); // reduce repetitive &str borrowings
    let source_id = StrSourceId::repl();

    // -------------------------------------------------------------------------
    //                          Lexing Phase
    // -------------------------------------------------------------------------

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

    // -------------------------------------------------------------------------
    //                          Parsing Phase
    // -------------------------------------------------------------------------

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

    // -------------------------------------------------------------------------
    //                          Renaming Phase
    // -------------------------------------------------------------------------

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

    // -------------------------------------------------------------------------
    //                          Typechecking Phase
    // -------------------------------------------------------------------------

    // Perform HIR lowering (typechecking)
    let lowering_result = hir_lowering_phase(
        source,
        ast_lowering,
        SEMANTIC_ERR_CODE,
        StreamType::Stderr,
        stdout,
        opt_config.should_tailrec_optimize(),
    );

    // If there are semantic errors, return an appropriate result
    let hir_lowering = match lowering_result {
        Ok(lowering) => lowering,
        Err(_) => return Err(SEMANTIC_ERR_STR.to_owned()),
    };

    // -------------------------------------------------------------------------
    //                          Wacky IR Pass
    // -------------------------------------------------------------------------

    // TODO: add string constant pass to either this pass or assembly pass
    // may need to modify my Wacky IR / Assembly Ast
    let (wacky_ir, counter, symbol_table) = lower_program(hir_lowering);
    let wacky_ir = optimize(wacky_ir, opt_config);
    let has_read_instructions = wacky_ir.has_read_instr();

    // -------------------------------------------------------------------------
    //                          Assembly Pass
    // -------------------------------------------------------------------------

    // TODO: find how to use asm_gen for future passes
    let (mut assembly_ast, asm_gen, mut function_callee_regs) =
        wacky_to_assembly(wacky_ir, counter, symbol_table);

    // -------------------------------------------------------------------------
    //                     Register Allocation Pass
    // -------------------------------------------------------------------------

    if REG_ALLOC {
        assembly_ast = allocate_registers_program(
            assembly_ast,
            &asm_gen.function_regs,
            &mut function_callee_regs,
            REG_COALESCE,
        );
    }

    // -------------------------------------------------------------------------
    //                      Replace Pseudoreg Pass
    // -------------------------------------------------------------------------

    replace_pseudo_in_program(
        &mut assembly_ast,
        &asm_gen.symbol_table,
        &function_callee_regs,
    );

    // -------------------------------------------------------------------------
    //                      Fixing Instructions Pass
    // -------------------------------------------------------------------------

    let mut assembly_ast = fix_program(assembly_ast, &function_callee_regs);

    // -------------------------------------------------------------------------
    //                          Code Generation Pass
    // -------------------------------------------------------------------------

    generate_predefined(&mut assembly_ast);
    let formatted_assembly = AssemblyFormatter::format_program(&assembly_ast, asm_gen.str_literals);

    // -------------------------------------------------------------------------
    //                          Full Pipeline
    // -------------------------------------------------------------------------

    // Writes to file

    // Convert `path` to a relative path starting from `test_cases/valid`
    let valid_tests_dir = Path::new("../test_cases/valid");
    let test_artifacts_dir = Path::new("test_artifacts");

    // Ensure `path` is within `valid_tests`
    let relative_path = path.strip_prefix(valid_tests_dir).unwrap_or(path);

    // Construct the output path in `test_artifacts` with a `.s` extension
    let output_file_path = test_artifacts_dir.join(relative_path).with_extension("s");
    let output_file_path_str = output_file_path.to_str().expect("always exist");

    // Ensure the parent directory exists
    if let Some(parent) = output_file_path.parent() {
        create_dir_all(parent)
            .map_err(|e| format!("Failed to create directory {}: {e}", parent.display()))?;
    }

    match fs::write(&output_file_path, formatted_assembly) {
        Ok(()) => {
            // println!("Full: Successfully wrote to file {}", output_file_path_str);
        }
        Err(e) => {
            let err_msg = format!("Failed to write to file {output_file_path_str}: {e}");
            eprintln!("{}", err_msg);
            return Err(err_msg);
        }
    }

    // If both syntax and semantic analysis succeed, return success
    Ok(CompileSingleTestOutput {
        message: format!("Test compiled: {}", path.display()),
        has_read_instructions,
    })
}

/// Extract expected output from the test file.
fn extract_expected_output(source: &str) -> Vec<String> {
    // if source.contains("read") {
    //     println!("{source}");
    //     return Vec::new();
    // }
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
            // println!("Trimmed in output: {trimmed}");
            if trimmed == "#" || trimmed.is_empty() {
                break;
            }
            // Remove leading `#` and trim spaces
            expected_output.push(trimmed.trim_start_matches('#').trim().to_string());
        }
    }

    expected_output
}

fn is_interactive(source: &str, has_read_instr: bool) -> bool {
    let in_str = "# Input:";
    if has_read_instr && !source.contains(in_str) {
        return true;
    }
    false
}
/// Extract input from the test file.
fn extract_input(source: &str) -> Option<String> {
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("# Input: ") {
            return Some(
                trimmed
                    .to_owned()
                    .trim_start_matches("# Input: ")
                    .to_owned()
                    .add("\n"),
            );
        }
    }
    None
}

/// Replaces actual output runtime errors/addresses with appropriate blocks from expected output
pub fn transform_actual_output_to_expected_form(output: String) -> String {
    let mut transformed_output = Vec::new();
    let hex_address_regex = Regex::new(r"0x[0-9a-fA-F]+").unwrap();

    for line in output.lines() {
        if line.contains("fatal error:") || line.contains("Error: Division by zero") {
            transformed_output.push("#runtime_error#".to_string());
        } else if hex_address_regex.is_match(line) {
            transformed_output.push(hex_address_regex.replace_all(line, "#addrs#").to_string());
        } else {
            transformed_output.push(line.to_string());
        }
    }

    transformed_output.join("\n")
}

/// If any output/exit is provided within a file, compare that with an actual test run
pub fn compare_test_result(path: &Path, has_read_instructions: bool) -> Result<String, String> {
    use std::process::{Command, Stdio};
    let source = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read file {}: {}", path.display(), e);
            return Err(format!("File read error: {e}"));
        }
    };

    let input = extract_input(&source);
    let expected_output = extract_expected_output(&source);
    println!("expected_output: {:?}", expected_output);
    println!("input: {:?}", input);
    println!("{:?}", path);
    if is_interactive(&source, has_read_instructions) {
        return Ok(NO_OUTPUT_STR.to_owned());
    }

    // Convert `path` to a relative path starting from `test_cases/valid`
    let valid_tests_dir = Path::new("../test_cases/valid");
    let test_artifacts_dir = Path::new("test_artifacts");

    // Ensure `path` is within `valid_tests`
    let relative_path = path.strip_prefix(valid_tests_dir).unwrap_or(path);

    let asm_path = test_artifacts_dir.join(relative_path).with_extension("s");
    let bin_path = test_artifacts_dir.join(relative_path);

    // Step 1: Compile Assembly to Executable
    let output = Command::new("gcc")
        .args([
            "-o",
            &bin_path.to_str().unwrap(),
            "-z noexecstack",
            &asm_path.to_str().unwrap(),
        ])
        .stderr(Stdio::piped()) // Capture stderr
        .output()
        .map_err(|e| format!("GCC execution error: {e}"))?;

    if !output.status.success() {
        let error_message = String::from_utf8_lossy(&output.stderr);
        return Err(format!("failed to assemble: \n{}", error_message));
    }

    // Step 2: Run the compiled program and capture output
    let mut child = Command::new(&bin_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to spawn process: {e}"))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(input.unwrap_or(String::new()).replace(" ", "\n").as_bytes())
            .map_err(|e| format!("Failed to write to stdin: {e}"))?;
    }
    println!("!!!!");
    let output = child
        .wait_with_output()
        .map_err(|e| format!("Execution error: {e}"))?;
    // let output = Command::new(&bin_path)
    //     .stdin(Stdio::piped())
    //     .stdout(Stdio::piped())
    //     .stderr(Stdio::piped())
    //     .output()
    //     .map_err(|e| format!("Execution error: {e}"))?;

    let actual_output = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let transformed_actual_output = transform_actual_output_to_expected_form(actual_output);
    let expected_output_str = expected_output.join("\n");

    // Step 3: Compare output
    let test_result = if transformed_actual_output == expected_output_str {
        Ok("Test passed!".to_string())
    } else {
        Err(format!(
            "non matching output - expected: \n{}\nbut got: \n{}",
            expected_output_str, transformed_actual_output
        ))
    };

    // Step 4: Cleanup (delete binary file)
    if let Err(e) = fs::remove_file(&bin_path) {
        eprintln!("Failed to delete binary {}: {}", bin_path.display(), e);
    }

    test_result
}
