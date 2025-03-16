#![allow(clippy::arbitrary_source_item_ordering)]

use backend::assembly_fix::fix_program;
use backend::assembly_trans::wacky_to_assembly;
use backend::emission::AssemblyFormatter;
use backend::predefined::generate_predefined;
use backend::regalloc::allocate_registers_program;
use backend::replace_pseudo::replace_pseudo_in_program;
use clap::Parser as ClapParser;
use frontend::StreamType;
use frontend::parsing::lexer::lexing_phase;
use frontend::parsing::parser::parsing_phase;
use frontend::source::StrSourceId;
use frontend::wacc_hir::ast_lowering_phase;
use frontend::wacc_thir::hir_lowering_phase;
use middle::thir_transform::lower_program;
use std::io::stdout;
use std::path::PathBuf;
use std::process::ExitCode;
use util::opt_flags::OptimizationConfig;

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

    /// Stop after register allocation and print the info
    #[arg(long)]
    print_reg_alloc: bool,

    /// Stop after replacing pseudo registers
    #[arg(long)]
    pseudo: bool,

    /// Stop after fixing mov instructions
    #[arg(long)]
    fixing: bool,

    /// Stop after code generation phase and print the final assembly code
    #[arg(long)]
    codegen: bool,

    /* ==================== Optimization Flags ==================== */
    /// Enable constant folding
    #[arg(long = "fold")]
    fold_constants: bool,

    /// Enable copy propagation
    #[arg(long = "copy-prop")]
    copy_propagation: bool,

    /// Eliminate unreachable/dead code
    #[arg(long = "rm-unreachable")]
    eliminate_unreachable_code: bool,

    /// Eliminate dead stores
    #[arg(long = "rm-dead-stores")]
    eliminate_dead_stores: bool,

    /// Whether to perform register allocation
    #[arg(long = "reg-alloc")]
    reg_alloc: bool,

    /// Whether to coalesce registers
    #[arg(long = "reg-coalesce")]
    reg_coalesce: bool,

    /// Whether to optimize tail recursion
    #[arg(long = "tailrec")]
    tailrec: bool,

    /// Whether to print the CFG
    #[arg(long = "print-cfg")]
    print_cfg: bool,

    /// Enable all optimizations
    #[arg(long = "O2")]
    optimize: bool,

    /// Enable all optimizations except coalescing
    #[arg(long = "O1")]
    optimize_no_coalesce: bool,

    /// Enable all non register related optimizations
    #[arg(long = "O0")]
    optimize_no_reg: bool,
}

impl Args {
    ///
    /// Reconstruct an OptimizationConfig from the command line arguments
    /// that we can pass to the optimization passes. Dead stores require constant folding or else
    /// it'll miss runtime errors
    fn get_optimization_config(&self) -> OptimizationConfig {
        OptimizationConfig::builder()
            .fold_constants(
                self.optimize
                    || self.fold_constants
                    || self.optimize_no_coalesce
                    || self.optimize_no_reg
                    || self.eliminate_dead_stores,
            )
            .copy_propagation(
                self.optimize
                    || self.optimize_no_reg
                    || self.copy_propagation
                    || self.optimize_no_coalesce,
            )
            .eliminate_unreachable_code(
                self.optimize
                    || self.eliminate_unreachable_code
                    || self.optimize_no_reg
                    || self.optimize_no_coalesce,
            )
            .eliminate_dead_stores(
                self.optimize
                    || self.eliminate_dead_stores
                    || self.optimize_no_coalesce
                    || self.optimize_no_reg,
            )
            .reg_alloc(self.optimize || self.reg_alloc || self.optimize_no_coalesce)
            .reg_coalesce(self.optimize || self.reg_coalesce)
            .tailrec(
                self.optimize || self.tailrec || self.optimize_no_coalesce || self.optimize_no_reg,
            )
            .print_cfg(self.print_cfg)
            .build()
    }
}

const SEMANTIC_ERR_CODE: u8 = 200;
const SYNTAX_ERR_CODE: u8 = 100;

#[allow(clippy::too_many_lines)]
fn main() -> ExitCode {
    let args = Args::parse();

    let optimization_config = args.get_optimization_config();
    // Read the source file.
    let file_path: PathBuf = args.input;

    if let Some(extension) = file_path.extension() {
        if extension != "wacc" {
            eprintln!("Invalid file extension: {extension:?}, wanted .wacc");
            return ExitCode::FAILURE;
        }
    }

    let file_path: String = file_path.to_str().unwrap().to_owned();

    // create `Stdout` reference for printing
    let stdout = &stdout();

    // load source, and source-ID
    let source = match std::fs::read_to_string(&file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read file {file_path}: {e}");
            return ExitCode::FAILURE;
        }
    };
    let source = source.as_str(); // reduce repetitive &str borrowings
    let source_id = StrSourceId::from_str(&file_path);

    // -------------------------------------------------------------------------
    //                          Lexing Phase
    // -------------------------------------------------------------------------

    let lexing_result = lexing_phase(
        source,
        source_id.clone(),
        SYNTAX_ERR_CODE as i32,
        StreamType::Stderr,
        stdout,
    );

    // If there are syntax errors, return an appropriate result
    let tokens = match lexing_result {
        Ok(tokens) => tokens,
        Err(_) => return ExitCode::from(SYNTAX_ERR_CODE),
    };

    if args.lexing {
        println!("{tokens:#?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Parsing Phase
    // -------------------------------------------------------------------------

    let parsing_result = parsing_phase(
        source,
        source_id,
        tokens,
        SYNTAX_ERR_CODE as i32,
        StreamType::Stderr,
        stdout,
    );

    // If there are syntax errors, return an appropriate result
    let ast_program = match parsing_result {
        Ok(program) => program,
        Err(_) => return ExitCode::from(SYNTAX_ERR_CODE),
    };

    if args.parsing {
        println!("{ast_program:#?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Renaming Phase
    // -------------------------------------------------------------------------

    let lowering_result = ast_lowering_phase(
        source,
        ast_program,
        SEMANTIC_ERR_CODE as i32,
        StreamType::Stderr,
        stdout,
    );

    // If there are semantic errors, return an appropriate result
    let ast_lowering = match lowering_result {
        Ok(lowering) => lowering,
        Err(_) => return ExitCode::from(SEMANTIC_ERR_CODE),
    };

    if args.renaming {
        println!("{:#?}", ast_lowering.hir_program);
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Typechecking Phase
    // -------------------------------------------------------------------------

    // Perform HIR lowering (typechecking)
    let lowering_result = hir_lowering_phase(
        source,
        ast_lowering,
        SEMANTIC_ERR_CODE as i32,
        StreamType::Stderr,
        stdout,
        optimization_config.should_tailrec_optimize(),
    );

    // If there are semantic errors, return an appropriate result
    let hir_lowering = match lowering_result {
        Ok(lowering) => lowering,
        Err(_) => return ExitCode::from(SEMANTIC_ERR_CODE),
    };

    if args.typechecking {
        println!("{:?}", hir_lowering.thir_program);
        println!("{:?}", hir_lowering.hir_ident_symbol_table);
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Wacky IR Pass
    // -------------------------------------------------------------------------

    // TODO: add string constant pass to either this pass or assembly pass
    // may need to modify my Wacky IR / Assembly Ast
    let (wacky_ir, counter, symbol_table) = lower_program(hir_lowering);
    let wacky_ir = middle::optimizations::optimize(wacky_ir, optimization_config);

    if args.wacky {
        println!("{wacky_ir:#?}");
        // println!("{symbol_table:#?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Assembly Pass
    // -------------------------------------------------------------------------

    // TODO: find how to use asm_gen for future passes
    let (mut assembly_ast, asm_gen, mut function_callee_regs) =
        wacky_to_assembly(wacky_ir, counter, symbol_table);

    if args.assembly {
        println!("{assembly_ast:#?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                     Register Allocation Pass
    // -------------------------------------------------------------------------

    if optimization_config.has_reg_alloc() {
        assembly_ast = allocate_registers_program(
            assembly_ast,
            &asm_gen.function_regs,
            &mut function_callee_regs,
            optimization_config.has_reg_coalesce(),
        );
        if args.print_reg_alloc {
            println!("{assembly_ast:#?}");
            return ExitCode::SUCCESS;
        }
    }

    // -------------------------------------------------------------------------
    //                      Replace Pseudoreg Pass
    // -------------------------------------------------------------------------

    replace_pseudo_in_program(
        &mut assembly_ast,
        &asm_gen.symbol_table,
        &function_callee_regs,
    );
    if args.pseudo {
        println!("{assembly_ast:#?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                      Fixing Instructions Pass
    // -------------------------------------------------------------------------

    let mut assembly_ast = fix_program(assembly_ast, &function_callee_regs);
    if args.fixing {
        println!("{assembly_ast:#?}");
        return ExitCode::SUCCESS;
    }

    // -------------------------------------------------------------------------
    //                          Code Generation Pass
    // -------------------------------------------------------------------------

    generate_predefined(&mut assembly_ast);
    let formatted_assembly = AssemblyFormatter::format_program(&assembly_ast, asm_gen.str_literals);
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
    let output_file_path = format!("{file_name}.s");
    match std::fs::write(&output_file_path, formatted_assembly) {
        Ok(()) => {
            println!("Full: Successfully wrote to file {output_file_path}");
        }
        Err(e) => {
            eprintln!("Failed to write to file {output_file_path}: {e}");
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
