mod cfg;
mod constant_folding;
mod copy_prop;
mod dead_store_elim;
mod unreachable_code_elim;

use crate::wackir::{WackInstr, WackProgram};
use constant_folding::constant_fold_function;
use copy_prop::copy_propagation;
use dead_store_elim::eliminate_dead_stores;
use std::mem;
use util::opt_flags::OptimizationConfig;

// Public Re-exports
pub use cfg::{cfg_to_instrs, make_cfg};
pub use unreachable_code_elim::eliminate_unreachable_code;

/// Optimizes an entire WackIR program.
#[must_use]
#[inline]
pub fn optimize(program: WackProgram, config: OptimizationConfig) -> WackProgram {
    let mut optimized_program = program;

    // optimize main body
    optimized_program.main_body = optimize_fun(optimized_program.main_body, "main", config);

    // optimize user functions
    for fun in optimized_program.functions.iter_mut() {
        // use mem::take to avoid cloning the function body
        let body = mem::take(&mut fun.body);
        fun.body = optimize_fun(body, (&fun.name).into(), config);
    }

    optimized_program
}

fn optimize_fun(
    mut function_body: Vec<WackInstr>,
    func_name: &str,
    config: OptimizationConfig,
) -> Vec<WackInstr> {
    if function_body.is_empty() {
        return function_body;
    }

    // Used for debugging purposes, mutable so we only print the CFG once before any optimizations
    // We leave the commented print statements in the code for future debugging purposes
    let mut is_first = config.should_print_cfg();
    let mut counter = 0;

    // Iterate and keep applying optimizations until you reach a fixed point
    loop {
        // Store the current state for comparison later
        let current_body = function_body.clone();

        if is_first {
            let cfg = make_cfg(current_body.clone(), func_name);
            if let Ok(png_path) = cfg.print_graphviz(&mut counter) {
                println!("Generated CFG visualization Wack: {}", png_path);
            }
        }

        let post_constant_folding = if config.has_fold_constants() {
            constant_fold_function(current_body)
        } else {
            current_body
        };

        let mut cfg = make_cfg(post_constant_folding, func_name);

        if config.has_eliminate_unreachable_code() {
            cfg = eliminate_unreachable_code(cfg);
        }

        // if config.should_print_cfg() {
        //     let new_name = format!("{}_unreachable_{}", func_name, counter);
        //     let cfg = make_cfg(cfg_to_instrs(cfg.clone()), new_name.as_str());
        //     if let Ok(png_path) = cfg.print_graphviz() {
        //         println!("Generated CFG visualization: {}", png_path);
        //     }
        // }

        if config.has_copy_propagation() {
            cfg = copy_propagation(&cfg);
        }

        // if config.should_print_cfg() {
        //     let new_name = format!("{}_copy_prop_{}", func_name, counter);
        //     let cfg = make_cfg(cfg_to_instrs(cfg.clone()), new_name.as_str());
        //     if let Ok(png_path) = cfg.print_graphviz() {
        //         println!("Generated CFG visualization: {}", png_path);
        //     }
        // }
        if config.has_eliminate_dead_stores() {
            cfg = eliminate_dead_stores(&cfg);
        }
        // if config.should_print_cfg() {
        //     let new_name = format!("{}_deadstores_{}", func_name, counter);
        //     let cfg = make_cfg(cfg_to_instrs(cfg.clone()), new_name.as_str());
        //     if let Ok(png_path) = cfg.print_graphviz() {
        //         println!("Generated CFG visualization: {}", png_path);
        //     }
        //     counter += 1;
        // }

        let optimized_fun_body = cfg_to_instrs(cfg);

        // Check if fixed point is reached
        if optimized_fun_body == function_body || optimized_fun_body.is_empty() {
            // Print the final CFG
            if config.should_print_cfg() {
                let new_name = format!("{}_optimized", func_name);
                let cfg = make_cfg(function_body.clone(), new_name.as_str());
                // println!("cfg: {:?}", cfg);
                if let Ok(png_path) = cfg.print_graphviz(&mut counter) {
                    println!("Generated CFG visualization: {}", png_path);
                }
            }
            return optimized_fun_body;
        }

        counter += 1;
        function_body = optimized_fun_body;
        is_first = false;
    }
}
