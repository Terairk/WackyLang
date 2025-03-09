mod cfg;
mod constant_folding;
mod copy_prop;
mod dead_store_elim;
mod unreachable_code_elim;

use crate::wackir::{WackInstr, WackProgram};
use cfg::{cfg_to_instrs, make_cfg};
use constant_folding::constant_fold_function;
use copy_prop::copy_propagation;
use dead_store_elim::eliminate_dead_stores;
use std::mem;
use unreachable_code_elim::eliminate_unreachable_code;
use util::opt_flags::OptimizationConfig;

/// Optimizes an entire WackIR program.
#[inline]
pub fn optimize(program: WackProgram, config: OptimizationConfig) -> WackProgram {
    let mut optimized_program = program;

    for fun in optimized_program.functions.iter_mut() {
        // use mem::take to avoid cloning the function body
        let body = mem::take(&mut fun.body);
        fun.body = optimize_fun(body, config);
    }

    optimized_program
}

fn optimize_fun(mut function_body: Vec<WackInstr>, config: OptimizationConfig) -> Vec<WackInstr> {
    if function_body.is_empty() {
        return function_body;
    }

    // Iterate and keep applying optimizations until you reach a fixed point
    loop {
        // Store the current state for comparison later
        // use mem::take to avoid cloning the function body
        let current_body = mem::take(&mut function_body);

        let post_constant_folding = if config.has_fold_constants() {
            constant_fold_function(&current_body)
        } else {
            current_body.clone()
        };

        let mut cfg = make_cfg(post_constant_folding);

        if config.has_eliminate_unreachable_code() {
            cfg = eliminate_unreachable_code(cfg);
        }

        if config.has_copy_propagation() {
            cfg = copy_propagation(cfg);
        }

        if config.has_eliminate_dead_stores() {
            cfg = eliminate_dead_stores(cfg);
        }

        let optimized_fun_body = cfg_to_instrs(cfg);

        if optimized_fun_body == current_body || optimized_fun_body.is_empty() {
            return optimized_fun_body;
        }

        function_body = optimized_fun_body;
    }
}
