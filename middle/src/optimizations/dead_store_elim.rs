use super::cfg::{EmptyCFG, get_dst};

use std::collections::HashSet;

use util::cfg::backwards_dataflow_analysis;
use util::{CFG, cfg::BasicBlock, cfg::NodeId};

use crate::wackir::{WackInstr, WackTempIdent, WackValue};
use WackInstr::{
    AddPtr, Alloc, ArrayAccess, Binary, Copy, CopyToOffset, Exit, FreeChecked, FreeUnchecked,
    FunCall, Jump, JumpIfNotZero, JumpIfZero, JumpToHandler, Label, Load, NullPtrGuard, Print,
    Println, Read, Return, Unary,
};

// static mut CFG_COUNT: usize = 0;

pub(crate) fn eliminate_dead_stores(cfg: &EmptyCFG) -> EmptyCFG {
    // Get annotated CFG with live variables
    let mut annotated_cfg = find_live_variables(cfg);

    // Useful for deugging purposes and visualizing the CFG with annotations
    // unsafe {
    //     if let Ok(png_path) = annotated_cfg.print_graphviz(&mut CFG_COUNT) {
    //         println!("Generated CFG deadstore visualization: {}", png_path);
    //     }
    // }
    // println!("{:?}", annotated_cfg);
    // println!("===================================================");

    // Create new CFG with dead stores removed
    for block in annotated_cfg.basic_blocks.values_mut() {
        block.instructions = block
            .instructions
            .iter()
            .filter_map(|(live_vars, instr)| {
                if is_dead_store(live_vars, instr) {
                    None
                } else {
                    Some((live_vars.clone(), instr.clone()))
                }
            })
            .collect();
    }

    // Strip annotations

    annotated_cfg.strip_annotations()
}

type LiveVariables = HashSet<WackTempIdent>;
type LiveCFG = CFG<WackInstr, LiveVariables>;
type LiveBasicBlock = BasicBlock<WackInstr, LiveVariables>;

/// Iterative algorithm using backwards dataflow analysis
// TODO: make this take ownership to reduce cloning
// Still have to make new CFG but it'll use more moves
/// Finds live variables using backwards dataflow analysis
fn find_live_variables(cfg: &EmptyCFG) -> LiveCFG {
    // Create a default LiveVariables to use for initialization
    let default_live_vars = LiveVariables::new();

    // Initialize the LiveCFG with default annotations
    let initial_cfg = cfg.clone().initialize_annotation(&default_live_vars);

    // Perform the backwards dataflow analysis
    backwards_dataflow_analysis(&initial_cfg, meet, transfer)
}

/// Transfer function for live variable analysis
///
/// Takes a basic block and the live variables at the end of the block,
/// and computes the live variables at each instruction and at the beginning of the block
/// Basically kills any destinations and adds any sources
fn transfer(mut block: LiveBasicBlock, end_live_variables: LiveVariables) -> LiveBasicBlock {
    fn remove_var(var: &WackTempIdent, var_set: &mut LiveVariables) {
        var_set.remove(var);
    }

    fn add_var(var: &WackValue, var_set: &mut LiveVariables) {
        if let WackValue::Var(ref v) = *var {
            var_set.insert(v.clone());
        }
    }

    fn add_vars(used_vals: &[WackValue], var_set: &mut LiveVariables) {
        for val in used_vals {
            add_var(val, var_set);
        }
    }

    let mut current_live_vars = end_live_variables;

    // Process instructions in reverse order
    for instr in block.instructions.iter_mut().rev() {
        // Annotate the instructions with the current live variables
        instr.0 = current_live_vars.clone();

        // Update live variables based on the instruction
        match instr.1 {
            Binary {
                ref dst,
                ref src1,
                ref src2,
                ..
            } => {
                remove_var(dst, &mut current_live_vars);
                add_var(src1, &mut current_live_vars);
                add_var(src2, &mut current_live_vars);
            }
            Unary {
                ref dst, ref src, ..
            } => {
                remove_var(dst, &mut current_live_vars);
                add_var(src, &mut current_live_vars);
            }
            JumpIfZero { ref condition, .. } => {
                add_var(condition, &mut current_live_vars);
            }
            JumpIfNotZero { ref condition, .. } => {
                add_var(condition, &mut current_live_vars);
            }
            Copy { ref dst, ref src } => {
                remove_var(dst, &mut current_live_vars);
                add_var(src, &mut current_live_vars);
            }
            Return(ref v) => {
                add_var(v, &mut current_live_vars);
            }
            FunCall {
                ref dst, ref args, ..
            } => {
                remove_var(dst, &mut current_live_vars);
                add_vars(args, &mut current_live_vars);
            }
            Load {
                ref dst,
                ref src_ptr,
            } => {
                remove_var(dst, &mut current_live_vars);
                add_var(src_ptr, &mut current_live_vars);
            }
            AddPtr {
                ref src_ptr,
                ref index,
                ref dst_ptr,
                ..
            } => {
                remove_var(dst_ptr, &mut current_live_vars);
                add_var(src_ptr, &mut current_live_vars);
                add_var(index, &mut current_live_vars);
            } // Other cases omitted for brevity but would follow the same pattern
            // Jump, Label, Return None would have no effect
            CopyToOffset {
                ref src,
                ref dst_ptr,
                ..
            } => {
                add_var(src, &mut current_live_vars);
                let dst = WackValue::Var(dst_ptr.clone());
                add_var(&dst, &mut current_live_vars);
            }
            ArrayAccess {
                ref dst_elem_ptr,
                ref src_array_ptr,
                ref index,
                ..
            } => {
                remove_var(dst_elem_ptr, &mut current_live_vars);
                add_var(src_array_ptr, &mut current_live_vars);
                add_var(index, &mut current_live_vars);
            }
            Read { .. } => {
                // The destination remains live because the read might fail
            }
            Alloc { ref dst_ptr, .. } => {
                remove_var(dst_ptr, &mut current_live_vars);
            }
            FreeUnchecked(ref ptr) | FreeChecked(ref ptr) | NullPtrGuard(ref ptr) => {
                add_var(ptr, &mut current_live_vars);
            }
            Exit(ref v) => {
                add_var(v, &mut current_live_vars);
            }
            Print { ref src, .. } | Println { ref src, .. } => {
                add_var(src, &mut current_live_vars);
            }
            Jump(_) | Label(_) | JumpToHandler(_) => {}
        }
    }

    // Annotate the block with the current live variables
    block.value = current_live_vars.clone();
    block
}

/// The meet operator calculates which variables are live at the end of a basic block.
/// It is the union of the live variables at the beginning of each successor block.
fn meet(block: &LiveBasicBlock, cfg: &LiveCFG) -> LiveVariables {
    let mut live_vars = LiveVariables::new();
    // println!("{:?}", cfg);
    for succ_id in &block.succs {
        match *succ_id {
            NodeId::Entry => panic!("Entry node should not be a successor"),
            NodeId::Exit => {}
            NodeId::Block(id) => {
                // println!("{:?}", id);
                let succ_live_vars = cfg
                    .get_block_value(id)
                    .expect("CFG is malformed or corrupted")
                    .clone();
                live_vars = live_vars.union(&succ_live_vars).cloned().collect();
            }
        }
    }
    live_vars
}

fn is_dead_store(live_vars: &LiveVariables, instr: &WackInstr) -> bool {
    match *instr {
        FunCall { .. } | CopyToOffset { .. } => false,
        _ => {
            // Check if instr has a destination and if it is not live
            if let Some(dst) = get_dst(instr) {
                !live_vars.contains(dst)
            } else {
                false
            }
        }
    }
}
