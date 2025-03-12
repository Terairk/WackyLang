use super::cfg::{EmptyCFG, get_dst, reverse_postorder_block_ids};

use std::collections::{HashMap, HashSet};

use derive_more::Display;
use util::{CFG, cfg::BasicBlock, cfg::NodeId};

use crate::wackir::{WackInstr, WackTempIdent, WackValue};
use WackInstr::{
    AddPtr, Alloc, ArrayAccess, Binary, Copy, CopyToOffset, Exit, FreeChecked, FreeUnchecked,
    FunCall, Jump, JumpIfNotZero, JumpIfZero, JumpToHandler, Label, Load, NullPtrGuard, Print,
    Println, Read, Return, Unary,
};

pub(crate) fn eliminate_dead_stores(cfg: &EmptyCFG) -> EmptyCFG {
    // Get annotated CFG with live variables
    let mut annotated_cfg = find_live_variables(cfg);

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
// Could also use cfg.initialize_annotations() but have to figure out worklist
fn find_live_variables(cfg: &EmptyCFG) -> LiveCFG {
    let mut worklist = Vec::new();

    // Create a new LiveCFG
    let mut live_cfg = LiveCFG {
        basic_blocks: HashMap::new(),
        entry_succs: cfg.entry_succs.clone(),
        exit_preds: cfg.exit_preds.clone(),
        debug_label: cfg.debug_label.clone(),
    };

    // Initialize live variables for each block
    for (&block_id, block) in &cfg.basic_blocks {
        // Create a new block with initial annotations
        let live_block = BasicBlock {
            id: block.id,
            instructions: block
                .instructions
                .iter()
                .map(|(_, instr)| (LiveVariables::new(), instr.clone()))
                .collect(),
            preds: block.preds.clone(),
            succs: block.succs.clone(),
            value: LiveVariables::new(),
        };

        // Add the block to the liveCFG
        live_cfg.basic_blocks.insert(block_id, live_block);

        // Add block to worklist
        worklist.push(block_id);
    }

    // Sort blocks in postorder to improve efficiency
    worklist = reverse_postorder_block_ids(&cfg, &worklist, false);

    // Iterate until the worklist is empty
    while let Some(block_id) = worklist.pop() {
        let block = live_cfg.basic_blocks.get(&block_id).unwrap().clone();
        let old_annotation = block.value.clone();

        // Compute incoming copies using meet operator
        // TODO: check if we do meet or transfer first
        let live_vars_at_exit = meet(&block, &live_cfg);

        // Apply transfer function and update the block in the CFG
        let new_block = transfer(block, live_vars_at_exit);
        live_cfg.update_basic_block(block_id, new_block);

        // If the live variables have changed, update the block and add predecessors to the worklist
        let updated_block = live_cfg.basic_blocks.get(&block_id).unwrap();
        if live_cfg.get_block_value(block_id).unwrap() != &old_annotation {
            for &pred_id in &updated_block.preds {
                match pred_id {
                    NodeId::Entry => {}
                    NodeId::Exit => panic!("Exit node should not be a predecessor"),
                    NodeId::Block(id) => {
                        if !worklist.contains(&id) {
                            worklist.push(id);
                        }
                    }
                }
            }
        }
    }

    live_cfg
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
            CopyToOffset { ref src, .. } => {
                add_var(src, &mut current_live_vars);
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
            Read { ref dst, .. } => {
                remove_var(dst, &mut current_live_vars);
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
    for succ_id in &block.succs {
        match *succ_id {
            NodeId::Entry => panic!("Entry node should not be a successor"),
            NodeId::Exit => {}
            NodeId::Block(id) => {
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
        FunCall { .. } => false,
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
