use std::collections::{HashMap, HashSet};

use derive_more::Display;
use util::{CFG, cfg::BasicBlock, cfg::NodeId};

use crate::wackir::{WackInstr, WackTempIdent, WackValue};

use WackInstr::{
    AddPtr, Alloc, ArrayAccess, Binary, Copy, CopyToOffset, Exit, FreeChecked, FreeUnchecked,
    FunCall, Jump, JumpIfNotZero, JumpIfZero, JumpToHandler, Label, Load, NullPtrGuard, Print,
    Println, Read, Return, Unary,
};

use super::cfg::{EmptyCFG, get_dst};
use util::cfg::reverse_postorder_block_ids;
// TODO: make this more efficient to reduce the clones on the reaching copies and instructions
pub fn copy_propagation(cfg: &EmptyCFG) -> EmptyCFG {
    let annotated_cfg = find_reaching_copies(cfg);

    // Rewrite each block in the annotated CFG with the reaching copies
    let mut transformed_cfg = annotated_cfg;

    // Rewrite instructions using the reaching copies information
    for block in transformed_cfg.basic_blocks.values_mut() {
        // Use the rewrite_instruction function on each instruction
        block.instructions = block
            .instructions
            .iter()
            .filter_map(|(reaching_copies, instr)| {
                let result = rewrite_instruction(reaching_copies, instr.clone());
                if result.is_some() {
                    Some((reaching_copies.clone(), result.unwrap()))
                } else {
                    None
                }
            })
            .collect();
    }

    // Strip annotations from the CFG
    transformed_cfg.strip_annotations()
}

// Struct representing a Reaching Copy
#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
#[display("{dst} = {src}")]
struct CP {
    src: WackValue,
    dst: WackValue,
}

impl CP {
    fn new(src: WackValue, dst: WackTempIdent) -> Self {
        Self {
            src,
            dst: WackValue::Var(dst),
        }
    }
}

type ReachingCopies = HashSet<CP>;
type ReachingCFG = CFG<WackInstr, ReachingCopies>;
type ReachingBasicBlock = BasicBlock<WackInstr, ReachingCopies>;

// pub type EmptyCFG = CFG<WackInstr, ()>;

/* ================== HELPER FUNCTIONS ===================== */

/// Transfer function takes all the Copy instructions that reach
/// the beginning of a basic block and calculates which copies reach each
/// individual instruction within block. Also goes to the end of the block
fn transfer(
    mut block: ReachingBasicBlock,
    initial_copies: ReachingCopies,
) -> BasicBlock<WackInstr, ReachingCopies> {
    let mut current_copies = initial_copies;

    // TODO: change the design to not use a tuple struct
    // as it's less clear without comments
    for instr in &mut block.instructions {
        // Annotate the instruction with the current copy
        instr.0 = current_copies.clone();

        // Update current copies based on the instruction
        //
        match instr.1 {
            Copy { ref src, ref dst } => {
                let cp = CP::new(src.clone(), dst.clone());
                if current_copies.contains(&cp) {
                    continue;
                }

                // if say x = y, we remove all copies that have x as a destination
                update_reaching_copies(&mut current_copies, dst);
                // Add the new copy
                current_copies.insert(cp);
            }
            _ => {
                // Remove all copies that have the destination of the instruction
                if let Some(dst) = get_dst(&instr.1) {
                    update_reaching_copies(&mut current_copies, dst);
                }
                // Since we don't have a store we don't have to handle this however
                // arrays and pairs maybe screwed due to aliasing somehow
            }
        }
    }

    // Annotation for the end of the block with the current copies
    block.value = current_copies;

    block
}

/// Meet operator propagates information about reaching copies from one block to the others
/// A copy reaches the beginning / meets a specific block if it reaches the end of
/// all the predecessors of that block
fn meet(
    block: &ReachingBasicBlock,
    all_copies: &ReachingCopies,
    cfg: &ReachingCFG,
) -> ReachingCopies {
    let mut incoming_copies = all_copies.clone();

    for pred_id in &block.preds {
        match *pred_id {
            NodeId::Entry => return HashSet::new(),
            NodeId::Block(id) => {
                let pred_out_copies = cfg
                    .get_block_value(id)
                    .expect("CFG is malformed or corrupted");
                incoming_copies.retain(|cp| pred_out_copies.contains(cp));
            }
            NodeId::Exit => panic!("Exit node should not be a predecessor"),
        }
    }

    incoming_copies
}

/// Update the reaching copies based on what's updated
fn update_reaching_copies(copies: &mut ReachingCopies, updated: &WackTempIdent) {
    // Remove all copies that have the updated variable as a destination
    let updated = WackValue::Var(updated.clone());
    copies.retain(|cp| cp.dst != updated);
    copies.retain(|cp| cp.src != updated);
}

/// Iterative algorithm to find all reaching copies within the CFG
fn find_all_copy_instructions(cfg: &EmptyCFG) -> ReachingCopies {
    let mut copies = HashSet::new();

    for block in cfg.basic_blocks.values() {
        for instr in &block.instructions {
            if let Copy { ref src, ref dst } = instr.1 {
                let cp = CP::new(src.clone(), dst.clone());
                copies.insert(cp);
            }
        }
    }

    copies
}

/// Iterative algorithm to find all reaching copies within the CFG
fn find_reaching_copies(cfg: &EmptyCFG) -> ReachingCFG {
    let all_copies = find_all_copy_instructions(cfg);
    let mut worklist = Vec::new();

    // Create a new ReachingCFG
    let mut reaching_cfg = ReachingCFG {
        basic_blocks: HashMap::new(),
        entry_succs: cfg.entry_succs.clone(),
        exit_preds: cfg.exit_preds.clone(),
        debug_label: cfg.debug_label.clone(),
    };

    // Initialize each block with all copies and add all blocks to the worklist
    for (&block_id, block) in &cfg.basic_blocks {
        // Create a new reaching block with initial annotations
        let reaching_block = BasicBlock {
            id: block.id,
            instructions: block
                .instructions
                .iter()
                .map(|(_, instr)| (all_copies.clone(), instr.clone()))
                .collect(),
            preds: block.preds.clone(),
            succs: block.succs.clone(),
            value: all_copies.clone(),
        };

        // Add the reaching block to the reaching CFG
        reaching_cfg.basic_blocks.insert(block_id, reaching_block);

        // Add block to worklist
        worklist.push(block_id);
    }

    // Sort blocks in reverse postorder to improve efficiency
    worklist = reverse_postorder_block_ids(&cfg, &worklist, true);

    // Process blocks until worklist is empty
    while let Some(block_id) = worklist.pop() {
        // Get the block and clone its current annotation
        let block = reaching_cfg.basic_blocks.get(&block_id).unwrap().clone();
        let old_annotation = block.value.clone();

        // Compute incoming copies using meet operator
        let incoming_copies = meet(&block, &all_copies, &reaching_cfg);

        // Apply transfer function and update the block in the CFG
        let updated_block = transfer(block, incoming_copies);
        reaching_cfg.basic_blocks.insert(block_id, updated_block);

        // If annotation changed, add successors to worklist
        let updated_block = reaching_cfg.basic_blocks.get(&block_id).unwrap();
        if old_annotation != updated_block.value {
            for &succ_id in &updated_block.succs {
                match succ_id {
                    NodeId::Exit => {}
                    NodeId::Entry => panic!("Malformed control-flow graph"),
                    NodeId::Block(id) => {
                        if !worklist.contains(&id) {
                            worklist.push(id);
                        }
                    }
                }
            }
        }
    }

    reaching_cfg
}

fn replace_operand(op: WackValue, reaching_copies: &ReachingCopies) -> WackValue {
    // if it's a constant we don't need to do anything
    if let WackValue::Literal(_) = op {
        return op;
    }

    for copy in reaching_copies {
        if copy.dst == op {
            return copy.src.clone();
        }
    }

    op
}

fn rewrite_instruction(reaching_copies: &ReachingCopies, instr: WackInstr) -> Option<WackInstr> {
    match instr {
        Copy { src, dst } => {
            for copy in reaching_copies {
                // if x = y already exists, we can remove the copy
                // if we have x = y, and we enounter y = x, we can remove the copy
                let dst = WackValue::Var(dst.clone());
                let same = copy.src == src && copy.dst == dst;
                let reverse = copy.src == dst && copy.dst == src;
                if same || reverse {
                    return None;
                }
            }
            let new_src = replace_operand(src, &reaching_copies);
            return Some(Copy { src: new_src, dst });
        }
        Unary { op, src, dst } => {
            let new_src = replace_operand(src, &reaching_copies);
            Some(Unary {
                op,
                src: new_src,
                dst,
            })
        }
        Binary {
            op,
            src1,
            src2,
            dst,
        } => {
            let new_src1 = replace_operand(src1, &reaching_copies);
            let new_src2 = replace_operand(src2, &reaching_copies);
            Some(Binary {
                op,
                src1: new_src1,
                src2: new_src2,
                dst,
            })
        }
        Return(value) => {
            let new_value = replace_operand(value, &reaching_copies);
            Some(Return(new_value))
        }
        JumpIfZero { condition, target } => {
            let new_condition = replace_operand(condition, &reaching_copies);
            Some(JumpIfZero {
                condition: new_condition,
                target,
            })
        }
        JumpIfNotZero { condition, target } => {
            let new_condition = replace_operand(condition, &reaching_copies);
            Some(JumpIfNotZero {
                condition: new_condition,
                target,
            })
        }
        FunCall {
            fun_name,
            args,
            dst,
        } => {
            let new_args = args
                .into_iter()
                .map(|arg| replace_operand(arg, &reaching_copies))
                .collect();
            Some(FunCall {
                fun_name,
                args: new_args,
                dst,
            })
        }
        Load { src_ptr, dst } => {
            let new_src_ptr = replace_operand(src_ptr, &reaching_copies);
            Some(Load {
                src_ptr: new_src_ptr,
                dst,
            })
        }
        AddPtr {
            src_ptr,
            index,
            scale,
            offset,
            dst_ptr,
        } => {
            let new_src_ptr = replace_operand(src_ptr, &reaching_copies);
            let new_index = replace_operand(index, &reaching_copies);
            Some(AddPtr {
                src_ptr: new_src_ptr,
                index: new_index,
                scale,
                offset,
                dst_ptr,
            })
        }
        ArrayAccess {
            src_array_ptr,
            index,
            scale,
            dst_elem_ptr,
        } => {
            let new_src_array_ptr = replace_operand(src_array_ptr, &reaching_copies);
            let new_index = replace_operand(index, &reaching_copies);
            Some(ArrayAccess {
                src_array_ptr: new_src_array_ptr,
                index: new_index,
                scale,
                dst_elem_ptr,
            })
        }
        CopyToOffset {
            src,
            dst_ptr,
            offset,
        } => {
            let new_src = replace_operand(src, &reaching_copies);
            Some(CopyToOffset {
                src: new_src,
                dst_ptr,
                offset,
            })
        }
        FreeUnchecked(value) => {
            let new_value = replace_operand(value, &reaching_copies);
            Some(FreeUnchecked(new_value))
        }
        FreeChecked(value) => {
            let new_value = replace_operand(value, &reaching_copies);
            Some(FreeChecked(new_value))
        }
        NullPtrGuard(value) => {
            let new_value = replace_operand(value, &reaching_copies);
            Some(NullPtrGuard(new_value))
        }
        Exit(value) => {
            let new_value = replace_operand(value, &reaching_copies);
            Some(Exit(new_value))
        }
        Print { src, ty } => {
            let new_src = replace_operand(src, &reaching_copies);
            Some(Print { src: new_src, ty })
        }
        Println { src, ty } => {
            let new_src = replace_operand(src, &reaching_copies);
            Some(Println { src: new_src, ty })
        }
        // Instructions that don't have operands to replace
        Jump(_) | Label(_) | Read { .. } | Alloc { .. } | JumpToHandler(_) => Some(instr),
    }
}
