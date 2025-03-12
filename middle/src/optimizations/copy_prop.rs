use std::collections::HashSet;

use derive_more::Display;
use util::{CFG, cfg::BasicBlock};

use crate::wackir::{WackInstr, WackTempIdent, WackValue};
use WackInstr::{
    AddPtr, Alloc, ArrayAccess, Binary, Copy, CopyToOffset, Exit, FreeChecked, FreeUnchecked,
    FunCall, Jump, JumpIfNotZero, JumpIfZero, JumpToHandler, Label, Load, NullPtrGuard, Print,
    Println, Read, Return, Unary,
};

use super::cfg::EmptyCFG;
pub fn copy_propagation(cfg: EmptyCFG) -> EmptyCFG {
    let annotated_cfg = find_reaching_copies(cfg);
    // Rewrite each block in the annotated CFG with the reaching copies

    // Strip annotations from the CFG
    unimplemented!()
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

// pub type EmptyCFG = CFG<WackInstr, ()>;

/* ================== HELPER FUNCTIONS ===================== */

/// Transfer function takes all the Copy instructions that reach
/// the beginning of a basic block and calculates which copies reach each
/// individual instruction within block. Also goes to the end of the block
fn transfer(
    mut block: BasicBlock<WackInstr, ReachingCopies>,
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
            FunCall { ref dst, .. } => {
                // Remove all copies that have the destination as a destination
                update_reaching_copies(&mut current_copies, dst);
            }
            Unary { ref dst, .. } => {
                // Remove all copies that have the destination as a destination
                update_reaching_copies(&mut current_copies, dst);
            }
            Binary { ref dst, .. } => {
                // Remove all copies that have the destination as a destination
                update_reaching_copies(&mut current_copies, dst);
            }

            _ => panic!("Only Copy instructions should be in the reaching copies"),
        }
    }

    block
}

// Update the reaching copies based on what's updated
fn update_reaching_copies(copies: &mut ReachingCopies, updated: &WackTempIdent) {
    // Remove all copies that have the updated variable as a destination
    let updated = WackValue::Var(updated.clone());
    copies.retain(|cp| cp.dst != updated);
    copies.retain(|cp| cp.src != updated);
}

fn find_reaching_copies(cfg: EmptyCFG) -> ReachingCFG {
    todo!()
}
//
// fn get_dst(instr: &WackInstr) -> Option<&WackTempIdent> {
//     match *instr {
//         Return(_) => None,
//         Unary { ref dst, .. }
//         | Binary { ref dst, .. }
//         | FunCall { ref dst, .. }
//         | Copy { ref dst, .. }
//         | Load { ref dst, .. } => Some(dst),
//         AddPtr { ref dst_ptr, .. } | CopyToOffset { ref dst_ptr, .. } => Some(dst_ptr),
//         _ => None,
//     }
// }
