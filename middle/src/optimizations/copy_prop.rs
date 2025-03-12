use std::collections::HashSet;

use derive_more::Display;
use util::{CFG, cfg::BasicBlock};

use crate::wackir::{WackInstr, WackTempIdent, WackValue};
use WackInstr::{Binary, Copy, FunCall, Unary};

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

type ReachingCopies = HashSet<CP>;
type ReachingCFG = CFG<WackInstr, ReachingCopies>;

// pub type EmptyCFG = CFG<WackInstr, ()>;

/* ================== HELPER FUNCTIONS ===================== */

/// Transfer function takes all the Copy instructions that reach
/// the beginning of a basic block and calculates which copies reach each
/// individual instruction within block. Also goes to the end of the block
// fn transfer(
//     mut block: BasicBlock<WackInstr, ReachingCopies>,
//     initial_copies: ReachingCopies,
// ) -> BasicBlock<WackInstr, ReachingCopies> {
//     let mut current_copies = initial_copies;
//
//     // TODO: change the design to not use a tuple struct
//     // as it's less clear without comments
//     for mut instr in block.instructions {
//         // Annotate the instruction with the current copy
//         instr.0 = current_copies.clone();
//
//         // Update current copies based on the instruction
//         //
//         match instr.1 {
//             Copy { src, dst } => {
//                 let cp = CP {
//                     src,
//                     dst: WackValue::Var(dst),
//                 };
//                 if current_copies.contains(&cp) {
//                     continue;
//                 }
//
//                 // if say x = y, we remove all copies that have x as a destination
//                 let dst = WackValue::Var(dst);
//                 current_copies.retain(|cp| cp.dst != dst);
//                 current_copies.retain(|cp| cp.src != dst);
//             }
//             _ => panic!("Only Copy instructions should be in the reaching copies"),
//         }
//     }
//
//     block
// }

// Update the reaching copies based on what's updated
// This WackTempIdent
fn filter_updated(copies: &ReachingCopies, updated: &WackTempIdent) {
    todo!()
}

fn find_reaching_copies(cfg: EmptyCFG) -> ReachingCFG {
    todo!()
}
