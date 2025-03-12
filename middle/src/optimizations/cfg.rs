use std::collections::HashSet;

use internment::ArcIntern;
use util::{
    CFG, Instruction, SimpleInstr,
    cfg::{Location, NodeId},
};
// temporary bCFG cus I want to leave everything the same

use crate::wackir::{WackInstr, WackTempIdent};

// TODO: Change the CFG to the proper CFG type
// I'll figure this out as I go along

pub type EmptyCFG = CFG<WackInstr, ()>;

#[must_use]
#[inline]
pub fn make_cfg(instrs: Vec<WackInstr>, func_name: &str) -> EmptyCFG {
    CFG::<WackInstr, ()>::from_instructions(func_name.to_owned(), instrs)
}

#[must_use]
#[inline]
pub fn cfg_to_instrs(cfg: EmptyCFG) -> Vec<WackInstr> {
    CFG::to_instructions(cfg)
}

// Implementation of the Instruction trait for WackInstr
// TODO: as said earlier, see if we can change things to take ownership
// so we don't have to clone as much
impl Instruction for WackInstr {
    #[inline]
    fn simplify(&self) -> SimpleInstr {
        use WackInstr::{Jump, JumpIfNotZero, JumpIfZero, JumpToHandler, Label, Return};
        match *self {
            Label(ref name) => SimpleInstr::Label(name.clone().into()),
            Jump(ref name) => SimpleInstr::UnconditionalJump(name.clone().into()),
            JumpIfZero { ref target, .. } | JumpIfNotZero { ref target, .. } => {
                SimpleInstr::ConditionalJump(target.clone().into())
            }
            JumpToHandler(_) => SimpleInstr::ErrorJump,
            Return(..) => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }
}

impl From<WackTempIdent> for Location {
    #[inline]
    fn from(ident: WackTempIdent) -> Self {
        let interned_string: ArcIntern<str> = ident.clone().into();
        let id = ident.get_id();
        Self::new(interned_string, id)
    }
}

/// Helper function to sort blocks in reverse postorder (returns block IDs only)
pub fn reverse_postorder_block_ids(
    cfg: &EmptyCFG,
    blocks: &[usize],
    is_reversed: bool,
) -> Vec<usize> {
    // First get postorder of NodeIds
    let mut visited = HashSet::new();
    let mut postorder = Vec::new();

    // Start DFS from entry node
    visit_dfs(NodeId::Entry, cfg, &mut visited, &mut postorder);

    // Reverse the postorder
    if is_reversed {
        postorder.reverse();
    }

    // Filter out Entry and Exit nodes, keep only Block nodes that were in the original list
    // and extract the block IDs
    postorder
        .into_iter()
        .filter_map(|id| {
            if let NodeId::Block(block_id) = id {
                if blocks.contains(&block_id) {
                    return Some(block_id);
                }
            }
            None
        })
        .collect()
}

/// Helper function for DFS traversal
pub fn visit_dfs(
    node: NodeId,
    cfg: &EmptyCFG,
    visited: &mut HashSet<NodeId>,
    postorder: &mut Vec<NodeId>,
) {
    if visited.contains(&node) {
        return;
    }

    visited.insert(node);

    // Get successor nodes
    let successors = match node {
        NodeId::Entry => &cfg.entry_succs,
        NodeId::Block(id) => {
            if let Some(block) = cfg.basic_blocks.get(&id) {
                &block.succs
            } else {
                return;
            }
        }
        NodeId::Exit => return, // Exit has no successors,
    };

    // Visit each successor
    for &succ in successors {
        if !visited.contains(&succ) {
            visit_dfs(succ, cfg, visited, postorder);
        }
    }

    // Add node to postorder after visiting all its successors
    postorder.push(node);
}
