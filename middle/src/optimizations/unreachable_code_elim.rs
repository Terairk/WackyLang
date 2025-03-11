use std::collections::HashSet;

use SimpleInstr::{ConditionalJump, UnconditionalJump};
use util::cfg::SimpleInstr;
use util::{CFG, Instruction, cfg::NodeId};

use super::cfg::EmptyCFG;
pub(crate) fn eliminate_unreachable_code(cfg: EmptyCFG) -> EmptyCFG {
    // Should call eliminate_unreachable_blocks
    // And eliminate_useless_labels
    // And remove_empty blocks
    todo!()
}

fn eliminate_unreachable_blocks(mut cfg: EmptyCFG) -> EmptyCFG {
    // DFS to find all reachable nodes
    fn dfs<T: Instruction, V: Clone + Default>(
        cfg: &CFG<T, V>,
        node_id: NodeId,
        reachable: &mut HashSet<NodeId>,
    ) {
        if reachable.contains(&node_id) {
            return;
        }

        reachable.insert(node_id);

        for succ in cfg.get_succs(node_id) {
            dfs(cfg, succ, reachable);
        }
    }

    let mut reachable_blocks = HashSet::new();
    dfs(&cfg, NodeId::Entry, &mut reachable_blocks);

    // Modify the CFG to remove unreachable blocks
    // First get block ids to remove
    let block_ids_to_remove: Vec<usize> = cfg
        .basic_blocks
        .keys()
        .filter(|id| !reachable_blocks.contains(&NodeId::Block(**id)))
        .copied()
        .collect();

    // Collect all edges to remove before modifying the CFG
    // We do this to satisfy the borrow checker
    let mut edges_to_remove = Vec::new();
    for id in &block_ids_to_remove {
        if let Some(block) = cfg.basic_blocks.get(id) {
            for pred in &block.preds {
                edges_to_remove.push((*pred, NodeId::Block(*id)));
            }

            for succ in &block.succs {
                edges_to_remove.push((NodeId::Block(*id), *succ));
            }
        }
    }

    // We can modify the CFG now
    for (from, to) in edges_to_remove {
        cfg.remove_edge(from, to);
    }

    // Remove the blocks after handling edges
    for id in block_ids_to_remove {
        cfg.basic_blocks.remove(&id);
    }

    cfg
}

// If the block has a single successor and all jump's target is the successor, then the jump is
// useless since it would fall through normally without the jump
fn eliminate_useless_jumps(mut cfg: EmptyCFG) -> EmptyCFG {
    let block_count = cfg.basic_blocks.len();

    // We need to sort the blocks to determine the natural successor
    // The sort order should match the original code order
    let mut block_ids: Vec<usize> = cfg.basic_blocks.keys().copied().collect();
    block_ids.sort_unstable();

    for (idx, &block_id) in block_ids.iter().enumerate() {
        if idx == block_count - 1 {
            // Don't modify the last block
            continue;
        }

        let block = &cfg.basic_blocks[&block_id];

        // Check if the last instruction is a jump
        if let Some((_, last_instr)) = block.instructions.last() {
            let simple_instr = last_instr.simplify();
            match simple_instr {
                UnconditionalJump(_) | ConditionalJump(_) => {
                    // Get the natural successor (next block in sequence)
                    // Not necessarily the id + 1 since we may have removed some blocks
                    // We can assume that the we can coerce this into a NodeId::Block
                    // since you can't jump straight to NodeId::Exit without a label
                    let default_succ = NodeId::Block(block_ids[idx + 1]);

                    // Check if jump is useless - only if all successors point to default_succ
                    let keep_jump = block.succs.iter().any(|succ| *succ != default_succ);

                    if !keep_jump {
                        // Remove the jump instruction
                        if let Some(block_mut) = cfg.basic_blocks.get_mut(&block_id) {
                            block_mut.instructions.pop();
                        }
                    }
                }
                _ => {}
            }
        }
    }

    cfg
}

fn eliminate_useless_labels(cfg: EmptyCFG) -> EmptyCFG {
    todo!()
}

fn remove_empty_blocks(cfg: EmptyCFG) -> EmptyCFG {
    todo!()
}
