use std::collections::HashSet;

use SimpleInstr::{ConditionalJump, Label, UnconditionalJump};
use util::cfg::SimpleInstr;
use util::{CFG, Instruction, cfg::NodeId};

use super::cfg::EmptyCFG;
#[must_use]
#[inline]
pub fn eliminate_unreachable_code(mut cfg: EmptyCFG) -> EmptyCFG {
    // println!("Eliminating unreachable code");
    cfg = eliminate_unreachable_blocks(cfg);
    cfg = eliminate_useless_jumps(cfg);
    cfg = eliminate_useless_labels(cfg);
    cfg = remove_empty_blocks(cfg);

    cfg
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

// NOTE: that removing useless labels for the first basic block is only safe
// due to removing useless jumps previously
fn eliminate_useless_labels(mut cfg: EmptyCFG) -> EmptyCFG {
    // Similar to eliminate_useless_jumps, we need to sort the blocks
    let mut block_ids: Vec<usize> = cfg.basic_blocks.keys().copied().collect();
    block_ids.sort_unstable();

    for (idx, &block_id) in block_ids.iter().enumerate() {
        let block = &cfg.basic_blocks[&block_id];

        // Check if the first instruction is a label
        if !block.instructions.is_empty() {
            if let Label(_) = block.instructions[0].1.simplify() {
                // Get the natural predecessor (previous block in sequence)
                let default_pred = if idx == 0 {
                    NodeId::Entry
                } else {
                    NodeId::Block(block_ids[idx - 1])
                };

                // Check if label is useless - only if it has exactly one predecessor
                // and that predecessor is the default pred
                if block.preds.len() == 1 && block.preds[0] == default_pred {
                    if let Some(block_mut) = cfg.basic_blocks.get_mut(&block_id) {
                        block_mut.instructions.remove(0);
                    }
                }
            }
        }
    }

    cfg
}

fn remove_empty_blocks(mut cfg: EmptyCFG) -> EmptyCFG {
    // We'll keep processing until no more blocks can be removed
    let mut changed = true;

    while changed {
        changed = false;
        let block_ids: Vec<usize> = cfg.basic_blocks.keys().copied().collect();

        for block_id in block_ids {
            // Skip if block was already removed
            if !cfg.basic_blocks.contains_key(&block_id) {
                continue;
            }

            // Extract all the data we need from the block first
            let is_empty;
            let succ;
            let preds;
            {
                let block = &cfg.basic_blocks[&block_id];
                is_empty = block.instructions.is_empty();

                if !is_empty || block.succs.len() != 1 {
                    continue;
                }

                succ = block.succs[0];
                preds = block.preds.clone();
            } // block reference ends here

            // Make sure we're not removing the entry block
            if !preds.is_empty() {
                // For each predecessor, add an edge to the successor
                for &pred in &preds {
                    // println!("Removing edge from {} to {}", pred, block_id);
                    cfg.remove_edge(pred, NodeId::Block(block_id));

                    // Only add new edge if successor still exists
                    if cfg.contains_node(succ) {
                        // println!("Adding edge from {} to {}", pred, succ);
                        cfg.add_edge(pred, succ);
                    }
                }

                // Remove edge from this block to successor
                // println!("Removing edge from {} to {}", block_id, succ);
                cfg.remove_edge(NodeId::Block(block_id), succ);

                // Remove the block
                // println!("Removing block {}", block_id);
                cfg.basic_blocks.remove(&block_id);

                changed = true;
                // Continue to next iteration since we modified the graph
                break;
            }
        }
    }

    cfg
}
