use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    mem,
};

/*  General structure I'm using written in ASDL Syntax
*
*  node_id = ENTRY | EXIT | BlockId(int num)
*  node = BasicBlock(node_id id, instruction* instructions,
*                    node_id* predecessors, node_id* successors)
*  | EntryNode(node_id* successors)
*  | ExitNode(node_id* predecessors)
*  graph = Graph(node* nodes)
* */

/* Contains a generic Control Flow Graph to be used for
 * both Wacky and AssemblyIR. If you want we can change the structure
 * to be different ie a map from node_id to node instaed of a list of nodes
 * or even track the entry and exit nodes separately from those that represent
 * basic blocks */
// TODO: figure out how I handle my rogue jumps to error handlers here

use internment::ArcIntern;
// Location is either a Label or a Function
type Location = ArcIntern<str>;

use SimpleInstr::{ConditionalJump, ErrorJump, Label, Other, Return, UnconditionalJump};

/// A simplified instruction type that can represent both Wack and AssemblyIR
#[derive(Debug, Clone, PartialEq)]
pub enum SimpleInstr {
    Label(Location),
    ConditionalJump(Location),
    UnconditionalJump(Location),
    Return,
    ErrorJump, // We treat this simlarly to a return as it functionally acts the same
    Other,
}

/// Trait used for instructions that can be used in a CFG
pub trait Instruction: Sized + Clone {
    /// Convert instruction to a simplified form for CFG analysis
    fn simplify(&self) -> SimpleInstr;

    /// Format instruction for display
    fn format(&self, f: &mut Formatter<'_>) -> fmt::Result;
}

/// Node ID in the control flow graph
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeId {
    Entry,
    Block(usize),
    Exit,
}

/// A basic block in the control flow graph
// T = ?, V = ?
// one might be SimplifiedInstr, the other might be original Instruction
// We treat exit nodes and entry nodes as basic blocks but with no Instructions etc
#[derive(Debug, Clone)]
pub struct BasicBlock<T, V> {
    pub id: NodeId,
    pub instructions: Vec<(V, T)>,
    pub preds: Vec<NodeId>,
    pub succs: Vec<NodeId>,
    pub value: V,
}

/// Control flow graph
// NOTE: Notably we use Vec for entry_succs in particular
// in case we ever run into an edge case which requires it
// But afaik we should only have one entry successor
// Just makes the code more modular
#[derive(Debug, Clone)]
pub struct CFG<T, V> {
    pub basic_blocks: HashMap<usize, BasicBlock<T, V>>,
    pub entry_succs: Vec<NodeId>,
    pub exit_preds: Vec<NodeId>,
    pub debug_label: String,
}

impl<T: Instruction + Clone, V: Clone + Default> CFG<T, V> {
    /// Create a new CFG from a list of instructions
    // TODO: check if we can take ownership of the instructions
    pub fn from_instructions(debug_label: String, instructions: &[T]) -> CFG<T, ()> {
        let basic_blocks = Self::partition_into_basic_blocks(instructions);
        let mut cfg = CFG {
            basic_blocks,
            entry_succs: Vec::new(),
            exit_preds: Vec::new(),
            debug_label,
        };

        cfg.add_all_edges();
        cfg
    }

    // TODO: take ownership of the instructions maybe
    fn partition_into_basic_blocks(instructions: &[T]) -> HashMap<usize, BasicBlock<T, ()>> {
        let mut finished_blocks = Vec::new();
        let mut current_block = Vec::new();

        // You can't jump into/out of the middle of a basic block
        // Basically this implies that Label can appear only as the first instruction of a block
        // and the Return or Jump instruction can appear as the last instruction
        // Hence a label indicates the start of a new block and the end of the previous block
        // And a Return or Jump instruction indicates the end of the current block
        for instr in instructions {
            match instr.simplify() {
                Label(_) => {
                    if !current_block.is_empty() {
                        finished_blocks.push(mem::take(&mut current_block));
                    }
                    current_block.push(instr.clone());
                }
                ConditionalJump(_) | UnconditionalJump(_) | ErrorJump | Return => {
                    current_block.push(instr.clone());
                    finished_blocks.push(mem::take(&mut current_block));
                }
                Other => current_block.push(instr.clone()),
            }
        }

        if !current_block.is_empty() {
            finished_blocks.push(current_block);
        }

        // Create basic blocks with IDs
        let mut basic_blocks = HashMap::new();
        for (idx, instrs) in finished_blocks.into_iter().enumerate() {
            basic_blocks.insert(
                idx,
                BasicBlock {
                    id: NodeId::Block(idx),
                    instructions: instrs.into_iter().map(|instr| ((), instr)).collect(),
                    preds: Vec::new(),
                    succs: Vec::new(),
                    value: (),
                },
            );
        }

        basic_blocks
    }

    /// Add edges between basic blocks
    fn add_all_edges(&mut self) {
        // Build map from labels to the IDs of the blocks that they start with
        // TODO: may need to consider functions and not just labels
        // TODO: figure out the point of this label_map
        let mut label_map = HashMap::new();
        for (&idx, block) in &self.basic_blocks {
            if let Some((_, first_instr)) = block.instructions.first() {
                if let Label(label) = first_instr.simplify() {
                    label_map.insert(label, NodeId::Block(idx));
                }
            }
        }

        // Add edge from entry to the first block
        if !self.basic_blocks.is_empty() {
            self.add_edge(NodeId::Entry, NodeId::Block(0));
        }

        // Add outgoing edges from each basic block
        let block_ids: Vec<usize> = self.basic_blocks.keys().copied().collect();
        let max_id = block_ids.iter().max().copied().unwrap_or(0);

        for &id in &block_ids {
            let next_id = if id == max_id {
                NodeId::Exit
            } else {
                NodeId::Block(id + 1)
            };

            // Get the last instruction of the block
            if let None = self.basic_blocks.get(&id) {
                continue;
            }
            // Safety: We just checked that the block exists
            let block = self.basic_blocks.get(&id).unwrap();
            if let None = block.instructions.last() {
                continue;
            }
            // Safety: We just checked that the block is not empty
            let (_, last_instr) = block.instructions.last().unwrap();
            match last_instr.simplify() {
                Return | ErrorJump => {
                    // TODO: check if we handle ErrorJump is handled this way
                    // or if we should add information of where it jumps to
                    self.add_edge(NodeId::Block(id), NodeId::Exit);
                }
                UnconditionalJump(target) => {
                    if let Some(target_id) = label_map.get(&target) {
                        self.add_edge(NodeId::Block(id), target_id.clone());
                    }
                }
                ConditionalJump(target) => {
                    // Add edge to the target block ie the one if we fail the condition
                    self.add_edge(NodeId::Block(id), next_id);
                    if let Some(target_id) = label_map.get(&target) {
                        self.add_edge(NodeId::Block(id), target_id.clone());
                    }
                }
                _ => {
                    self.add_edge(NodeId::Block(id), next_id);
                }
            }
        }
    }

    /// Add an edge between two nodes
    pub fn add_edge(&mut self, pred: NodeId, succ: NodeId) {
        match pred {
            NodeId::Entry => {
                if !self.entry_succs.contains(&succ) {
                    self.entry_succs.push(succ);
                }
            }
            NodeId::Block(id) => {
                if let Some(block) = self.basic_blocks.get_mut(&id) {
                    if !block.succs.contains(&succ) {
                        block.succs.push(succ);
                    }
                }
            }
            NodeId::Exit => {}
        }

        match &succ {
            NodeId::Entry => {}
            NodeId::Block(id) => {
                if let Some(block) = self.basic_blocks.get_mut(&id) {
                    if !block.preds.contains(&pred) {
                        block.preds.push(pred);
                    }
                }
            }
            NodeId::Exit => {
                if !self.exit_preds.contains(&pred) {
                    self.exit_preds.push(pred);
                }
            }
        }
    }

    /// Removes an edge between two nodes
    pub fn remove_edge(&mut self, pred: NodeId, succ: NodeId) {
        match pred {
            NodeId::Entry => {
                // Uses vec which is O(n) to remove an element
                // Perhaps we can use a better data structure
                self.entry_succs.retain(|id| *id != succ);
            }
            NodeId::Block(id) => {
                if let Some(block) = self.basic_blocks.get_mut(&id) {
                    block.succs.retain(|id| *id != succ);
                }
            }
            NodeId::Exit => {}
        }

        match succ {
            NodeId::Entry => {}
            NodeId::Block(id) => {
                if let Some(block) = self.basic_blocks.get_mut(&id) {
                    block.preds.retain(|id| *id != pred);
                }
            }
            NodeId::Exit => {
                self.exit_preds.retain(|id| *id != pred);
            }
        }
    }

    /// Get the successors of a node
    #[must_use]
    #[inline]
    pub fn get_succs(&self, node_id: NodeId) -> Vec<NodeId> {
        match node_id {
            NodeId::Entry => self.entry_succs.clone(),
            NodeId::Block(id) => {
                if let Some(block) = self.basic_blocks.get(&id) {
                    block.succs.clone()
                } else {
                    Vec::new()
                }
            }
            NodeId::Exit => Vec::new(),
        }
    }
}

/* ===================== Impl's for Types ==================== */
impl Display for NodeId {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Entry => write!(f, "Entry"),
            Self::Block(id) => write!(f, "Block {id}"),
            Self::Exit => write!(f, "Exit"),
        }
    }
}
