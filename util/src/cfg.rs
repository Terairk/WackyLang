use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Display, Formatter},
    fs::File,
    io::Write,
    mem,
    process::Command,
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

use derive_more::Display;
// Location is either a Label or a Function
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location((InternStr, usize));

impl Location {
    #[must_use]
    #[inline]
    pub const fn new(name: InternStr, id: usize) -> Self {
        Self((name, id))
    }
}

use crate::alias::InternStr;
use SimpleInstr::{ConditionalJump, ErrorJump, Label, Other, Return, UnconditionalJump};

/// A simplified instruction type that can represent both Wack and AssemblyIR
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleInstr {
    Label(Location),
    ConditionalJump(Location),
    UnconditionalJump(Location),
    Return,
    ErrorJump, // We treat this simlarly to a return as it functionally acts the same
    Other,
}

/// Trait used for instructions that can be used in a CFG
pub trait Instruction: Sized + Clone + Debug {
    /// Convert instruction to a simplified form for CFG analysis
    fn simplify(&self) -> SimpleInstr;

    /// Format instruction for display
    #[inline]
    fn format(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

/// Node ID in the control flow graph
/// NOTE: don't change the display on these unless you want to change the graphviz function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub enum NodeId {
    #[display("entry")]
    Entry,
    #[display("block{_0}")]
    Block(usize),
    #[display("exit")]
    Exit,
}

/// A basic block in the control flow graph
// T = Instruction, V = value we need to store for passes
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
    #[must_use]
    #[inline]
    pub fn from_instructions(debug_label: String, instructions: Vec<T>) -> CFG<T, ()> {
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
    fn partition_into_basic_blocks(instructions: Vec<T>) -> HashMap<usize, BasicBlock<T, ()>> {
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
                    current_block.push(instr);
                }
                ConditionalJump(_) | UnconditionalJump(_) | ErrorJump | Return => {
                    current_block.push(instr);
                    finished_blocks.push(mem::take(&mut current_block));
                }
                Other => current_block.push(instr),
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
            if !self.basic_blocks.contains_key(&id) {
                continue;
            }
            // We just checked that the block exists so shouldn't panic
            let block = &self.basic_blocks[&id];
            if block.instructions.last().is_none() {
                continue;
            }
            // We just checked that the block is not empty
            let (_, ref last_instr) = *block.instructions.last().unwrap();
            match last_instr.simplify() {
                Return | ErrorJump => {
                    // TODO: check if we handle ErrorJump is handled this way
                    // or if we should add information of where it jumps to
                    self.add_edge(NodeId::Block(id), NodeId::Exit);
                }
                UnconditionalJump(target) => {
                    if let Some(target_id) = label_map.get(&target) {
                        self.add_edge(NodeId::Block(id), *target_id);
                    }
                }
                ConditionalJump(target) => {
                    // Add edge to the target block ie the one if we fail the condition
                    self.add_edge(NodeId::Block(id), next_id);
                    if let Some(target_id) = label_map.get(&target) {
                        self.add_edge(NodeId::Block(id), *target_id);
                    }
                }
                _ => {
                    self.add_edge(NodeId::Block(id), next_id);
                }
            }
        }
    }

    /// Add an edge between two nodes
    #[inline]
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

        match succ {
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
    #[inline]
    pub fn remove_edge(&mut self, pred: NodeId, succ: NodeId) {
        match pred {
            NodeId::Entry => {
                // Uses vec which is O(n) to remove an element
                // Perhaps we can use a better data structure
                self.entry_succs.retain(|node_id| *node_id != succ);
            }
            NodeId::Block(id) => {
                if let Some(block) = self.basic_blocks.get_mut(&id) {
                    block.succs.retain(|node_id| *node_id != succ);
                }
            }
            NodeId::Exit => {}
        }

        match succ {
            NodeId::Entry => {}
            NodeId::Block(id) => {
                if let Some(block) = self.basic_blocks.get_mut(&id) {
                    block.preds.retain(|node_id| *node_id != pred);
                }
            }
            NodeId::Exit => {
                self.exit_preds.retain(|node_id| *node_id != pred);
            }
        }
    }

    /// Get the successors of a node
    #[must_use]
    #[inline]
    pub fn get_succs(&self, node_id: NodeId) -> Vec<NodeId> {
        match node_id {
            NodeId::Entry => self.entry_succs.clone(),
            NodeId::Block(id) => self
                .basic_blocks
                .get(&id)
                .map_or_else(Vec::new, |block| block.succs.clone()),
            NodeId::Exit => Vec::new(),
        }
    }

    /// Get the predecessors of a node
    #[must_use]
    #[inline]
    pub fn get_preds(&self, node_id: NodeId) -> Vec<NodeId> {
        match node_id {
            NodeId::Entry => Vec::new(),
            NodeId::Block(id) => self
                .basic_blocks
                .get(&id)
                .map_or_else(Vec::new, |block| block.preds.clone()),
            NodeId::Exit => self.exit_preds.clone(),
        }
    }

    /// Get the value of a basic block
    #[must_use]
    #[inline]
    pub fn get_block_value(&self, block_id: usize) -> Option<&V> {
        self.basic_blocks.get(&block_id).map(|block| &block.value)
    }

    /// Update a basic block
    #[inline]
    pub fn update_basic_block(&mut self, block_id: usize, new_block: BasicBlock<T, V>) {
        self.basic_blocks.insert(block_id, new_block);
    }

    /// Convert back to a list of instructions
    #[must_use]
    #[inline]
    pub fn to_instructions(self) -> Vec<T> {
        // Pre-allocate capacity for efficiency
        let total_instructions = self
            .basic_blocks
            .values()
            .map(|block| block.instructions.len())
            .sum();
        let mut result = Vec::with_capacity(total_instructions);

        // Sort the basic blocks by ID
        let mut ids: Vec<usize> = self.basic_blocks.keys().copied().collect();
        ids.sort_unstable();

        // Consume self by moving basic blocks out
        let mut basic_blocks = self.basic_blocks;

        // Process blocks in order
        for id in ids {
            if let Some(block) = basic_blocks.get_mut(&id) {
                // We use mem::take to avoid cloning the instructions
                let block_instrs = mem::take(&mut block.instructions);
                for (_, instr) in block_instrs {
                    result.push(instr);
                }
            }
        }

        result
    }

    #[must_use]
    #[inline]
    pub fn contains_node(&self, node_id: NodeId) -> bool {
        match node_id {
            NodeId::Entry => true,
            NodeId::Block(id) => self.basic_blocks.contains_key(&id),
            NodeId::Exit => true,
        }
    }

    /// Initialize annotations with a default value
    #[must_use]
    #[inline]
    pub fn initialize_annotation<W: Clone + Default>(self, dummy_val: &W) -> CFG<T, W> {
        let mut new_blocks = HashMap::with_capacity(self.basic_blocks.len());

        for (idx, block) in self.basic_blocks {
            // Pre-allocate the vector with exact size needed
            let mut new_instructions = Vec::with_capacity(block.instructions.len());

            // Move instructions into the new vector with new annotations
            for (_, instruction) in block.instructions {
                new_instructions.push((dummy_val.clone(), instruction));
            }

            new_blocks.insert(
                idx,
                BasicBlock {
                    id: block.id,
                    instructions: new_instructions,
                    preds: block.preds,
                    succs: block.succs,
                    value: dummy_val.clone(),
                },
            );
        }

        CFG {
            basic_blocks: new_blocks,
            entry_succs: self.entry_succs,
            exit_preds: self.exit_preds,
            debug_label: self.debug_label,
        }
    }

    /// Strip annotations
    #[must_use]
    #[inline]
    pub fn strip_annotations(self) -> CFG<T, ()> {
        self.initialize_annotation(&())
    }
}

// Display for Instructions cus nicer formatting, Debug for V cus () doesn't implement Display but
// implements Debug
impl<T: Instruction + Clone + Display, V: Clone + Default + Debug> CFG<T, V> {
    /// Generate a GraphViz DOT file and render it as PNG
    // TODO: check if this works later on
    #[must_use]
    #[inline]
    #[allow(static_mut_refs)]
    pub fn print_graphviz(&self, counter: &mut usize) -> Result<String, std::io::Error> {
        let filename = format!("{}_{counter}.dot", self.debug_label.replace(" ", "_"));
        *counter += 1;
        let mut file = File::create(&filename)?;

        writeln!(file, "digraph {{")?;
        writeln!(file, "  labeljust=l")?;
        writeln!(file, "  node[shape=\"box\"]")?;
        writeln!(file, "  entry[label=\"ENTRY\"]")?;
        writeln!(file, "  exit[label=\"EXIT\"]")?;

        // Write blocks
        for (&id, block) in &self.basic_blocks {
            writeln!(file, "  block{id}[label=<")?;
            writeln!(file, "    <table>")?;
            writeln!(
                file,
                "      <tr><td colspan=\"2\"><b>{}</b></td></tr>",
                block.id
            )?;

            // Write instructions
            // honestly: I have no idea how to fix this clippy lint
            // if i do &(ref, ref) - i get another clippy lint
            #[allow(clippy::pattern_type_mismatch)]
            for (val, instr) in &block.instructions {
                write!(file, "      <tr><td align=\"left\">")?;
                let instr_buf = format!("{instr}");
                // Escape HTML characters
                let escaped = instr_buf.replace("<", "&lt;").replace(">", "&gt;");
                write!(file, "{escaped}")?;
                write!(file, "</td><td align=\"left\">")?;

                // Format the value
                let val_buf = format!("{val:?}");

                // Don't use escaped_val if val_buf is "()" (empty)
                let display_val = if val_buf == "()" {
                    "{}".to_string()
                } else {
                    // Keep the representation but properly escape for HTML
                    val_buf.replace("<", "&lt;").replace(">", "&gt;")
                };

                write!(file, "{display_val}")?;
                // // Format the value
                // let val_buf = format!("{val:?}");
                // // Debug print the value for value since it maybe () which doesn't implement
                // // Display
                // println!("{val:?}");
                // println!("{val_buf}");
                // write!(file, "{val_buf}")?;
                // write!(file, "5")?;
                writeln!(file, "</td></tr>")?;
            }

            // Write block value
            write!(file, "      <tr><td colspan=\"2\">")?;
            let val_buf = format!("{:?}", block.value);
            write!(file, "{val_buf}")?;
            writeln!(file, "</td></tr>")?;

            writeln!(file, "    </table>>]")?;
        }

        // Write edges from entry
        for succ in &self.entry_succs {
            writeln!(file, "  entry -> {succ}")?;
        }

        // Write edges between blocks
        for (&id, block) in &self.basic_blocks {
            for succ in &block.succs {
                writeln!(file, "  block{id} -> {succ}")?;
            }
        }

        writeln!(file, "}}")?;

        // Generate PNG
        let png_filename = filename.replace(".dot", ".png");
        let status = Command::new("dot")
            .args(["-Tpng", &filename, "-o", &png_filename])
            .status()?;

        if status.success() {
            // Maybe we shouldn't remove the dot file, Comment/Uncomment as needed
            let _ = std::fs::remove_file(&filename);
            Ok(png_filename)
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Failed to generate PNG with GraphViz",
            ))
        }
    }
}

/// Helper function to sort blocks in reverse postorder (returns block IDs only)
#[inline]
pub fn reverse_postorder_block_ids<T: Clone + Debug, V: Clone>(
    cfg: &CFG<T, V>,
    blocks: &[usize],
    is_reversed: bool,
) -> Vec<usize> {
    // First get postorder of NodeIds
    let mut visited = HashSet::new();
    let mut postorder = Vec::new();

    // Start DFS from entry node
    visit_dfs(NodeId::Entry, cfg, &mut visited, &mut postorder);

    // Reverse the postorder if requested
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
#[inline]
pub fn visit_dfs<T: Clone + Debug, V: Clone>(
    node: NodeId,
    cfg: &CFG<T, V>,
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
        NodeId::Exit => return, // Exit has no successors
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

/// Performs a backwards dataflow analysis on a CFG
#[must_use]
#[inline]
pub fn backwards_dataflow_analysis<
    T: Instruction + Clone + Debug,
    V: Clone + PartialEq + Default,
>(
    cfg: &CFG<T, V>,
    meet: impl Fn(&BasicBlock<T, V>, &CFG<T, V>) -> V,
    transfer: impl Fn(BasicBlock<T, V>, V) -> BasicBlock<T, V>,
) -> CFG<T, V> {
    let mut worklist = Vec::new();

    // Initialize CFG with default annotations
    let mut result_cfg = cfg.clone().initialize_annotation(&V::default());

    // Add all blocks to the worklist
    for &block_id in result_cfg.basic_blocks.keys() {
        worklist.push(block_id);
    }

    // Sort blocks in reverse postorder for efficiency
    worklist = reverse_postorder_block_ids(cfg, &worklist, false);

    // Iterate until the worklist is empty
    while let Some(block_id) = worklist.pop() {
        let block = result_cfg.basic_blocks.get(&block_id).unwrap().clone();
        let old_annotation = block.value.clone();

        // Apply meet operator to compute values at exit
        let vars_at_exit = meet(&block, &result_cfg);

        // Apply transfer function and update the block in the CFG
        let new_block = transfer(block, vars_at_exit);
        result_cfg.update_basic_block(block_id, new_block);

        // If the block's value has changed, add predecessors to the worklist
        if result_cfg.get_block_value(block_id).unwrap() != &old_annotation {
            for &pred_id in &result_cfg.basic_blocks.get(&block_id).unwrap().preds {
                if let NodeId::Block(id) = pred_id {
                    if !worklist.contains(&id) {
                        worklist.push(id);
                    }
                }
            }
        }
    }

    result_cfg
}

// Example usage
//
// fn main() {
//     let wack_instructions: Vec<WackInstruction> = /* ... */;
//     let cfg = ControlFlowGraph::from_instructions("function_name".to_string(), &wack_instructions);
//
//     // Print CFG visualization
//     if let Ok(png_path) = cfg.print_graphviz(|val, f| write!(f, "{:?}", val)) {
//         println!("Generated CFG visualization: {}", png_path);
//     }
//
//     // Convert back to instructions
//     let optimized_instructions = cfg.to_instructions();
// }
