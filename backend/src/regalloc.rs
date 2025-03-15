// This pass is supposed to be done first after lowering to assembly
// TODO: honestly this should be in a submodule

use std::collections::{BTreeSet, HashMap};
use std::fmt;

use build_interference_graph::build_graph;

use crate::{
    assembly_ast::{AsmFunction, AsmInstruction, AsmProgram, Operand, Register},
    assembly_trans::FunctionRegisters,
    registers::{LEN_ALL_BASEREGS, is_callee_saved, is_callee_saved_reg},
};
pub type FunctionCallee = HashMap<String, BTreeSet<Register>>;

// This is messy code but I can fix this later
#[must_use]
#[inline]
pub fn allocate_registers_program(
    program: AsmProgram,
    func_regs: &FunctionRegisters,
    func_callee_regs: &mut FunctionCallee,
) -> AsmProgram {
    let mut new_functions = Vec::new();

    for function in program.asm_functions {
        let new_function = allocate_registers(function, func_regs, func_callee_regs);
        new_functions.push(new_function);
    }

    let updated_program = AsmProgram {
        asm_functions: new_functions,
    };

    updated_program
}

fn allocate_registers(
    mut function: AsmFunction,
    func_regs: &FunctionRegisters,
    func_callee_regs: &mut FunctionCallee,
) -> AsmFunction {
    let func_name = function.name.as_str();
    let instructions = function.instructions;

    // Main register allocation logic
    let mut interference_graph = build_graph(&instructions, func_name, func_regs);
    add_spill_costs(&mut interference_graph, &instructions);
    color_graph(&mut interference_graph);
    let register_map = create_register_map(&interference_graph, func_callee_regs, func_name);
    let transformed_instructions = replace_pseudoregs(instructions, &register_map);

    // Update the function with the transformed instructions
    function.instructions = transformed_instructions;
    function
}

/* ======================== TYPES ======================== */
// Some of these are placeholder types
type PseudoReg = String;
type RegisterMap = HashMap<PseudoReg, Register>;

/// Represents a node in the interference graph
#[derive(Clone)]
pub struct Node {
    id: Operand,
    neighbors: Vec<usize>, // Indices into graph's nodes vector
    spill_cost: f64,
    color: Option<i32>,
    pruned: bool, // Whether this node has been pruned from the graph
}

/// Represents the interference graph
#[derive(Clone)]
struct InterferenceGraph {
    nodes: Vec<Node>,

    /// Maps node IDs to their indices in the nodes vector
    id_to_index: HashMap<Operand, usize>,
}

/* ================== Impl's for Types ================= */
impl Node {
    pub const DEFAULT_SPILL_COST: f64 = 0.0;
    /// Creates a new node
    pub fn new(id: Operand) -> Self {
        Node {
            id,
            neighbors: Vec::new(),
            spill_cost: Self::DEFAULT_SPILL_COST,
            color: None,
            pruned: false,
        }
    }

    pub fn update_spill_cost(&mut self, cost: f64) {
        self.spill_cost = cost;
    }

    /// Returns the node's spill cost
    pub fn spill_cost(&self) -> f64 {
        self.spill_cost
    }

    /// Returns whether the node has been colored
    pub fn is_colored(&self) -> bool {
        self.color.is_some()
    }

    /// Returns the node's color if assigned
    pub fn color(&self) -> Option<i32> {
        self.color
    }

    /// Sets the node's color
    pub fn set_color(&mut self, color: i32) {
        self.color = Some(color);
    }

    /// Returns whether the node has been pruned
    pub fn is_pruned(&self) -> bool {
        self.pruned
    }

    /// Marks the node as pruned
    pub fn prune(&mut self) {
        self.pruned = true;
    }

    /// Unmarks the node as pruned
    pub fn unprune(&mut self) {
        self.pruned = false;
    }

    pub fn get_id(&self) -> &Operand {
        &self.id
    }
}

impl InterferenceGraph {
    /// Creates a new empty graph
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            id_to_index: std::collections::HashMap::new(),
        }
    }

    /// Adds a node to the graph
    pub fn add_node(&mut self, id: &Operand, spill_cost: f64) -> usize {
        if let Some(&index) = self.id_to_index.get(&id) {
            return index;
        }

        let index = self.nodes.len();
        let mut node = Node::new(id.clone());
        node.update_spill_cost(spill_cost);
        self.nodes.push(node);
        self.id_to_index.insert(id.clone(), index);
        index
    }

    /// Adds an interference edge between two nodes
    pub fn add_edge(&mut self, id1: &Operand, id2: &Operand) -> Result<(), String> {
        let index1 = self.get_node_index(id1)?;
        let index2 = self.get_node_index(id2)?;

        if index1 != index2 {
            // Add each node as a neighbor of the other, if not already present
            if !self.nodes[index1].neighbors.contains(&index2) {
                self.nodes[index1].neighbors.push(index2);
            }
            if !self.nodes[index2].neighbors.contains(&index1) {
                self.nodes[index2].neighbors.push(index1);
            }
        }

        Ok(())
    }

    /// Gets a node's index by its ID
    fn get_node_index(&self, id: &Operand) -> Result<usize, String> {
        self.id_to_index
            .get(id)
            .copied()
            .ok_or_else(|| format!("Node with ID '{:?}' not found", id))
    }

    // Keep these in in case we use them in the future

    ///// Returns a reference to a node by its ID
    //pub fn get_node(&self, id: &Operand) -> Result<&Node, String> {
    //    let index = self.get_node_index(id)?;
    //    Ok(&self.nodes[index])
    //}

    /// Returns a mutable reference to a node by its ID
    ///
    // pub fn get_node_mut(&mut self, id: &Operand) -> Result<&mut Node, String> {
    //     let index = self.get_node_index(id)?;
    //     Ok(&mut self.nodes[index])
    // }

    /// Returns the number of nodes in the graph
    // pub fn size(&self) -> usize {
    //     self.nodes.len()
    // }

    /// Returns the neighbors of a node
    // pub fn neighbors(&self, id: &Operand) -> Result<Vec<&Node>, String> {
    //     let node_index = self.get_node_index(id)?;
    //     let neighbors = self.nodes[node_index]
    //         .neighbors
    //         .iter()
    //         .map(|&idx| &self.nodes[idx])
    //         .collect();
    //     Ok(neighbors)
    // }

    pub fn nodes(&self) -> &Vec<Node> {
        &self.nodes
    }
}

/* ======================== FUNCTIONS ======================== */

mod build_interference_graph {
    use std::collections::HashSet;

    use util::{
        CFG,
        cfg::{BasicBlock, NodeId, backwards_dataflow_analysis},
    };

    // This is supposed to use Operands apparently -- maybe for easy conversion
    // And because we'll add PseudoRegisters here
    type LiveRegisters = HashSet<Operand>;

    // type LiveVariables = HashSet<WackTempIdent>;
    type AsmCFG = CFG<AsmInstruction, LiveRegisters>;
    type LiveBasicBlock = BasicBlock<AsmInstruction, LiveRegisters>;

    use crate::{
        assembly_trans::FunctionRegisters,
        registers::{ALL_BASEREGS, RS6, register_set_to_vec},
    };

    use super::*;

    /// Build's interference graph for a function using a control flow graph internally
    pub fn build_graph(
        instructions: &[AsmInstruction],
        func_name: &str,
        func_reg: &FunctionRegisters,
    ) -> InterferenceGraph {
        let mut interference_graph = create_base_graph();
        add_pseudoregisters(&mut interference_graph, instructions);
        let cfg = make_control_flow_graph(instructions, func_name);
        let new_cfg = analyze_lifeness(&cfg, func_reg);

        // Print CFG visualization, useful for debugging so leave it in
        // let mut counter = 80;
        // if let Ok(png_path) = new_cfg.print_graphviz(&mut counter) {
        //     println!("Generated CFG visualization: {png_path}");
        // }

        add_edges(&new_cfg, &mut interference_graph, func_reg);
        interference_graph
    }

    fn create_base_graph() -> InterferenceGraph {
        let mut graph = InterferenceGraph::new();

        // First, add all hardware registers as nodes
        for &reg in &ALL_BASEREGS {
            let operand = Operand::Reg(reg);
            graph.add_node(&operand, f64::INFINITY); // Can't spill hardware registers
        }

        // Now add interference edges between all pairs of hardware registers
        for &reg1 in &ALL_BASEREGS {
            for &reg2 in &ALL_BASEREGS {
                if reg1 != reg2 {
                    let operand1 = Operand::Reg(reg1);
                    let operand2 = Operand::Reg(reg2);
                    graph.add_edge(&operand1, &operand2).unwrap();
                }
            }
        }
        graph
    }

    fn add_pseudoregisters(graph: &mut InterferenceGraph, instructions: &[AsmInstruction]) {
        for instruction in instructions {
            for operand in get_operands(instruction) {
                if let Operand::Pseudo(_) = operand {
                    // We don't have any pseudoregisters with static storage duration
                    // so simply add them
                    graph.add_node(&operand, Node::DEFAULT_SPILL_COST);
                }
            }
        }
    }

    fn make_control_flow_graph(instructions: &[AsmInstruction], func_name: &str) -> AsmCFG {
        let empty_cfg = AsmCFG::from_instructions(func_name.to_owned(), instructions.to_vec());
        empty_cfg.initialize_annotation(&LiveRegisters::default())
    }

    fn analyze_lifeness(cfg: &AsmCFG, func_regs: &FunctionRegisters) -> AsmCFG {
        // Perform backwards dataflow analysis to compute live registers at each instruction
        // Use closures to capture the function's register set
        let transfer_meet = |block: LiveBasicBlock, end_live_registers: LiveRegisters| {
            transfer(block, end_live_registers, func_regs)
        };
        backwards_dataflow_analysis(cfg, meet, transfer_meet)
    }

    /// The meet operator calculates which registers are live at the end of a basic block.
    /// It is the union of the live registers at the beginning of each successor block.
    // We ignore the fact that callee-saved registers are technically live at the end of a function
    // But we do something during the instruction fix-up pass
    fn meet(block: &LiveBasicBlock, cfg: &AsmCFG) -> LiveRegisters {
        let mut live_regs = LiveRegisters::new();
        // println!("{:?}", cfg);
        for succ_id in &block.succs {
            match *succ_id {
                NodeId::Entry => panic!("Entry node should not be a successor"),
                NodeId::Exit => {
                    live_regs.insert(Operand::Reg(Register::AX));
                }
                NodeId::Block(id) => {
                    let succ_live_regs = cfg
                        .get_block_value(id)
                        .expect("CFG is malformed or corrupted")
                        .clone();
                    live_regs = live_regs.union(&succ_live_regs).cloned().collect();
                }
            }
        }
        live_regs
    }

    fn transfer(
        mut block: LiveBasicBlock,
        end_live_registers: LiveRegisters,
        func_regs: &FunctionRegisters,
    ) -> LiveBasicBlock {
        let mut current_live_registers = end_live_registers;

        // Process instructions in reverse order
        for instr in block.instructions.iter_mut().rev() {
            // Annotate the instructions with the current live registers
            instr.0 = current_live_registers.clone();
            let (used, updated) = find_used_and_updated(instr.1.clone(), func_regs);
            for v in updated {
                match v {
                    Operand::Pseudo(_) => {
                        current_live_registers.remove(&v);
                    }
                    Operand::Reg(_) => {
                        current_live_registers.remove(&v);
                    }
                    _ => {}
                }
            }

            for v in used {
                match v {
                    Operand::Pseudo(_) => {
                        current_live_registers.insert(v);
                    }
                    Operand::Reg(_) => {
                        current_live_registers.insert(v);
                    }
                    _ => {}
                }
            }
        }

        // Annotate the block with the current live registers
        block.value = current_live_registers.clone();
        block
    }

    fn add_edges(cfg: &AsmCFG, graph: &mut InterferenceGraph, func_regs: &FunctionRegisters) {
        // Iterate through all nodes in the CFG
        for (_block_id, block) in &cfg.basic_blocks {
            if block.id == NodeId::Entry || block.id == NodeId::Exit {
                continue;
            }

            // Process each instruction in the block
            for (live_regs, instr) in &block.instructions {
                // We only care about updated in this context
                let (_used, updated) = find_used_and_updated(instr.clone(), func_regs);

                // Check each live register against updated registers
                for l in live_regs {
                    let is_move_with_l_as_src = match instr {
                        AsmInstruction::Mov { src, .. } if src == l => true,
                        _ => false,
                    };

                    if is_move_with_l_as_src {
                        // If l is the source of a move, it is not live after the instruction
                        continue;
                    }

                    // For each updated register, add interference edge if l and u are different
                    for u in &updated {
                        if l != u {
                            let _ = graph.add_edge(l, u);
                        }
                    }
                }
            }
        }
    }

    fn find_used_and_updated(
        instr: AsmInstruction,
        func_set: &FunctionRegisters,
    ) -> (Vec<Operand>, Vec<Operand>) {
        let mut used = Vec::new();
        let mut updated = Vec::new();

        match instr {
            AsmInstruction::Mov { src, dst, .. } => {
                used.push(src);
                updated.push(dst);
            }
            AsmInstruction::Cmov { src, dst, .. } => {
                used.push(src);
                used.push(dst.clone());
                updated.push(dst);
            }
            AsmInstruction::MovZeroExtend { src, dst, .. } => {
                used.push(src);
                updated.push(dst);
            }
            AsmInstruction::Lea { src, dst, .. } => {
                used.push(src);
                updated.push(dst);
            }
            AsmInstruction::Unary { operand, .. } => {
                used.push(operand.clone());
                updated.push(operand);
            }
            AsmInstruction::Binary { op1, op2, .. } => {
                used.push(op1);
                used.push(op2.clone());
                // Destination is both updated and used
                updated.push(op2);
            }
            AsmInstruction::Cmp { op1, op2, .. } => {
                used.push(op1);
                used.push(op2);
                // No updates for Cmp
            }
            AsmInstruction::Test { op1, op2, .. } => {
                used.push(op1);
                used.push(op2);
                // No updates for Test
            }
            AsmInstruction::Idiv(divisor) => {
                used.push(divisor);
                used.push(Operand::Reg(Register::AX));
                used.push(Operand::Reg(Register::DX));
                updated.push(Operand::Reg(Register::AX));
                updated.push(Operand::Reg(Register::DX));
            }
            AsmInstruction::Cdq => {
                used.push(Operand::Reg(Register::AX));
                updated.push(Operand::Reg(Register::DX));
            }
            AsmInstruction::Jmp(_, _) => {
                // No registers used or updated
            }
            AsmInstruction::JmpCC { .. } => {
                // No registers used or updated
            }
            AsmInstruction::SetCC { operand, .. } => {
                updated.push(operand);
            }
            AsmInstruction::Label(_) => {
                // No registers used or updated
            }
            AsmInstruction::Comment(_) => {
                // No registers used or updated
            }
            AsmInstruction::AllocateStack(_) => {
                // No registers used or updated
            }
            AsmInstruction::DeallocateStack(_) => {
                // No registers used or updated
            }
            AsmInstruction::Push(operand) => {
                used.push(operand);
            }
            AsmInstruction::Pop(_register) => {
                panic!("Pop should not be used in this context");
            }
            AsmInstruction::Call(fun_name, _) => {
                // let new_regs = Vec::new();

                // If we can't determine the function's register set, assume it uses all
                // param-passing registers
                let reg_set = match func_set.get(&fun_name) {
                    Some(set) => set,
                    None => {
                        println!("Warning: Function {} not found in function set", fun_name);
                        &RS6
                    }
                };

                let regs = register_set_to_vec(*reg_set);
                for reg in regs {
                    used.push(Operand::Reg(reg));
                }

                // Caller-saved registers that might be modified by the callee
                updated.push(Operand::Reg(Register::DI));
                updated.push(Operand::Reg(Register::SI));
                updated.push(Operand::Reg(Register::DX));
                updated.push(Operand::Reg(Register::CX));
                updated.push(Operand::Reg(Register::R8));
                updated.push(Operand::Reg(Register::R9));
                updated.push(Operand::Reg(Register::AX));
            }
            AsmInstruction::Ret => {
                // This is handled by our Exit node in the meet function
            }
        }

        // Convert operands to the registers they use/update

        // Process registers used to read
        let mut final_used = Vec::new();
        for operand in &used {
            match operand {
                Operand::Pseudo(_) | Operand::Reg(_) => {
                    final_used.push(operand.clone());
                }
                Operand::Memory(reg, _) => {
                    final_used.push(Operand::Reg(*reg));
                }
                Operand::Indexed { base, index, .. } => {
                    final_used.push(Operand::Reg(*base));
                    final_used.push(Operand::Reg(*index));
                }
                Operand::Imm(_) | Operand::Data(_, _) => {
                    // These don't use registers
                }
            }
        }

        // Process registers used to update
        let mut additional_used = Vec::new();
        let mut final_updated = Vec::new();

        for operand in &updated {
            match operand {
                Operand::Pseudo(_) | Operand::Reg(_) => {
                    final_updated.push(operand.clone());
                }
                Operand::Memory(reg, _) => {
                    // Reading from the register to get the memory address
                    additional_used.push(Operand::Reg(*reg));
                    // Not adding anything to updated since we're updating memory, not a register
                }
                Operand::Indexed { base, index, .. } => {
                    // Reading from both registers to get the memory address
                    additional_used.push(Operand::Reg(*base));
                    additional_used.push(Operand::Reg(*index));
                    // Not adding anything to updated since we're updating memory, not a register
                }
                Operand::Imm(_) | Operand::Data(_, _) => {
                    // These can't be updated
                }
            }
        }

        // Add the additional used registers to the final used list
        final_used.extend(additional_used);

        (final_used, final_updated)
    }
}

fn add_spill_costs(graph: &mut InterferenceGraph, instructions: &[AsmInstruction]) {
    // Create a HashMap to track the number of appearances for each operand
    let mut appearance_counts: HashMap<Operand, f64> = HashMap::new();

    // Count the number of times each operand appears in the instructions
    for instruction in instructions {
        // Extract all operands from the instruction
        let operands = get_operands(instruction);

        // Increment the count for each operand
        for operand in operands {
            // Only track registers and pseudoregisters
            if let Operand::Pseudo(_) = &operand {
                *appearance_counts.entry(operand).or_insert(0.0) += 1.0;
            }
        }
    }

    // Assign calculated costs to pseudoregisters
    for (operand, count) in appearance_counts {
        match &operand {
            Operand::Pseudo(_) => {
                // Pseudoregisters get a spill cost equal to their number of appearances
                if let Some(&index) = graph.id_to_index.get(&operand) {
                    graph.nodes[index].update_spill_cost(count);
                } else {
                    // Add the node if it doesn't exist yet
                    graph.add_node(&operand, count);
                }
            }
            _ => {} // Ignore other types of operands
        }
    }
}

fn color_graph(graph: &mut InterferenceGraph) {
    // Get the list of unpruned nodes
    let unpruned_nodes: Vec<usize> = graph
        .nodes
        .iter()
        .enumerate()
        .filter(|(_, node)| !node.is_pruned())
        .map(|(idx, _)| idx)
        .collect();

    // Base case: if there are no unpruned nodes left, we're done
    if unpruned_nodes.is_empty() {
        return;
    }

    // Choose the next node to prune
    let mut chosen_node = None;
    let k = LEN_ALL_BASEREGS;

    // First try to find a node with degree < k
    for &node_idx in &unpruned_nodes {
        let unpruned_neighbors_count = graph.nodes[node_idx]
            .neighbors
            .iter()
            .filter(|&&neighbor_idx| !graph.nodes[neighbor_idx].is_pruned())
            .count();

        if unpruned_neighbors_count < k {
            chosen_node = Some(node_idx);
            break;
        }
    }

    // If no node with degree < k was found, choose a spill candidate
    if chosen_node.is_none() {
        let mut best_spill_metric = f64::INFINITY;

        for &node_idx in &unpruned_nodes {
            let unpruned_neighbors_count = graph.nodes[node_idx]
                .neighbors
                .iter()
                .filter(|&&neighbor_idx| !graph.nodes[neighbor_idx].is_pruned())
                .count();

            // Avoid division by zero
            if unpruned_neighbors_count > 0 {
                let spill_metric =
                    graph.nodes[node_idx].spill_cost() / unpruned_neighbors_count as f64;

                if spill_metric < best_spill_metric {
                    chosen_node = Some(node_idx);
                    best_spill_metric = spill_metric;
                }
            }
        }
    }

    // Prune the chosen node
    if let Some(node_idx) = chosen_node {
        graph.nodes[node_idx].prune();

        // Color the rest of the graph recursively
        color_graph(graph);

        // Now try to color the chosen node
        let mut available_colors: Vec<i32> = (1..=k).map(|x| x as i32).collect();

        // Remove colors that are already used by neighbors
        for &neighbor_idx in &graph.nodes[node_idx].neighbors {
            if let Some(color) = graph.nodes[neighbor_idx].color() {
                available_colors.retain(|&c| c != color);
            }
        }

        // If there are colors available, assign one to the node
        if !available_colors.is_empty() {
            let is_callee_saved = is_callee_saved(&graph.nodes[node_idx].id);

            let color = if is_callee_saved {
                // Assign highest-numbered color for callee-saved registers
                *available_colors.iter().max().unwrap()
            } else {
                // Assign lowest-numbered color for others
                *available_colors.iter().min().unwrap()
            };

            graph.nodes[node_idx].set_color(color);
            graph.nodes[node_idx].unprune(); // Mark as unpruned (back in the graph)
        }
        // If no colors available, the node stays pruned (spilled)
    }
}

fn create_register_map(
    graph: &InterferenceGraph,
    func_callee_regs: &mut FunctionCallee,
    func_name: &str,
) -> RegisterMap {
    // build map from colours to hard registers
    let mut colour_map: HashMap<i32, Register> = HashMap::new();
    for node in graph.nodes() {
        match node.get_id() {
            Operand::Reg(r) => {
                colour_map.insert(
                    node.color().expect("hard registers should have a colour"),
                    *r,
                );
            }
            Operand::Pseudo(_) => {}
            _ => panic!("Shouldn't expect other things here"),
        }
    }

    // build map from pseudo registers to hard reisters
    let mut register_map: RegisterMap = RegisterMap::new();
    let mut callee_saved_regs: BTreeSet<Register> = BTreeSet::new();

    for node in graph.nodes() {
        match node.get_id() {
            Operand::Pseudo(p) => match node.color() {
                Some(colour) => {
                    let hardreg = colour_map.get(&colour).unwrap();
                    register_map.insert(p.clone(), hardreg.clone());
                    if is_callee_saved_reg(hardreg) {
                        callee_saved_regs.insert(*hardreg);
                    }
                }
                None => {}
            },
            Operand::Reg(_) => {}
            _ => panic!("Shouldn't expect other Operands in the graph"),
        }
    }

    func_callee_regs.insert(func_name.to_owned(), callee_saved_regs);
    register_map
}

fn replace_pseudoregs(
    instructions: Vec<AsmInstruction>,
    register_map: &RegisterMap,
) -> Vec<AsmInstruction> {
    let map_pseudo = |op: Operand| match op {
        Operand::Pseudo(p) => {
            let hardreg = register_map.get(&p);
            // println!("operator p in replace_pseudoregs: {:?}", p);
            if let Some(hardreg) = hardreg {
                Operand::Reg(*hardreg)
            } else {
                Operand::Pseudo(p)
            }
        }
        _ => op,
    };

    // It actually isn't safe to remove instructions where
    // the source and dst for a mov is the same because
    // movl with clear the upper 32 bits of the register
    // so removing it will cause the program to behave differently
    instructions
        .into_iter()
        .map(|instr| replace_ops(instr, map_pseudo))
        .collect()
}

/// Extract all operands from an instruction.
/// NOTE: don't need to include implicit operands (e.g ax/dx for cdq)
/// because we only use this to find pseudos
fn get_operands(instruction: &AsmInstruction) -> Vec<Operand> {
    match instruction {
        AsmInstruction::Mov { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Cmov { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::MovZeroExtend { src, dst, .. } => vec![src.clone(), dst.clone()],
        AsmInstruction::Lea { src, dst } => vec![src.clone(), dst.clone()],
        AsmInstruction::Unary { operand, .. } => vec![operand.clone()],
        AsmInstruction::Binary { op1, op2, .. } => vec![op1.clone(), op2.clone()],
        AsmInstruction::Cmp { op1, op2, .. } => vec![op1.clone(), op2.clone()],
        AsmInstruction::Test { op1, op2, .. } => vec![op1.clone(), op2.clone()],
        AsmInstruction::Idiv(op) => vec![op.clone()],
        AsmInstruction::SetCC { operand, .. } => vec![operand.clone()],
        AsmInstruction::Push(op) => vec![op.clone()],
        AsmInstruction::Pop(_) => panic!("Shouldn't use this yet"),
        // Instructions with no operands to extract
        AsmInstruction::Label(_)
        | AsmInstruction::Call(_, _)
        | AsmInstruction::Ret
        | AsmInstruction::Cdq
        | AsmInstruction::JmpCC { .. }
        | AsmInstruction::Jmp(_, _)
        | AsmInstruction::Comment(_)
        | AsmInstruction::AllocateStack(_)
        | AsmInstruction::DeallocateStack(_) => vec![],
    }
}

/// Map function f over all the operands in an instruction
fn replace_ops(instruction: AsmInstruction, f: impl Fn(Operand) -> Operand) -> AsmInstruction {
    // println!("instruction in get_operands: {:?}", instruction);
    match instruction {
        AsmInstruction::Mov { typ, src, dst } => AsmInstruction::Mov {
            typ,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Cmov {
            condition,
            typ,
            src,
            dst,
        } => AsmInstruction::Cmov {
            condition,
            typ,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::MovZeroExtend {
            src_type,
            dst_type,
            src,
            dst,
        } => AsmInstruction::MovZeroExtend {
            src_type,
            dst_type,
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Lea { src, dst } => AsmInstruction::Lea {
            src: f(src),
            dst: f(dst),
        },
        AsmInstruction::Unary {
            operator,
            typ,
            operand,
        } => AsmInstruction::Unary {
            operator,
            typ,
            operand: f(operand),
        },
        AsmInstruction::Binary {
            operator,
            typ,
            op1,
            op2,
        } => AsmInstruction::Binary {
            operator,
            typ,
            op1: f(op1),
            op2: f(op2),
        },
        AsmInstruction::Cmp { typ, op1, op2 } => AsmInstruction::Cmp {
            typ,
            op1: f(op1),
            op2: f(op2),
        },
        AsmInstruction::Test { typ, op1, op2 } => AsmInstruction::Test {
            typ,
            op1: f(op1),
            op2: f(op2),
        },
        AsmInstruction::Idiv(op) => AsmInstruction::Idiv(f(op)),
        AsmInstruction::SetCC { condition, operand } => AsmInstruction::SetCC {
            condition,
            operand: f(operand),
        },
        AsmInstruction::Push(op) => AsmInstruction::Push(f(op)),
        AsmInstruction::Pop(_) => {
            panic!("Shouldn't use this as Pop doesn't have any operands to replace")
        }
        // Instructions where we don't need to replace operands
        _ => instruction.clone(),
    }
}

// Custom Debug implementation for InterferenceGraph
impl fmt::Debug for InterferenceGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Opening line
        writeln!(f, "InterferenceGraph {{")?;

        // Format nodes
        writeln!(f, "    nodes: [")?;
        for (i, node) in self.nodes.iter().enumerate() {
            write!(f, "        ")?;
            fmt::Debug::fmt(node, f)?;
            if i < self.nodes.len() - 1 {
                writeln!(f, ",")?;
            } else {
                writeln!(f)?;
            }
        }
        writeln!(f, "    ],")?;

        // Format id_to_index map
        writeln!(f, "    id_to_index: {{")?;
        let mut entries: Vec<_> = self.id_to_index.iter().collect();
        entries.sort_by_key(|(k, _)| k.to_string());

        for (i, (key, value)) in entries.iter().enumerate() {
            write!(f, "        {key:?}: {value}")?;
            if i < entries.len() - 1 {
                writeln!(f, ",")?;
            } else {
                writeln!(f)?;
            }
        }
        writeln!(f, "    }}")?;

        // Closing brace
        write!(f, "}}")
    }
}

// Custom Debug implementation for Node
impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Node {{")?;
        write!(f, " id: {:?},", self.id)?;

        write!(f, " neighbors: [")?;
        for (i, &neighbor) in self.neighbors.iter().enumerate() {
            write!(f, "{}", neighbor)?;
            if i < self.neighbors.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "],")?;

        write!(f, " spill_cost: {:.2},", self.spill_cost)?;
        write!(f, " color: {:?},", self.color)?;
        write!(f, " pruned: {}", self.pruned)?;
        write!(f, " }}")
    }
}
