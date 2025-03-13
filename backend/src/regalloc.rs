// This pass is supposed to be done first after lowering to assembly

use std::collections::HashMap;

use crate::assembly_ast::{AsmInstruction, Operand, Register};

fn allocate_registers(instructions: Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    let mut interference_graph = build_graph(&instructions);
    add_spill_costs(&interference_graph, &instructions);
    color_graph(&mut interference_graph);
    let register_map = create_register_map(&interference_graph);
    let transformed_instructions = replace_pseudoregs(instructions, register_map);
    transformed_instructions
}

/* ======================== TYPES ======================== */
// Some of these are placeholder types
type PseudoReg = usize;
type RegisterMap = HashMap<PseudoReg, Register>;

/// Represents a node in the interference graph
#[derive(Debug, Clone)]
pub struct Node {
    id: Operand,
    neighbors: Vec<Operand>,
    spill_cost: f64,
    color: Option<i32>,
    pruned: bool, // Whether this node has been pruned from the graph
}

/// Represents the interference graph
#[derive(Debug)]
struct InterferenceGraph {
    nodes: Vec<Node>,

    /// Maps node IDs to their indices in the nodes vector
    id_to_index: HashMap<Operand, usize>,
}

/* ======================== FUNCTIONS ======================== */

fn build_graph(instructions: &[AsmInstruction]) -> InterferenceGraph {
    todo!()
}

fn add_spill_costs(graph: &InterferenceGraph, instructions: &[AsmInstruction]) {
    todo!()
}

fn color_graph(graph: &mut InterferenceGraph) {
    todo!()
}

fn create_register_map(graph: &InterferenceGraph) -> RegisterMap {
    todo!()
}

fn replace_pseudoregs(
    instructions: Vec<AsmInstruction>,
    register_map: RegisterMap,
) -> Vec<AsmInstruction> {
    todo!()
}
