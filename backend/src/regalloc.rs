// This pass is supposed to be done first after lowering to assembly
// TODO: honestly this should be in a submodule

use std::collections::HashMap;

use build_interference_graph::build_graph;

use crate::assembly_ast::{AsmInstruction, Operand, Register};

fn allocate_registers(instructions: Vec<AsmInstruction>, func_name: &str) -> Vec<AsmInstruction> {
    let mut interference_graph = build_graph(&instructions, func_name);
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
    neighbors: Vec<usize>, // Indices into graph's nodes vector
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

/* ================== Impl's for Types ================= */
impl Node {
    /// Creates a new node
    pub fn new(id: Operand) -> Self {
        Node {
            id,
            neighbors: Vec::new(),
            spill_cost: 0.0,
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

    // pub fn add_neighbor(&mut self, neighbor: usize) {
    //     self.neighbors.push(neighbor);
    // }
    //
    // pub fn add_neighbors(&mut self, neighbors: &[usize]) {
    //     self.neighbors.extend(neighbors.iter().cloned());
    // }
    //
    // pub fn neighbors(&self) -> &[usize] {
    //     &self.neighbors
    // }
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

    /// Returns a reference to a node by its ID
    pub fn get_node(&self, id: &Operand) -> Result<&Node, String> {
        let index = self.get_node_index(id)?;
        Ok(&self.nodes[index])
    }

    /// Returns a mutable reference to a node by its ID
    pub fn get_node_mut(&mut self, id: &Operand) -> Result<&mut Node, String> {
        let index = self.get_node_index(id)?;
        Ok(&mut self.nodes[index])
    }

    /// Returns the number of nodes in the graph
    pub fn size(&self) -> usize {
        self.nodes.len()
    }

    /// Returns the neighbors of a node
    pub fn neighbors(&self, id: &Operand) -> Result<Vec<&Node>, String> {
        let node_index = self.get_node_index(id)?;
        let neighbors = self.nodes[node_index]
            .neighbors
            .iter()
            .map(|&idx| &self.nodes[idx])
            .collect();
        Ok(neighbors)
    }
}

/* ======================== FUNCTIONS ======================== */

mod build_interference_graph {
    use util::CFG;
    // TODO: replace LiveRegisters with the actual type
    struct LiveRegisters;
    type AsmCFG = CFG<AsmInstruction, LiveRegisters>;

    use super::*;

    pub fn build_graph(instructions: &[AsmInstruction], func_name: &str) -> InterferenceGraph {
        let mut interference_graph = create_base_graph();
        add_pseudoregisters(&mut interference_graph, instructions);
        let cfg = make_control_flow_graph(instructions, func_name);
        analyze_lifeness(&cfg);
        add_edges(&cfg, &mut interference_graph);
        interference_graph
    }

    fn create_base_graph() -> InterferenceGraph {
        todo!()
    }

    fn add_pseudoregisters(graph: &mut InterferenceGraph, instructions: &[AsmInstruction]) {
        todo!()
    }

    fn make_control_flow_graph(instructions: &[AsmInstruction], func_name: &str) -> AsmCFG {
        todo!()
    }

    fn analyze_lifeness(cfg: &AsmCFG) {
        todo!()
    }

    fn add_edges(cfg: &AsmCFG, graph: &mut InterferenceGraph) {
        todo!()
    }
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
