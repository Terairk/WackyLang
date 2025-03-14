// This pass is supposed to be done first after lowering to assembly
// TODO: honestly this should be in a submodule

use std::collections::HashMap;

use build_interference_graph::build_graph;

use crate::{
    assembly_ast::{AsmInstruction, Operand, Register},
    assembly_trans::FunctionRegisters,
};

fn allocate_registers(
    instructions: Vec<AsmInstruction>,
    func_name: &str,
    func_regs: &FunctionRegisters,
) -> Vec<AsmInstruction> {
    let mut interference_graph = build_graph(&instructions, func_name, func_regs);
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
    use std::collections::HashSet;

    use util::{
        CFG,
        cfg::{BasicBlock, NodeId, backwards_dataflow_analysis},
    };
    // TODO: replace LiveRegisters with the actual type

    // #[derive(Debug, Clone, Default)]
    // struct LiveRegisters;

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
        let mut cfg = make_control_flow_graph(instructions, func_name);
        analyze_lifeness(&mut cfg, &func_reg);
        add_edges(&cfg, &mut interference_graph);
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
        let emptyCFG = AsmCFG::from_instructions(func_name.to_owned(), instructions.to_vec());
        emptyCFG.initialize_annotation(&LiveRegisters::default())
    }

    fn analyze_lifeness(cfg: &mut AsmCFG, func_regs: &FunctionRegisters) {
        let default_live_regs = LiveRegisters::default();

        // Perform backwards dataflow analysis to compute live registers at each instruction
        // Use closures to capture the function's register set
        let transfer_meet = |block: LiveBasicBlock, end_live_registers: LiveRegisters| {
            transfer(block, end_live_registers, func_regs)
        };
        backwards_dataflow_analysis(cfg, meet, transfer_meet);
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
                    // println!("{:?}", id);
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
                if let Operand::Reg(reg) = v {
                    current_live_registers.remove(&Operand::Reg(reg));
                }
            }

            for v in used {
                if let Operand::Reg(reg) = v {
                    current_live_registers.insert(Operand::Reg(reg));
                }
            }
        }

        // Annotate the block with the current live registers
        block.value = current_live_registers.clone();
        block
    }

    fn add_edges(cfg: &AsmCFG, graph: &mut InterferenceGraph) {
        todo!()
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
            AsmInstruction::Pop(register) => {
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
                updated.push(Operand::Reg(Register::AX)); // Return value
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
fn replace_ops(instruction: &AsmInstruction, f: impl Fn(Operand) -> Operand) -> AsmInstruction {
    match *instruction {
        AsmInstruction::Mov {
            typ,
            ref src,
            ref dst,
        } => AsmInstruction::Mov {
            typ,
            src: f(src.clone()),
            dst: f(dst.clone()),
        },
        AsmInstruction::Cmov {
            condition,
            typ,
            ref src,
            ref dst,
        } => AsmInstruction::Cmov {
            condition,
            typ,
            src: f(src.clone()),
            dst: f(dst.clone()),
        },
        AsmInstruction::MovZeroExtend {
            src_type,
            dst_type,
            ref src,
            ref dst,
        } => AsmInstruction::MovZeroExtend {
            src_type,
            dst_type,
            src: f(src.clone()),
            dst: f(dst.clone()),
        },
        AsmInstruction::Lea { ref src, ref dst } => AsmInstruction::Lea {
            src: f(src.clone()),
            dst: f(dst.clone()),
        },
        AsmInstruction::Unary {
            operator,
            typ,
            ref operand,
        } => AsmInstruction::Unary {
            operator,
            typ,
            operand: f(operand.clone()),
        },
        AsmInstruction::Binary {
            operator,
            typ,
            ref op1,
            ref op2,
        } => AsmInstruction::Binary {
            operator,
            typ,
            op1: f(op1.clone()),
            op2: f(op2.clone()),
        },
        AsmInstruction::Cmp {
            typ,
            ref op1,
            ref op2,
        } => AsmInstruction::Cmp {
            typ,
            op1: f(op1.clone()),
            op2: f(op2.clone()),
        },
        AsmInstruction::Test {
            typ,
            ref op1,
            ref op2,
        } => AsmInstruction::Test {
            typ,
            op1: f(op1.clone()),
            op2: f(op2.clone()),
        },
        AsmInstruction::Idiv(ref op) => AsmInstruction::Idiv(f(op.clone())),
        AsmInstruction::SetCC {
            condition,
            ref operand,
        } => AsmInstruction::SetCC {
            condition,
            operand: f(operand.clone()),
        },
        AsmInstruction::Push(ref op) => AsmInstruction::Push(f(op.clone())),
        AsmInstruction::Pop(_) => {
            panic!("Shouldn't use this as Pop doesn't have any operands to replace")
        }
        // Instructions where we don't need to replace operands
        _ => instruction.clone(),
    }
}
