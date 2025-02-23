// This occurs after first converting from Wacky IR to Assembly IR
// This pass replaces all pseudo registers with real registers

// Replace a pseudo operand with a stack operand
// Whenever we encounter a Operand::Pseudo(identifier), we check
// the given mapping and replace it if we already encountered it
// Otherwise we assign assign it the next_stack_offset, update mapping
// and subtract 4. For now we just consider ints cus its easier for now

// Next pass after this will then fix any move instructions,
// as this pass may produce mov instructions with 2 memory operands
// which isn't legal x86-64

use std::collections::HashMap;

use crate::assembly_ast::{AsmInstruction, AsmProgram, Operand};

/* ================== PUBLIC API ================== */
#[inline]
pub fn replace_pseudo_in_program(program: &mut AsmProgram) {
    for func in &mut program.asm_functions {
        // Start fresh for each function.
        let mut mapping: HashMap<String, i32> = HashMap::new();
        let mut next_stack_offset = -4;
        let mut last_offset = 0;

        for instr in &mut func.instructions {
            replace_pseudo_in_instruction(
                instr,
                &mut mapping,
                &mut next_stack_offset,
                &mut last_offset,
            );
        }

        // Prepend an AllocateStack instruction to reserve the required stack space.
        // Note this doesn't take into account parameters just yet
        // TODO: handle parameters
        // If last_offset == 0, then no temporary was used
        if last_offset != 0 {
            func.instructions
                .insert(0, AsmInstruction::AllocateStack(-last_offset));
        }
    }
}

/* ================== INTERNALS ================== */

fn replace_pseudo_operand(
    op: &mut Operand,
    mapping: &mut HashMap<String, i32>,
    next_stack_offset: &mut i32,
    last_offset: &mut i32,
) {
    if let Operand::Pseudo(ref ident) = *op {
        // If already assigned, use existing mapping
        if let Some(&offset) = mapping.get(ident) {
            *op = Operand::Stack(offset);
        } else {
            // Assign new stack offset
            let offset = *next_stack_offset;
            mapping.insert(ident.clone(), offset);
            *last_offset = offset;
            // may have unexpected side effects if it underflows
            *next_stack_offset -= 4;
            *op = Operand::Stack(offset);
        }
    }
}

fn replace_pseudo_in_instruction(
    instr: &mut AsmInstruction,
    mapping: &mut HashMap<String, i32>,
    next_stack_offset: &mut i32,
    last_offset: &mut i32,
) {
    match instr {
        AsmInstruction::Mov { src, dst, .. }
        | AsmInstruction::MovZeroExtend { src, dst, .. }
        | AsmInstruction::Lea { src, dst } => {
            replace_pseudo_operand(src, mapping, next_stack_offset, last_offset);
            replace_pseudo_operand(dst, mapping, next_stack_offset, last_offset);
        }
        AsmInstruction::Binary { op1, op2, .. } | AsmInstruction::Cmp { op1, op2, .. } => {
            replace_pseudo_operand(op1, mapping, next_stack_offset, last_offset);
            replace_pseudo_operand(op2, mapping, next_stack_offset, last_offset);
        }
        AsmInstruction::Unary { operand, .. }
        | AsmInstruction::Idiv(operand)
        | AsmInstruction::SetCC { operand, .. }
        | AsmInstruction::Push(operand) => {
            replace_pseudo_operand(operand, mapping, next_stack_offset, last_offset);
        }
        // Other instructions which don't have operands that need replacement are left unchanged.
        _ => {}
    }
}
