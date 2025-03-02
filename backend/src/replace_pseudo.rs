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

use middle::wackir::WackTempIdent;

use crate::assembly_ast::{AsmInstruction, AsmProgram, AssemblyType, Operand};
type SymbolTableWack = HashMap<WackTempIdent, AssemblyType>;
type SymbolTable = HashMap<String, AssemblyType>;

/* ================== PUBLIC API ================== */
#[inline]
pub fn replace_pseudo_in_program(program: &mut AsmProgram, symbol_table: &SymbolTableWack) {
    // Quick dirty fix since I don't have WackTempIdent's anymore and only have String's
    // This is a temporary fix until I can figure out how to get the WackTempIdent's back
    // / don't convert them to Strings

    let symbol_table: SymbolTable = symbol_table
        .clone()
        .into_iter()
        .map(|(k, v)| (k.into(), v))
        .collect();
    // println!("{:?}", symbol_table);
    for func in &mut program.asm_functions {
        // Start fresh for each function.
        let mut mapping: HashMap<String, i32> = HashMap::new();
        let mut next_stack_offset = 0;
        let mut last_offset = 0;

        for instr in &mut func.instructions {
            replace_pseudo_in_instruction(
                instr,
                &mut mapping,
                &mut next_stack_offset,
                &mut last_offset,
                &symbol_table,
            );
        }

        // Prepend an AllocateStack instruction to reserve the required stack space.
        // Note this doesn't take into account parameters just yet

        // rounds up to nearest multiple of 16 (negative version)
        last_offset = round_down_16(last_offset);
        // index 2 here since we have push rbp and mov rbp, rsp
        if last_offset != 0 {
            func.instructions
                .insert(2, AsmInstruction::AllocateStack(-last_offset));
            func.instructions.insert(
                2,
                AsmInstruction::Comment(
                    "This allocate ensures stack is 16-byte aligned".to_owned(),
                ),
            );
        }
    }
}

/* ================== INTERNALS ================== */

fn replace_pseudo_operand(
    op: &mut Operand,
    mapping: &mut HashMap<String, i32>,
    next_stack_offset: &mut i32,
    last_offset: &mut i32,
    symbol_table: &SymbolTable,
) {
    if let Operand::Pseudo(ref ident) = *op {
        // If already assigned, use existing mapping
        if let Some(&offset) = mapping.get(ident) {
            *op = Operand::Stack(offset);
        } else {
            // Assign new stack offset
            // Correct alignment to match System V ABI;
            let asm_type = symbol_table.get(ident).unwrap();
            let alignment = get_alignment(*asm_type);
            // println!("next_stack_offset: {next_stack_offset}, alignment = {alignment}");
            *next_stack_offset -= alignment;
            *next_stack_offset = round_down(*next_stack_offset, alignment);
            // println!("next_stack_offset: {next_stack_offset}");

            let offset = *next_stack_offset;
            mapping.insert(ident.clone(), offset);
            *last_offset = offset;
            // may have unexpected side effects if it underflows
            *op = Operand::Stack(offset);
        }
    }
}

fn replace_pseudo_in_instruction(
    instr: &mut AsmInstruction,
    mapping: &mut HashMap<String, i32>,
    next_stack_offset: &mut i32,
    last_offset: &mut i32,
    symbol_table: &SymbolTable,
) {
    use AsmInstruction::*;
    match instr {
        Mov { src, dst, .. }
        | Cmov { src, dst, .. }
        | MovZeroExtend { src, dst, .. }
        | Lea { src, dst } => {
            replace_pseudo_operand(src, mapping, next_stack_offset, last_offset, symbol_table);
            replace_pseudo_operand(dst, mapping, next_stack_offset, last_offset, symbol_table);
        }
        Binary { op1, op2, .. } | Cmp { op1, op2, .. } | Test { op1, op2, .. } => {
            replace_pseudo_operand(op1, mapping, next_stack_offset, last_offset, symbol_table);
            replace_pseudo_operand(op2, mapping, next_stack_offset, last_offset, symbol_table);
        }
        Unary { operand, .. } | Idiv(operand) | SetCC { operand, .. } | Push(operand) => {
            replace_pseudo_operand(
                operand,
                mapping,
                next_stack_offset,
                last_offset,
                symbol_table,
            );
        }
        // Other instructions which don't have operands that need replacement are left unchanged.
        _ => {}
    }
}

// rounds down to neartest multiple of 16
// meant for negative numbers
const fn round_down_16(x: i32) -> i32 {
    x & !15
}

const fn round_down(x: i32, multiple: i32) -> i32 {
    x & !(multiple - 1)
}

const fn get_alignment(typ: AssemblyType) -> i32 {
    use AssemblyType::*;
    match typ {
        Byte => 1,
        Longword => 4,
        Quadword => 8,
    }
}
