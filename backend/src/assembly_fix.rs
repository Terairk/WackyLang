// This pass is the last pass currently and will just fix any assembly instructions
// where you have 2 memory operands in a mov instruction. This may need to be changed in the future
// to handle other instructions but for now its mov

/* ================== INVARIANTS =================== */
// We'll use R10D as a scratch register because it usually doesn't serve
// any special purpose

use crate::assembly_ast::{
    AsmFunction, AsmInstruction, AsmProgram, AssemblyType, Operand, Register,
};

#[must_use]
#[inline]
pub fn fix_instructions(program: AsmProgram) -> AsmProgram {
    // Process each function separately
    let mut new_functions: Vec<AsmFunction> = Vec::new();

    for func in program.asm_functions {
        let mut new_func_body: Vec<AsmInstruction> = Vec::new();
        for instr in func.instructions {
            match instr {
                AsmInstruction::Mov { typ, src, dst } => {
                    fix_move(&mut new_func_body, typ, src, dst);
                }
                other => new_func_body.push(other),
            }
        }

        // TODO: maybe we can use bon crate
        new_functions.push(AsmFunction {
            name: func.name,
            global: func.global,
            external: func.external,
            instructions: new_func_body,
        });
    }

    AsmProgram {
        asm_functions: new_functions,
    }
}

fn fix_move(func_body: &mut Vec<AsmInstruction>, typ: AssemblyType, src: Operand, dst: Operand) {
    match (src.clone(), dst.clone()) {
        (Operand::Stack(_), Operand::Stack(_)) => {
            // We'll use R10D as a scratch register because it usually doesn't serve
            // any special purpose
            let first_move = AsmInstruction::Mov {
                typ: typ.clone(),
                src,
                dst: Operand::Reg(Register::R10),
            };
            let second_move = AsmInstruction::Mov {
                typ,
                src: Operand::Reg(Register::R10),
                dst,
            };
            func_body.push(first_move);
            func_body.push(second_move);
        }
        _ => func_body.push(AsmInstruction::Mov { typ, src, dst }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fix_instructions_allocate_and_mov_fix() {
        /* Create a function with two Mov instructions:
         * movl -4(%rbp), -8(%rbp) -- invalid mov (both operands are memory)
         * movl $5, -4(%rbp)
         */

        let mut function = AsmFunction {
            name: "example".to_string(),
            global: false,
            external: false,
            instructions: vec![
                AsmInstruction::Mov {
                    typ: AssemblyType::Longword,
                    src: Operand::Stack(-4),
                    dst: Operand::Stack(-8),
                },
                AsmInstruction::Mov {
                    typ: AssemblyType::Longword,
                    src: Operand::Imm(5),
                    dst: Operand::Stack(-4),
                },
            ],
        };

        let program = AsmProgram {
            asm_functions: vec![function],
        };

        // Run the fix instructions pass
        let fixed_program = fix_instructions(program);

        // Now inspect the program
        // We expect 3 instructions total with the invalid mov split into 2 instructions

        // Index 1 and 2 should correspond to rewriting of mov from Stack(-4) to Stack(-8)
        let func = &fixed_program.asm_functions[0];
        println!("{:?}", func.instructions);
        if let AsmInstruction::Mov { src, dst, .. } = &func.instructions[0] {
            // first rewritten instruction: mov -4(%rbp) to %r10
            match (src, dst) {
                (Operand::Stack(s1), Operand::Reg(Register::R10)) => {
                    assert_eq!(*s1, -4);
                }
                _ => panic!("First rewritten mov is incorrect."),
            }
        } else {
            panic!("Expected a mov instruction at index 1.");
        }

        if let AsmInstruction::Mov { src, dst, .. } = &func.instructions[1] {
            // second rewritten instruction: mov %r10 to -8(%rbp)
            match (src, dst) {
                (Operand::Reg(Register::R10), Operand::Stack(s2)) => {
                    assert_eq!(*s2, -8);
                }
                _ => panic!("Second rewritten mov is incorrect."),
            }
        } else {
            panic!("Expected a mov instruction at index 2.");
        }

        // The third instruction should be the valid mov that was unchanged.
        if let AsmInstruction::Mov { src, dst, .. } = &func.instructions[2] {
            match (src, dst) {
                (Operand::Imm(val), Operand::Stack(s)) => {
                    assert_eq!(*val, 5);
                    assert_eq!(*s, -4);
                }
                _ => panic!("Valid mov instruction did not match expected operands."),
            }
        } else {
            panic!("Expected a mov instruction at index 3.");
        }
    }
}
