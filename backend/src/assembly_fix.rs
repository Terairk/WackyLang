// This pass is the last pass currently and will just fix any assembly instructions
// where you have 2 memory operands in a mov instruction. This may need to be changed in the future
// to handle other instructions but for now its mov

/* ================== INVARIANTS =================== */
// We'll use R10D as a scratch register because it usually doesn't serve
// any special purpose

use crate::{
    assembly_ast::{
        AsmBinaryOperator, AsmFunction, AsmInstruction, AsmProgram, AssemblyType,
        Operand, Register,
    },
    predefined::inbuiltOverflow,
};

#[must_use]
#[inline]
pub fn fix_program(program: AsmProgram) -> AsmProgram {
    use Operand::Imm;
    // Process each function separately
    let mut new_functions: Vec<AsmFunction> = Vec::new();

    for func in program.asm_functions {
        let mut new_func_body: Vec<AsmInstruction> = Vec::new();
        for instr in func.instructions {
            match instr {
                AsmInstruction::Mov { typ, src, dst } => {
                    fix_move(&mut new_func_body, typ, src, dst);
                }
                AsmInstruction::Binary {
                    operator,
                    typ,
                    op1,
                    op2,
                } => fix_binary(&mut new_func_body, operator, typ, op1, op2),
                AsmInstruction::Idiv(Imm(val)) => {
                    fix_idiv(&mut new_func_body, val);
                }
                AsmInstruction::Cmp { typ, op1, op2 } => fix_cmp(&mut new_func_body, typ, op1, op2),
                _ => new_func_body.push(instr),
            }
        }

        // TODO: maybe we can use bon crate
        new_functions.push(AsmFunction {
            name: func.name,
            global: func.global,
            instructions: new_func_body,
            directives: vec![],
        });
    }

    AsmProgram {
        asm_functions: new_functions,
    }
}

/* ================== INTERNALS ================== */

fn fix_binary(
    asm: &mut Vec<AsmInstruction>,
    operator: AsmBinaryOperator,
    typ: AssemblyType,
    op1: Operand,
    op2: Operand,
) {
    use AsmBinaryOperator as AsmBinOp;
    match operator {
        AsmBinOp::Add | AsmBinOp::Sub => fix_add_sub(operator, typ, op1, op2, asm),
        AsmBinOp::Mult => fix_mult(typ, op1, op2, asm),
        _ => asm.push(AsmInstruction::Binary {
            operator,
            typ,
            op1,
            op2,
        }),
    }
    asm.push(AsmInstruction::JmpOverflow(inbuiltOverflow.to_owned()));
}

fn fix_move(asm: &mut Vec<AsmInstruction>, typ: AssemblyType, src: Operand, dst: Operand) {
    let new_instrs = match (src.clone(), dst.clone()) {
        (Operand::Stack(_), Operand::Stack(_)) => vec![
            AsmInstruction::Mov {
                typ: typ,
                src,
                dst: Operand::Reg(Register::R10),
            },
            AsmInstruction::Mov {
                typ,
                src: Operand::Reg(Register::R10),
                dst,
            },
        ],
        _ => vec![AsmInstruction::Mov { typ, src, dst }],
    };
    asm.extend(new_instrs);
}

fn fix_add_sub(
    operator: AsmBinaryOperator,
    typ: AssemblyType,
    op1: Operand,
    op2: Operand,
    asm: &mut Vec<AsmInstruction>,
) {
    let new_instrs = match (op1.clone(), op2.clone()) {
        (Operand::Stack(_), Operand::Stack(_)) => vec![
            AsmInstruction::Mov {
                typ: typ.clone(),
                src: op1,
                dst: Operand::Reg(Register::R10),
            },
            AsmInstruction::Binary {
                operator,
                typ,
                op1: Operand::Reg(Register::R10),
                op2,
            },
        ],
        _ => vec![AsmInstruction::Binary {
            operator,
            typ,
            op1,
            op2,
        }],
    };
    asm.extend(new_instrs);
}

fn fix_mult(typ: AssemblyType, op1: Operand, op2: Operand, asm: &mut Vec<AsmInstruction>) {
    let new_instrs = match (op1.clone(), op2.clone()) {
        (_, Operand::Stack(_)) => vec![
            AsmInstruction::Mov {
                typ: typ.clone(),
                src: op2.clone(),
                dst: Operand::Reg(Register::R11),
            },
            AsmInstruction::Binary {
                operator: AsmBinaryOperator::Mult,
                typ: typ.clone(),
                op1,
                op2: Operand::Reg(Register::R11),
            },
            AsmInstruction::Mov {
                typ,
                src: Operand::Reg(Register::R11),
                dst: op2,
            },
        ],
        _ => vec![AsmInstruction::Binary {
            operator: AsmBinaryOperator::Mult,
            typ,
            op1,
            op2,
        }],
    };
    asm.extend(new_instrs);
}

fn fix_idiv(asm: &mut Vec<AsmInstruction>, val: i32) {
    let new_instrs = vec![
        AsmInstruction::Mov {
            typ: AssemblyType::Longword,
            src: Operand::Imm(val),
            dst: Operand::Reg(Register::R10),
        },
        AsmInstruction::Idiv(Operand::Reg(Register::R10)),
    ];
    asm.extend(new_instrs);
}

fn fix_cmp(asm: &mut Vec<AsmInstruction>, typ: AssemblyType, op1: Operand, op2: Operand) {
    let new_instrs = match (op1.clone(), op2.clone()) {
        (Operand::Stack(_), Operand::Stack(_)) => vec![
            AsmInstruction::Mov {
                typ: typ.clone(),
                src: op1,
                dst: Operand::Reg(Register::R10),
            },
            AsmInstruction::Cmp {
                typ,
                op1: Operand::Reg(Register::R10),
                op2,
            },
        ],
        (_, Operand::Imm(_)) => vec![
            AsmInstruction::Mov {
                typ: typ.clone(),
                src: op2,
                dst: Operand::Reg(Register::R11),
            },
            AsmInstruction::Cmp {
                typ,
                op1,
                op2: Operand::Reg(Register::R11),
            },
        ],
        _ => vec![AsmInstruction::Cmp { typ, op1, op2 }],
    };
    asm.extend(new_instrs);
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

        let function = AsmFunction {
            name: "example".to_owned(),
            global: false,
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
            directives: vec![],
        };

        let program = AsmProgram {
            asm_functions: vec![function],
        };

        // Run the fix instructions pass
        let fixed_program = fix_program(program);

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
