// This pass is the last pass currently and will just fix any assembly instructions
// where you have 2 memory operands in a mov instruction. This may need to be changed in the future
// to handle other instructions but for now its mov

/* ================== INVARIANTS =================== */
// We'll use R10 and R11 as a scratch register because it usually doesn't serve
// any special purpose. Consequently we don't use R10 and R11 in our interference graph

use crate::assembly_ast::AsmInstruction::{
    Binary, Cmp, Idiv, JmpCC, Lea, Mov, MovZeroExtend, Pop, Ret,
};
use crate::assembly_ast::Operand::{Data, Memory, Reg};
use crate::assembly_ast::Register::{BP, R10, R11, SP};
use crate::assembly_ast::{
    AsmBinaryOperator, AsmFunction, AsmInstruction, AsmProgram, AssemblyType, Operand, Operand::Imm,
};
use crate::assembly_ast::{AsmLabel, CondCode};
use crate::predefined::GEN_USIZE;
use crate::regalloc::FunctionCallee;
use AssemblyType::Quadword;
use util::gen_flags::INBUILT_OVERFLOW;

///
/// Fix the assembly instructions in the program to ensure they are valid x86-64 instructions.
/// Some instructions have special requirements such as needing a register as 2nd operand
#[must_use]
#[inline]
pub fn fix_program(program: AsmProgram, func_callee_regs: &FunctionCallee) -> AsmProgram {
    use Imm;
    // Process each function separately
    let mut new_functions: Vec<AsmFunction> = Vec::new();

    for func in program.asm_functions {
        let mut new_func_body: Vec<AsmInstruction> = Vec::new();
        for instr in func.instructions {
            match instr {
                Mov { typ, src, dst } => {
                    fix_move(&mut new_func_body, typ, src, dst);
                }
                MovZeroExtend {
                    src_type,
                    dst_type,
                    src,
                    dst,
                } => {
                    fix_zero_extend(&mut new_func_body, src_type, dst_type, src, dst);
                }
                Binary {
                    operator,
                    typ,
                    op1,
                    op2,
                } => fix_binary(&mut new_func_body, operator, typ, op1, op2),
                Idiv(Imm(val)) => {
                    fix_idiv(&mut new_func_body, val);
                }
                Cmp { typ, op1, op2 } => fix_cmp(&mut new_func_body, typ, op1, op2),
                Lea { src, dst } => {
                    fix_lea(&mut new_func_body, src, dst);
                }
                Ret => {
                    // If we have a return instruction, we need to pop the callee saved registers
                    // before returning, we don't do this earlier because its more efficient to
                    // insert all at once versus at the end and simplifies the code, ie more
                    // efficient to insert at the last index versus last index - 1

                    for reg in func_callee_regs.get(&func.name).unwrap().iter().rev() {
                        new_func_body.push(Pop(*reg));
                    }
                    new_func_body.push(Mov {
                        typ: Quadword,
                        src: Reg(BP),
                        dst: Reg(SP),
                    });
                    new_func_body.push(Pop(BP));

                    new_func_body.push(Ret);
                }
                // All other instructions are left unchanged as they dont have special requirements
                _ => new_func_body.push(instr),
            }
        }

        new_functions.push(AsmFunction {
            name: func.name,
            global: func.global,
            instructions: new_func_body,
            directives: vec![],
            regs: func.regs,
        });
    }

    AsmProgram {
        asm_functions: new_functions,
    }
}

fn fix_zero_extend(
    new_func_body: &mut Vec<AsmInstruction>,
    src_type: AssemblyType,
    dst_type: AssemblyType,
    src: Operand,
    dst: Operand,
) {
    if src_type == AssemblyType::Byte {
        let new_instrs = match (src.clone(), dst.clone()) {
            // Zero extend can't have Immediate as source
            // so we move it into a register first
            (Imm(_), _) => vec![
                Mov {
                    typ: src_type,
                    src,
                    dst: Reg(R10),
                },
                MovZeroExtend {
                    src_type,
                    dst_type,
                    src: Reg(R10),
                    dst: Reg(R11),
                },
                Mov {
                    typ: dst_type,
                    src: Reg(R11),
                    dst,
                },
            ],
            (_, Memory(_, _)) => vec![
                MovZeroExtend {
                    src_type,
                    dst_type,
                    src,
                    dst: Reg(R10),
                },
                Mov {
                    typ: dst_type,
                    src: Reg(R10),
                    dst,
                },
            ],
            _ => vec![MovZeroExtend {
                src_type,
                dst_type,
                src,
                dst,
            }],
        };
        new_func_body.extend(new_instrs);
    } else {
        panic!("Zero extend currently only works for byte types");
    }
}

/* ================== INTERNALS ================== */

// NOTE: we insert an overflow check here, with optimizations enabled, we never actually
// make an add instruction so we don't need to worry about this and WackIR instructions
// interfering
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
        _ => asm.push(Binary {
            operator,
            typ,
            op1,
            op2,
        }),
    }
    asm.push(JmpCC {
        condition: CondCode::OF,
        label: AsmLabel::new(INBUILT_OVERFLOW, GEN_USIZE),
        is_func: true,
    });
}

fn fix_move(asm: &mut Vec<AsmInstruction>, typ: AssemblyType, src: Operand, dst: Operand) {
    let new_instrs = match (src.clone(), dst.clone()) {
        (Memory(_, _) | Data(_, _), Memory(_, _)) => vec![
            Mov {
                typ,
                src,
                dst: Reg(R10),
            },
            Mov {
                typ,
                src: Reg(R10),
                dst,
            },
        ],
        _ => vec![Mov { typ, src, dst }],
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
        (Memory(_, _), Memory(_, _)) => vec![
            Mov {
                typ,
                src: op1,
                dst: Reg(R10),
            },
            Binary {
                operator,
                typ,
                op1: Reg(R10),
                op2,
            },
        ],
        _ => vec![Binary {
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
        (_, Memory(_, _)) => vec![
            Mov {
                typ,
                src: op2.clone(),
                dst: Reg(R11),
            },
            Binary {
                operator: AsmBinaryOperator::Mult,
                typ,
                op1,
                op2: Reg(R11),
            },
            Mov {
                typ,
                src: Reg(R11),
                dst: op2,
            },
        ],
        _ => vec![Binary {
            operator: AsmBinaryOperator::Mult,
            typ,
            op1,
            op2,
        }],
    };
    asm.extend(new_instrs);
}

// Can't perform IDiv with immediate value so move into a
// register first
fn fix_idiv(asm: &mut Vec<AsmInstruction>, val: i32) {
    let new_instrs = vec![
        Mov {
            typ: AssemblyType::Longword,
            src: Imm(val),
            dst: Reg(R10),
        },
        Idiv(Reg(R10)),
    ];
    asm.extend(new_instrs);
}

fn fix_cmp(asm: &mut Vec<AsmInstruction>, typ: AssemblyType, op1: Operand, op2: Operand) {
    let new_instrs = match (op1.clone(), op2.clone()) {
        (Memory(_, _), Memory(_, _)) => vec![
            Mov {
                typ,
                src: op1,
                dst: Reg(R10),
            },
            Cmp {
                typ,
                op1: Reg(R10),
                op2,
            },
        ],
        (_, Imm(_)) => vec![
            Mov {
                typ,
                src: op2,
                dst: Reg(R11),
            },
            Cmp {
                typ,
                op1,
                op2: Reg(R11),
            },
        ],
        _ => vec![Cmp { typ, op1, op2 }],
    };
    asm.extend(new_instrs);
}

fn fix_lea(asm: &mut Vec<AsmInstruction>, src: Operand, dst: Operand) {
    let new_instrs = match (src.clone(), dst.clone()) {
        // Lea can't have memory as destination, must be register
        (_, Memory(_, _)) => vec![
            Lea { src, dst: Reg(R10) },
            Mov {
                typ: AssemblyType::Quadword,
                src: Reg(R10),
                dst,
            },
        ],
        _ => vec![Lea { src, dst }],
    };
    asm.extend(new_instrs);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        assembly_ast::Register::{BP, R10},
        registers::RegisterSet,
    };

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
                Mov {
                    typ: AssemblyType::Longword,
                    src: Memory(BP, -4),
                    dst: Memory(BP, -8),
                },
                Mov {
                    typ: AssemblyType::Longword,
                    src: Imm(5),
                    dst: Memory(BP, -4),
                },
            ],
            directives: vec![],
            regs: RegisterSet::empty(),
        };

        let program = AsmProgram {
            asm_functions: vec![function],
        };

        // Run the fix instructions pass
        let fixed_program = fix_program(program, &FunctionCallee::new());

        // Now inspect the program
        // We expect 3 instructions total with the invalid mov split into 2 instructions

        // Index 1 and 2 should correspond to rewriting of mov from Stack(-4) to Stack(-8)
        let func = &fixed_program.asm_functions[0];
        if let Mov { src, dst, .. } = &func.instructions[0] {
            // first rewritten instruction: mov -4(%rbp) to %r10
            println!("1st instruction, src: {:?}, dst: {:?}", src, dst);
            match (src, dst) {
                (Memory(BP, s1), Reg(R10)) => {
                    assert_eq!(*s1, -4);
                }
                _ => panic!("First rewritten mov is incorrect."),
            }
        } else {
            panic!("Expected a mov instruction at index 1.");
        }

        if let Mov { src, dst, .. } = &func.instructions[1] {
            // second rewritten instruction: mov %r10 to -8(%rbp)

            println!("2nd instruction, src: {:?}, dst: {:?}", src, dst);
            match (src, dst) {
                (Reg(R10), Memory(BP, s2)) => {
                    assert_eq!(*s2, -8);
                }
                _ => panic!("Second rewritten mov is incorrect."),
            }
        } else {
            panic!("Expected a mov instruction at index 2.");
        }

        // The third instruction should be the valid mov that was unchanged.
        if let Mov { src, dst, .. } = &func.instructions[2] {
            println!("3rd instruction {:?}, {:?}", src, dst);
            match (src, dst) {
                (Imm(val), Memory(BP, s)) => {
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
