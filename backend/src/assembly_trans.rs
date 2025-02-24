use middle::wackir::{
    BinaryOperator, UnaryOperator, WackConst, WackFunction, WackInstruction, WackProgram, WackValue,
};
use util::gen_flags::{GenFlags, insert_flag_gbl};

use crate::{
    assembly_ast::{
        AsmBinaryOperator, AsmFunction, AsmInstruction, AsmProgram, AsmUnaryOperator, AssemblyType,
        CondCode, Operand, Register,
    },
    gen_predefined::ERR_DIVZERO,
};

/* ================== PUBLIC API ================== */

#[inline]
#[must_use]
pub fn wacky_to_assembly(program: WackProgram, counter: usize) -> (AsmProgram, AsmGen) {
    let mut asm_gen = AsmGen::new(counter);
    let mut asm_functions: Vec<AsmFunction> = Vec::new();
    asm_functions.push(asm_gen.lower_main_asm(program.main_body));
    for wack_function in program.functions {
        asm_functions.push(asm_gen.lower_function(wack_function));
    }

    (AsmProgram { asm_functions }, asm_gen)
}

/* ================== INTERNAL API ================== */

pub struct AsmGen {
    pub counter: usize,
    // TODO: add counter for string literals
    // and a table to store these literals with their lengths
    // we need to mangle them as well
    // also maybe keep track of RIP relative addressing

    // Probably need a backend symbol table
    // honestly we probably don't need a symbol table for functions
    // as we dont have external functions other than the ones used by
    // Wacc's standard statements like read, print etc
    // So we automatically know if we should use PLT or not
}

impl AsmGen {
    const fn new(counter: usize) -> Self {
        Self { counter }
    }

    fn lower_main_asm(&mut self, instrs: Vec<WackInstruction>) -> AsmFunction {
        let mut asm_instructions = Vec::new();
        for wack_instr in instrs {
            self.lower_instruction(wack_instr, &mut asm_instructions);
        }

        asm_instructions.push(AsmInstruction::Ret);

        AsmFunction {
            name: "main".to_owned(),
            global: true,
            instructions: asm_instructions,
        }
    }

    fn lower_function(&mut self, wack_function: WackFunction) -> AsmFunction {
        let mut asm = Vec::new();
        let func_name: String = (*wack_function.name).to_owned();
        for instr in wack_function.body {
            self.lower_instruction(instr, &mut asm);
        }

        AsmFunction {
            name: func_name,
            global: false,
            instructions: asm,
        }
    }

    fn lower_instruction(
        &mut self,
        instr: WackInstruction,
        asm_instructions: &mut Vec<AsmInstruction>,
    ) {
        use AsmInstruction as Asm;
        use WackInstruction::{Binary, Return, Unary};
        // TODO: finish this scaffolding
        match instr {
            Return(value) => self.lower_return(value, asm_instructions),
            Unary { op, src, dst } => self.lower_unary(&op, src, dst, asm_instructions),
            Binary {
                op,
                src1,
                src2,
                dst,
            } => self.lower_binary(&op, src1, src2, dst, asm_instructions),
            _ => unimplemented!(),
        }
    }

    fn lower_return(&mut self, value: WackValue, asm_instructions: &mut Vec<AsmInstruction>) {
        use AsmInstruction as Asm;
        let operand = self.lower_value(value, asm_instructions);

        // TODO: most of these arms aren't correct apart from Imm
        match operand {
            Operand::Imm(_) => asm_instructions.push(Asm::Mov {
                typ: AssemblyType::Longword,
                src: operand,
                dst: Operand::Reg(Register::AX),
            }),
            Operand::Reg(_) => asm_instructions.push(Asm::Mov {
                typ: AssemblyType::Quadword,
                src: operand,
                dst: Operand::Reg(Register::AX),
            }),
            Operand::Pseudo(_) => asm_instructions.push(Asm::Mov {
                typ: AssemblyType::Quadword,
                src: operand,
                dst: Operand::Reg(Register::AX),
            }),
            Operand::Memory(_, _) => asm_instructions.push(Asm::Mov {
                typ: AssemblyType::Quadword,
                src: operand,
                dst: Operand::Reg(Register::AX),
            }),
            Operand::Data(_, _) => asm_instructions.push(Asm::Mov {
                typ: AssemblyType::Quadword,
                src: operand,
                dst: Operand::Reg(Register::AX),
            }),
            Operand::PseudoMem(_, _) => asm_instructions.push(Asm::Mov {
                typ: AssemblyType::Quadword,
                src: operand,
                dst: Operand::Reg(Register::AX),
            }),
            Operand::Indexed { .. } => unimplemented!(),
            Operand::Stack(_) => unimplemented!(),
        }

        asm_instructions.push(Asm::Ret);
    }

    fn lower_unary(
        &mut self,
        op: &UnaryOperator,
        src: WackValue,
        dst: WackValue,
        asm: &mut Vec<AsmInstruction>,
    ) {
        use AsmInstruction as Asm;
        // TODO: check if this is what I need to do
        let src_operand = self.lower_value(src, asm);
        let dst_operand = self.lower_value(dst, asm);

        // Need to figure out how to get src_type and dst_type
        // for now treat everything as an Int cus its easier for me
        // TODO: change this to actually get the correct type
        let op = op.clone();
        #[allow(clippy::single_match_else)]
        match op {
            UnaryOperator::Not => {
                asm.push(Asm::Cmp {
                    typ: AssemblyType::Longword,
                    op1: Operand::Imm(0),
                    op2: src_operand,
                });
                asm.push(Asm::Mov {
                    typ: AssemblyType::Longword,
                    src: Operand::Imm(0),
                    dst: dst_operand.clone(),
                });
                asm.push(Asm::SetCC {
                    condition: CondCode::E,
                    operand: dst_operand,
                });
            }
            _ => {
                asm.push(Asm::Mov {
                    typ: AssemblyType::Longword,
                    src: src_operand,
                    dst: dst_operand.clone(),
                });
                asm.push(Asm::Unary {
                    operator: op.into(),
                    typ: AssemblyType::Longword,
                    operand: dst_operand,
                });
            }
        }
    }

    fn lower_binary(
        &mut self,
        op: &BinaryOperator,
        src1: WackValue,
        src2: WackValue,
        dst: WackValue,
        asm: &mut Vec<AsmInstruction>,
    ) {
        use AsmInstruction as Asm;
        use BinaryOperator as BinOp;

        let src1_operand = self.lower_value(src1, asm);
        let src2_operand = self.lower_value(src2, asm);
        let dst_operand = self.lower_value(dst, asm);

        // We handle Div and Remainder operations differently
        // than the rest since it has specific semantics in x86-64
        match *op {
            BinOp::Div | BinOp::Mod => {
                insert_flag_gbl(GenFlags::DIV_BY_ZERO);
                // We need to sign extend EAX into EAX:EDX
                // Handle div by zero here
                asm.push(Asm::Cmp {
                    typ: AssemblyType::Longword,
                    op1: Operand::Imm(0),
                    op2: src2_operand.clone(),
                });
                asm.push(Asm::JmpCC {
                    condition: CondCode::E,
                    label: ERR_DIVZERO.to_owned(),
                });

                asm.push(Asm::Mov {
                    typ: AssemblyType::Longword,
                    src: src1_operand,
                    dst: Operand::Reg(Register::AX),
                });
                asm.push(Asm::Cdq);
                asm.push(Asm::Idiv(src2_operand));
                // Result is stored in different registers
                // Depending on operation
                let dst_reg = match *op {
                    BinOp::Div => Operand::Reg(Register::AX),
                    BinOp::Mod => Operand::Reg(Register::DX),
                    _ => unreachable!(),
                };
                asm.push(Asm::Mov {
                    typ: AssemblyType::Longword,
                    src: dst_reg,
                    dst: dst_operand,
                });
            }
            _ => {
                // Handle other binary operations in the same way
                asm.push(Asm::Mov {
                    typ: AssemblyType::Longword,
                    src: src1_operand,
                    dst: dst_operand.clone(),
                });
                let asm_op = convert_arith_binop(op.clone());
                // For now its binary operations so insert flag here
                insert_flag_gbl(GenFlags::OVERFLOW);
                asm.push(Asm::Binary {
                    operator: asm_op,
                    typ: AssemblyType::Longword,
                    op1: src2_operand,
                    op2: dst_operand,
                });
            }
        }
    }

    // This lowers a WackValue to an Asm Operand
    fn lower_value(
        &mut self,
        value: WackValue,
        _asm_instructions: &mut Vec<AsmInstruction>,
    ) -> Operand {
        use WackConst::{Bool, Char, Int, NullPair, StringLit};
        use WackValue::{Constant, Var};
        match value {
            Constant(Int(i)) => Operand::Imm(i),
            Constant(Bool(b)) => Operand::Imm(b),
            Constant(Char(c)) => Operand::Imm(c),
            // TODO: StringLit, need to add to symbol table with current function probably
            // while keeping track of a unique counter just for string constants
            // so we can emit properly, also this should emit a thing for RIP relative addressing
            // honestly its kinda similar to ConstDouble from the book
            Constant(StringLit(s)) => unimplemented!(),
            Constant(NullPair) => Operand::Imm(0),
            // TODO: need to figure out if its a Scalar or Aggregate Value
            // so I can do either Pseudo or PseudoMem, for now its Pseudo
            Var(ident) => Operand::Pseudo(ident.into()),
        }
    }
}

fn convert_arith_binop(wacky_binop: BinaryOperator) -> AsmBinaryOperator {
    use AsmBinaryOperator as AsmBinOp;
    use BinaryOperator as BinOp;

    match wacky_binop {
        BinOp::Add => AsmBinOp::Add,
        BinOp::Sub => AsmBinOp::Sub,
        BinOp::Mul => AsmBinOp::Mult,
        _ => panic!("Invalid binary arithmetic operator for Asm"),
    }
}
