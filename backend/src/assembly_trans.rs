use std::collections::{BTreeMap, HashMap};
use middle::wackir::{
    BinaryOperator, UnaryOperator, WackConst, WackFunction, WackInstruction, WackProgram, WackValue,
};
use util::gen_flags::{GenFlags, insert_flag_gbl};
use syntax::types::SemanticType;
use crate::{
    assembly_ast::{
        AsmBinaryOperator, AsmFunction, AsmInstruction, AsmProgram, AsmUnaryOperator, AssemblyType,
        CondCode, Operand, Register,
    },
    gen_predefined::ERR_DIVZERO,
};
use crate::assembly_ast::AssemblyType::Longword;
use crate::assembly_ast::Register::DI;
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
    pub str_counter: usize,
    pub str_literals: BTreeMap<String, String>,
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
    fn new(counter: usize) -> Self {
        Self { counter, str_counter: 0, str_literals: BTreeMap::new() }
    }

    fn generate_str_label(&mut self, s: &str) -> String {
        if let Some(label) = self.str_literals.get(s) {
            return label.clone(); // Reuse existing label
        }

        let label = format!(".L.str{}", self.str_counter);
        self.str_counter += 1;
        self.str_literals.insert(s.to_string(), label.clone());
        label
    }

    fn lower_main_asm(&mut self, instrs: Vec<WackInstruction>) -> AsmFunction {
        let mut asm_instructions = Vec::new();
        for wack_instr in instrs {
            self.lower_instruction(wack_instr, &mut asm_instructions);
        }

        asm_instructions.push(AsmInstruction::Ret);

        // any functions we generate ourselves are not external
        AsmFunction {
            name: "main".to_owned(),
            global: true,
            external: false,
            instructions: asm_instructions,
        }
    }

    fn lower_function(&mut self, wack_function: WackFunction) -> AsmFunction {
        // TODO: when we move the pushing to this phase instead of emission
        // add the push and mov instructions for rbp and rsp
        use AsmInstruction as Asm;
        use Register::{CX, DI, DX, R8, R9, SI};
        let mut asm = Vec::new();
        let func_name: String = wack_function.name.into();
        let params = wack_function.params;

        // Handle parameters finally
        // Pair up the parameters and registers for the first 6 parameters.

        asm.push(Asm::Comment(
            "Push registers onto stack to prevent clobbering".to_owned(),
        ));
        let arg_regs = [DI, SI, DX, CX, R8, R9];
        for (param, reg) in params.iter().zip(arg_regs.iter()) {
            let wack_value = WackValue::Var(param.clone());
            let param_name = self.lower_value(wack_value, &mut asm);
            asm.push(AsmInstruction::Mov {
                typ: AssemblyType::Longword,
                src: Operand::Reg(*reg),
                dst: param_name,
            });
        }

        // if there are extra parameters beyond the 6 registers
        // move them from the stack into the parameters
        asm.push(Asm::Comment(
            "Move remaining parameters from stack into our stack frame".to_owned(),
        ));
        let mut stack_index = 0;
        for param in params.iter().skip(arg_regs.len()).rev() {
            let wack_value = WackValue::Var(param.clone());
            let param_name = self.lower_value(wack_value, &mut asm);
            asm.push(AsmInstruction::Mov {
                typ: AssemblyType::Longword,
                src: Operand::Stack(16 + stack_index),
                dst: param_name,
            });
            stack_index += 8;
        }

        for instr in wack_function.body {
            self.lower_instruction(instr, &mut asm);
        }

        // any functions we generate ourselves are not external
        AsmFunction {
            name: func_name,
            global: false,
            external: false,
            instructions: asm,
        }
    }

    fn lower_print(&mut self, src: WackValue, ty: SemanticType, asm: &mut Vec<AsmInstruction>) {
        let operand = self.lower_value(src, asm);
        let func_name = match ty {
            SemanticType::Int => {
                asm.push(AsmInstruction::Mov {
                    typ: Longword,
                    src: operand,
                    dst: Operand::Reg(DI)
                });
                insert_flag_gbl(GenFlags::PRINT_INT);
                "_printi"
            },
            SemanticType::Bool => {
                asm.push(AsmInstruction::Mov {
                    typ: AssemblyType::Byte,
                    src: operand,
                    dst: Operand::Reg(DI)
                });
                insert_flag_gbl(GenFlags::PRINT_BOOLEAN);
                "_printb"
            },
            SemanticType::Char => {
                asm.push(AsmInstruction::Mov {
                    typ: AssemblyType::Byte,
                    src: operand,
                    dst: Operand::Reg(DI)
                });
                insert_flag_gbl(GenFlags::PRINT_CHR);
                "_printc"
            },
            SemanticType::String => {
                asm.push(AsmInstruction::Lea {
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_STR);
                "_prints"
            },
            SemanticType::Array(_) | SemanticType::Pair(_, _) | SemanticType::ErasedPair => {
                asm.push(AsmInstruction::Lea {
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_PTR);
                "_printp"
            },
            SemanticType::AnyType => panic!("AnyType in print"),
            SemanticType::Error(_) => panic!("Error type in print"),
        };
        asm.push(AsmInstruction::Call(func_name.to_string(), false));
    }

    fn lower_instruction(&mut self, instr: WackInstruction, asm: &mut Vec<AsmInstruction>) {
        use AsmInstruction as Asm;
        use WackInstruction::{
            Binary, Copy, FunCall, Jump, JumpIfNotZero, JumpIfZero, Label, Return, Unary,
        };
        // TODO: finish this scaffolding
        match instr {
            Return(value) => self.lower_return(value, asm),
            Unary { op, src, dst } => self.lower_unary(&op, src, dst, asm),
            Binary {
                op,
                src1,
                src2,
                dst,
            } => self.lower_binary(&op, src1, src2, dst, asm),
            Jump(target) => asm.push(Asm::Jmp(target.into())),
            JumpIfZero { condition, target } => {
                // TODO: experiment with AssemblyType::Byte and Longword
                let condition = self.lower_value(condition, asm);
                asm.push(Asm::Cmp {
                    typ: AssemblyType::Longword,
                    op1: Operand::Imm(0),
                    op2: condition,
                });
                asm.push(Asm::JmpCC {
                    condition: CondCode::E,
                    label: target.into(),
                });
            }
            JumpIfNotZero { condition, target } => {
                let condition = self.lower_value(condition, asm);
                asm.push(Asm::Cmp {
                    typ: AssemblyType::Longword,
                    op1: Operand::Imm(0),
                    op2: condition,
                });
                asm.push(Asm::JmpCC {
                    condition: CondCode::NE,
                    label: target.into(),
                });
            }
            Copy { src, dst } => {
                let src_operand = self.lower_value(src, asm);
                let dst_operand = self.lower_value(dst, asm);
                asm.push(Asm::Mov {
                    typ: AssemblyType::Longword,
                    src: src_operand,
                    dst: dst_operand,
                });
            }
            Label(id) => asm.push(Asm::Label(id.into())),
            FunCall {
                fun_name,
                args,
                dst,
            } => self.lower_fun_call(&fun_name, &args, dst, asm),
            WackInstruction::Print {
                src: src,
                ty: ty,
            } => {
                self.lower_print(src, ty, asm)
            }
            WackInstruction::Println {
                src: src,
                ty: ty,
            } => {
                self.lower_print(src, ty, asm);
                insert_flag_gbl(GenFlags::PRINT_LN);
                asm.push(AsmInstruction::Call("_println".to_string(), false));
            }
            WackInstruction::Exit(value) => {
                todo!("add GenFlag for EXIT");
                // insert_flag_gbl(GenFlags::EXIT);
                let operand = self.lower_value(value, asm);
                asm.push(AsmInstruction::Mov {typ: Longword, src: operand, dst: Operand::Reg(DI) });
                asm.push(AsmInstruction::Call("_exit".to_string(), false));
            }
            _ => unimplemented!(),
        }
    }

    fn lower_fun_call(
        &mut self,
        fun_name: &str,
        args: &[WackValue],
        dst: WackValue,
        asm: &mut Vec<AsmInstruction>,
    ) {
        use AsmInstruction as Asm;
        use Operand::{Imm, Reg};
        use Register::{AX, CX, DI, DX, R8, R9, SI};
        let arg_regs = [DI, SI, DX, CX, R8, R9];

        // adjust stack alignment

        // Determine the split index (at most 6 arguments for register_args)
        let split_index = args.len().min(arg_regs.len());

        // Split the slice into register_args and stack_args
        let (register_args, rest) = args.split_at(split_index);
        let stack_args: Option<&[WackValue]> = if rest.is_empty() { None } else { Some(rest) };

        // register_args is a slice of WackValues up to 6 values
        // stack_args is an Option of a slice of WackValues
        let stack_padding = if stack_args.iter().len() % 2 != 0 {
            8
        } else {
            0
        };

        // used to align the stack to 16 bytes for function calls
        asm.push(Asm::Comment("This aligns stack to 16 bytes".to_owned()));
        if stack_padding != 0 {
            asm.push(Asm::AllocateStack(stack_padding));
        }

        // pass args in registers
        for (reg_index, tacky_arg) in register_args.iter().enumerate() {
            let r = arg_regs[reg_index];
            let assembly_arg = self.lower_value(tacky_arg.clone(), asm);
            asm.push(Asm::Mov {
                typ: AssemblyType::Longword,
                src: assembly_arg,
                dst: Operand::Reg(r),
            });
        }

        // pass rest of args on stack
        // Note that the SystemV ABI requires that the stack is 16-byte aligned
        // and that each argument is 8-byte aligned
        // it might not matter now but it will matter if we ever link with C code
        // Note there is an edge case which we don't handle rn which is when we push
        // a 4-byte memory operand and those 4 bytes after it aren't readable memory

        if let Some(stack_args) = stack_args {
            asm.push(Asm::Comment(
                "Pushing arguments to stack in reverse order".to_owned(),
            ));
            for tacky_arg in stack_args {
                let assembly_arg: Operand = self.lower_value(tacky_arg.clone(), asm);
                let new_instrs: Vec<AsmInstruction> = match assembly_arg {
                    Imm(_) | Reg(_) => vec![Asm::Push(assembly_arg)],
                    _ => vec![
                        // Use register AX here as temporary storage
                        // Note this is the only register that can be used here
                        // We can't use callee saved registers here
                        Asm::Mov {
                            typ: AssemblyType::Longword,
                            src: assembly_arg,
                            dst: Operand::Reg(AX),
                        },
                        Asm::Push(Reg(AX)),
                    ],
                };
                asm.extend(new_instrs);
            }
        }

        // Any function call that the user makes will only call
        // other wacc functions which are all internal, hence external flag is false
        asm.push(Asm::Call(fun_name.into(), false));

        // adjust stack pointer
        #[allow(clippy::cast_possible_truncation)]
        let bytes_to_remove: i32 = 8 * stack_args.map_or(0, |args| args.len()) as i32;
        if bytes_to_remove != 0 {
            asm.push(Asm::DeallocateStack(bytes_to_remove));
        }

        // handle return value
        let assembly_dst = self.lower_value(dst, asm);
        asm.push(Asm::Mov {
            typ: AssemblyType::Longword,
            src: Reg(AX),
            dst: assembly_dst,
        });
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
        use BinaryOperator::*;

        let src1_operand = self.lower_value(src1, asm);
        let src2_operand = self.lower_value(src2, asm);
        let dst_operand = self.lower_value(dst, asm);

        // We handle Div and Remainder operations differently
        // than the rest since it has specific semantics in x86-64
        match *op {
            Div | Mod => {
                insert_flag_gbl(GenFlags::DIV_BY_ZERO);
                // We need to sign extend EAX into EAX:EDX
                // Handle div by zero here

                let new_instrs = vec![
                    Asm::Cmp {
                        typ: AssemblyType::Longword,
                        op1: src2_operand.clone(),
                        op2: Operand::Imm(0),
                    },
                    Asm::JmpCC {
                        condition: CondCode::E,
                        label: ERR_DIVZERO.to_owned(),
                    },
                    Asm::Mov {
                        typ: AssemblyType::Longword,
                        src: src1_operand,
                        dst: Operand::Reg(Register::AX),
                    },
                    Asm::Cdq,
                    Asm::Idiv(src2_operand),
                ];
                // Result is stored in different registers
                // Depending on operation
                let dst_reg = match *op {
                    Div => Operand::Reg(Register::AX),
                    Mod => Operand::Reg(Register::DX),
                    _ => unreachable!(),
                };
                asm.extend(new_instrs);
                asm.push(Asm::Mov {
                    typ: AssemblyType::Longword,
                    src: dst_reg,
                    dst: dst_operand,
                });
            }
            Mul | Add | Sub => {
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
            Gt | Gte | Lt | Lte | Eq | Neq => {
                let new_instrs = vec![
                    Asm::Cmp {
                        typ: AssemblyType::Longword,
                        op1: src1_operand,
                        op2: src2_operand,
                    },
                    Asm::Mov {
                        typ: AssemblyType::Longword,
                        src: Operand::Imm(0),
                        dst: dst_operand.clone(),
                    },
                    Asm::SetCC {
                        condition: convert_code(op.clone()),
                        operand: dst_operand,
                    },
                ];
                asm.extend(new_instrs);
            }
            _ => unimplemented!(),
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
            Constant(StringLit(s)) => {
                let label = self.generate_str_label(&*s);
                Operand::Data(label, 0)
            },
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

fn convert_code(code: BinaryOperator) -> CondCode {
    use BinaryOperator::*;
    use CondCode as CC;

    match code {
        Gt => CC::G,
        Gte => CC::GE,
        Lt => CC::L,
        Lte => CC::LE,
        Eq => CC::E,
        Neq => CC::NE,
        _ => panic!("Invalid binary comparison operator for Asm"),
    }
}
