use crate::assembly_ast::AssemblyType::Longword;
use crate::assembly_ast::Register::{AX, DI};
use crate::predefined::{
    inbuiltExit, inbuiltFree, inbuiltFreePair, inbuiltMalloc, inbuiltPrintBool, inbuiltPrintChar,
    inbuiltPrintInt, inbuiltPrintPtr, inbuiltPrintString, inbuiltPrintln, inbuiltReadChar,
    inbuiltReadInt,
};
use crate::{
    assembly_ast::{
        AsmBinaryOperator, AsmFunction, AsmInstruction, AsmProgram, AssemblyType, CondCode,
        Operand, Register,
    },
    gen_predefined::ERR_DIVZERO,
};
use middle::wackir::{
    BinaryOp, UnaryOp, WackFunction, WackInstr, WackLiteral, WackPrintType, WackProgram,
    WackReadType, WackTempIdent, WackValue,
};
use std::collections::BTreeMap;
use util::gen_flags::{insert_flag_gbl, GenFlags};
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
        Self {
            counter,
            str_counter: 0,
            str_literals: BTreeMap::new(),
        }
    }

    fn generate_str_label(&mut self, s: &str) -> String {
        if let Some(label) = self.str_literals.get(s) {
            return label.clone(); // Reuse existing label
        }

        let label = format!("str{}", self.str_counter);
        self.str_counter += 1;
        self.str_literals.insert(s.to_owned(), label.clone());
        label
    }

    fn lower_main_asm(&mut self, instrs: Vec<WackInstr>) -> AsmFunction {
        use AsmInstruction::{Mov, Pop, Push, Ret};
        use AssemblyType::Quadword;
        use Operand::Reg;
        use Register::{BP, SP};
        let mut asm_instructions = Vec::new();
        asm_instructions.push(Push(Reg(BP)));
        asm_instructions.push(Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        });
        for wack_instr in instrs {
            self.lower_instruction(wack_instr, &mut asm_instructions);
        }

        asm_instructions.push(Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        });
        asm_instructions.push(Pop(Reg(BP)));
        asm_instructions.push(Ret);

        // any functions we generate ourselves are not external
        AsmFunction {
            name: "main".to_owned(),
            global: true,
            instructions: asm_instructions,
            directives: vec![],
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

        asm.push(Asm::Push(Operand::Reg(Register::BP)));
        asm.push(Asm::Mov {
            typ: AssemblyType::Quadword,
            src: Operand::Reg(Register::SP),
            dst: Operand::Reg(Register::BP),
        });

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
            instructions: asm,
            directives: vec![],
        }
    }

    /// TODO: Fix bug with printing null pointer
    /// Lowering value doesn't differentiate between null pointer and 0
    /// So, we should add special value to Operand
    fn lower_print(&mut self, src: WackValue, ty: WackPrintType, asm: &mut Vec<AsmInstruction>) {
        let operand = self.lower_value(src, asm);
        let func_name = match ty {
            WackPrintType::Int => {
                asm.push(AsmInstruction::Mov {
                    typ: Longword,
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_INT);
                inbuiltPrintInt.to_owned()
            }
            WackPrintType::Bool => {
                asm.push(AsmInstruction::Mov {
                    typ: AssemblyType::Byte,
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_BOOLEAN);
                inbuiltPrintBool.to_owned()
            }
            WackPrintType::Char => {
                asm.push(AsmInstruction::Mov {
                    typ: AssemblyType::Byte,
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_CHR);
                inbuiltPrintChar.to_owned()
            }
            WackPrintType::StringOrCharArray => {
                asm.push(AsmInstruction::Lea {
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_STR);
                inbuiltPrintString.to_owned()
            }
            WackPrintType::OtherArray | WackPrintType::Pair => {
                asm.push(AsmInstruction::Lea {
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_PTR);
                inbuiltPrintPtr.to_owned()

                //TODO: does this print `nil` for null pointers??
            }
        };
        asm.push(AsmInstruction::Call(func_name, false));
    }

    #[allow(clippy::too_many_lines)]
    fn lower_instruction(&mut self, instr: WackInstr, asm: &mut Vec<AsmInstruction>) {
        use AsmInstruction as Asm;
        use AssemblyType::{Longword, Quadword};
        use WackInstr::{
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
                let dst_operand = self.lower_value(WackValue::Var(dst), asm);
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
            } => self.lower_fun_call((&fun_name).into(), &args, dst, asm),
            WackInstr::Print { src, ty } => self.lower_print(src, ty, asm),
            WackInstr::Println { src, ty } => {
                self.lower_print(src, ty, asm);
                insert_flag_gbl(GenFlags::PRINT_LN);
                asm.push(AsmInstruction::Call(inbuiltPrintln.to_owned(), false));
            }
            WackInstr::Exit(value) => {
                // TODO: double check this works, this might be wrong
                insert_flag_gbl(GenFlags::EXIT);
                let operand = self.lower_value(value, asm);
                asm.push(AsmInstruction::Mov {
                    typ: Longword,
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                asm.push(AsmInstruction::Call(inbuiltExit.to_owned(), false));
            }
            WackInstr::Read { dst, ty } => {
                let operand = self.lower_value(WackValue::Var(dst), asm);
                asm.push(AsmInstruction::Mov {
                    typ: Longword,
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                match ty {
                    WackReadType::Int => {
                        insert_flag_gbl(GenFlags::READ_INT);
                        asm.push(AsmInstruction::Call(inbuiltReadInt.to_owned(), false));
                    }
                    WackReadType::Char => {
                        insert_flag_gbl(GenFlags::READ_CHR);
                        asm.push(AsmInstruction::Call(inbuiltReadChar.to_owned(), false));
                    }
                }
            }
            WackInstr::FreeUnchecked(value) => {
                let operand = self.lower_value(value, asm);
                insert_flag_gbl(GenFlags::FREE);
                asm.push(AsmInstruction::Mov {
                    typ: Quadword,
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                asm.push(AsmInstruction::Call(inbuiltFree.to_owned(), false));
            }
            WackInstr::FreeChecked(value) => {
                let operand = self.lower_value(value, asm);
                insert_flag_gbl(GenFlags::FREE_PAIR);
                asm.push(AsmInstruction::Mov {
                    typ: Quadword,
                    src: operand,
                    dst: Operand::Reg(DI),
                });
                asm.push(AsmInstruction::Call(inbuiltFreePair.to_owned(), false));
            }
            WackInstr::Alloc { size, dst_ptr } => {
                println!("alloc size {}", size);
                let operand = self.lower_value(WackValue::Var(dst_ptr), asm);
                // Moving size to RDI for malloc function
                asm.push(AsmInstruction::Mov {
                    typ: Longword,
                    src: Operand::Imm(size as i32),
                    dst: Operand::Reg(DI),
                });
                asm.push(AsmInstruction::Call(inbuiltMalloc.to_owned(), false));
                // Moving pointer received to dst_ptr
                asm.push(AsmInstruction::Mov {
                    typ: Quadword,
                    src: Operand::Reg(AX),
                    dst: operand,
                });
            }
            WackInstr::CopyToOffset { src, dst, offset } => {
                let operand = self.lower_value(src, asm);
                let value = WackValue::Var(dst);
                let operand2 = self.lower_value(value, asm);
                println!("{:?}", operand2);
                let new_dst = match operand2 {
                    Operand::Pseudo(str) => Operand::PseudoMem(str, offset as i32),
                    // Operand::Memory(reg, off) => Operand::Memory(reg, off + offset as i32),
                    _ => unreachable!("copy to offset should be called only to memory"),
                };
                asm.push(AsmInstruction::Mov {
                    typ: Longword,
                    src: operand,
                    dst: new_dst,
                });
            }
            _ => unimplemented!(),
            // WackInstr::SignExtend { .. } => {}
            // WackInstr::Truncate { .. } => {}
            // WackInstr::ZeroExtend { .. } => {}
            // WackInstr::GetAddress { .. } => {}
            // WackInstr::Load { .. } => {}
            // WackInstr::Store { .. } => {}
            // WackInstr::AddPtr { .. } => {}
            // WackInstr::CopyFromOffset { .. } => {}
            // WackInstr::ArrayAccess { .. } => {} !
            // WackInstr::NullPtrGuard(_) => {} ! (nil)
        }
    }

    fn lower_fun_call(
        &mut self,
        fun_name: &str,
        args: &[WackValue],
        dst: WackTempIdent,
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
        let assembly_dst = self.lower_value(WackValue::Var(dst), asm);
        asm.push(Asm::Mov {
            typ: AssemblyType::Longword,
            src: Reg(AX),
            dst: assembly_dst,
        });
    }

    fn lower_return(&mut self, value: WackValue, asm_instructions: &mut Vec<AsmInstruction>) {
        use AsmInstruction as Asm;
        use AssemblyType::{Longword, Quadword};
        use Operand::Reg;
        use Register::{AX, BP, SP};

        let operand = self.lower_value(value, asm_instructions);
        // TODO: most of these arms aren't correct apart from Imm
        // in particular, these dont take into account types
        // also this can be refactored
        match operand {
            Operand::Imm(_) => asm_instructions.push(Asm::Mov {
                typ: Longword,
                src: operand,
                dst: Reg(AX),
            }),
            Operand::Reg(_) => asm_instructions.push(Asm::Mov {
                typ: Quadword,
                src: operand,
                dst: Reg(AX),
            }),
            Operand::Pseudo(_) => asm_instructions.push(Asm::Mov {
                typ: Quadword,
                src: operand,
                dst: Reg(AX),
            }),
            Operand::Memory(_, _) => asm_instructions.push(Asm::Mov {
                typ: Quadword,
                src: operand,
                dst: Reg(AX),
            }),
            Operand::Data(_, _) => asm_instructions.push(Asm::Mov {
                typ: Quadword,
                src: operand,
                dst: Reg(AX),
            }),
            Operand::PseudoMem(_, _) => asm_instructions.push(Asm::Mov {
                typ: Quadword,
                src: operand,
                dst: Reg(AX),
            }),
            Operand::Indexed { .. } => unimplemented!(),
            Operand::Stack(_) => unimplemented!(),
        }

        asm_instructions.push(Asm::Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        });
        asm_instructions.push(Asm::Pop(Reg(BP)));

        asm_instructions.push(Asm::Ret);
    }

    fn lower_unary(
        &mut self,
        op: &UnaryOp,
        src: WackValue,
        dst: WackTempIdent,
        asm: &mut Vec<AsmInstruction>,
    ) {
        use AsmInstruction as Asm;
        // TODO: check if this is what I need to do
        let src_operand = self.lower_value(src, asm);
        let dst_operand = self.lower_value(WackValue::Var(dst), asm);

        // Need to figure out how to get src_type and dst_type
        // for now treat everything as an Int cus its easier for me
        // TODO: change this to actually get the correct type
        let op = op.clone();
        #[allow(clippy::single_match_else)]
        match op {
            UnaryOp::Not => {
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
        op: &BinaryOp,
        src1: WackValue,
        src2: WackValue,
        dst: WackTempIdent,
        asm: &mut Vec<AsmInstruction>,
    ) {
        use AsmInstruction as Asm;
        use BinaryOp::*;

        let src1_operand = self.lower_value(src1, asm);
        let src2_operand = self.lower_value(src2, asm);
        let dst_operand = self.lower_value(WackValue::Var(dst), asm);

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
        use WackLiteral::{Bool, Char, Int, NullPair, StringLit};
        use WackValue::{Literal, Var};
        match value {
            Literal(Int(i)) => Operand::Imm(i),
            Literal(Bool(b)) => Operand::Imm(i32::from(b.into_u8())),
            Literal(Char(c)) => Operand::Imm(i32::from(c.into_u8())),
            // TODO: StringLit, need to add to symbol table with current function probably
            // while keeping track of a unique counter just for string constants
            // so we can emit properly, also this should emit a thing for RIP relative addressing
            // honestly its kinda similar to ConstDouble from the book
            Literal(StringLit(s)) => {
                let label = self.generate_str_label(&s);
                Operand::Data(label, 0)
            }
            Literal(NullPair) => Operand::Imm(0),
            // TODO: need to figure out if its a Scalar or Aggregate Value
            // so I can do either Pseudo or PseudoMem, for now its Pseudo
            Var(ident) => Operand::Pseudo(ident.into()),
        }
    }
}

fn convert_arith_binop(wacky_binop: BinaryOp) -> AsmBinaryOperator {
    use AsmBinaryOperator as AsmBinOp;
    use BinaryOp as BinOp;

    match wacky_binop {
        BinOp::Add => AsmBinOp::Add,
        BinOp::Sub => AsmBinOp::Sub,
        BinOp::Mul => AsmBinOp::Mult,
        _ => panic!("Invalid binary arithmetic operator for Asm"),
    }
}

fn convert_code(code: BinaryOp) -> CondCode {
    use BinaryOp::*;
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
