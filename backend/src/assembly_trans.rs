use crate::assembly_ast::{
    AsmBinaryOperator, AsmFunction, AsmInstruction, AsmLabel, AsmProgram, AssemblyType, CondCode,
    FUNCTION, LABEL, Operand, Register,
};
use crate::predefined::{GEN_USIZE, add_regsets};
use crate::regalloc::FunctionCallee;
use crate::registers::{ARR_INDEX_REG, ARR_LOAD_RETURN, ARR_PTR_REG, RegisterSet};
use AsmInstruction::{
    AllocateStack, Binary, Call, Cdq, Cmov, Cmp, Comment, DeallocateStack, Idiv, Jmp, JmpCC, Lea,
    Mov, MovZeroExtend, Push, Ret, SetCC, Test, Unary as AsmUnary,
};
use AssemblyType::{Byte, Longword, Quadword};
use CondCode::{E, G, GE, L, LE, NE};
use Operand::{Data, Imm, Indexed, Memory, Pseudo, Reg};
use Register::{AX, BP, CX, DI, DX, R8, R9, SI, SP};
use WackInstr::{
    AddPtr, Alloc, ArrayAccess, Binary as WackBinary, Copy, CopyToOffset, Exit, FreeChecked,
    FreeUnchecked, FunCall, Jump, JumpIfNotZero, JumpIfZero, Label, Load, NullPtrGuard, Print,
    Println, Read, Return, Unary as WackUnary,
};
use middle::thir_transform::WackIdentSymbolTable;
use middle::types::{BitWidth, WackType};
use middle::wackir::WackInstr::JumpToHandler;
use middle::wackir::{
    BinaryOp, UnaryOp, WackFunction, WackInstr, WackLiteral, WackPrintType, WackProgram,
    WackReadType, WackTempIdent, WackValue,
};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use util::gen_flags::{GenFlags, insert_flag_gbl};
use util::gen_flags::{
    INBUILT_ARR_LOAD1, INBUILT_ARR_LOAD4, INBUILT_ARR_LOAD8, INBUILT_BAD_CHAR, INBUILT_DIV_ZERO,
    INBUILT_EXIT, INBUILT_FREE, INBUILT_FREE_PAIR, INBUILT_MALLOC, INBUILT_NULL_ACCESS,
    INBUILT_PRINT_BOOL, INBUILT_PRINT_CHAR, INBUILT_PRINT_INT, INBUILT_PRINT_PTR,
    INBUILT_PRINT_STRING, INBUILT_PRINTLN, INBUILT_READ_CHAR, INBUILT_READ_INT,
};
/* ================== PUBLIC API ================== */

// The stack size for parameters is 8 bytes
const PARAM_STACK_SIZE: i32 = 8;
// Parameters are stored at 16(%rbp) and onwards
const STACK_START_PARAM: i32 = 16;
pub const MAIN_NAME: &str = "main";

#[inline]
#[must_use]
pub fn wacky_to_assembly(
    program: WackProgram,
    counter: usize,
    symbol_table: WackIdentSymbolTable,
) -> (AsmProgram, AsmGen, FunctionCallee) {
    let mut function_callee_regs = FunctionCallee::new();
    let mut asm_gen = AsmGen::new(counter, symbol_table);
    let mut asm_functions: Vec<AsmFunction> = Vec::new();
    asm_functions.push(asm_gen.lower_main_asm(program.main_body));
    for wack_function in program.functions {
        asm_functions.push(asm_gen.lower_function(wack_function));
    }

    for function in &asm_functions {
        function_callee_regs.insert(function.name.clone(), BTreeSet::new());
    }

    // Add the function to registers mapping to the register set
    add_regsets(&mut asm_gen);

    (AsmProgram { asm_functions }, asm_gen, function_callee_regs)
}

/* ================== INTERNAL API ================== */

pub type FunctionRegisters = HashMap<String, RegisterSet>;
pub struct AsmGen {
    pub counter: usize,
    pub str_counter: usize,
    pub str_literals: BTreeMap<String, String>,
    pub symbol_table: HashMap<WackTempIdent, AssemblyType>,
    pub function_regs: FunctionRegisters,
}

fn convert_type(ty: &WackType) -> AssemblyType {
    use {Byte, Longword, Quadword};

    match *ty {
        WackType::Int { ref width } => match width {
            BitWidth::W8 => Byte,
            BitWidth::W16 => {
                unimplemented!("The assembly typesystem does not support 16-bit width integers yet")
            }
            BitWidth::W32 => Longword,
            BitWidth::W64 => Quadword,
        },
        WackType::Pointer(_) => Quadword, // labels are pointers to code
        WackType::Array(_) => {
            unimplemented!("The Wack::Array <-> ByteArray conversion is not implemented yet")
        }
        WackType::Pair(_, _) => {
            unimplemented!("The AssemblyType system does not support raw pair-types yet")
        }
        WackType::Uninhabited => {
            unimplemented!("The AssemblyType system does not support raw uninhabited types yet")
        }
    }
}

const fn get_type_from_literal(lit: &WackLiteral) -> AssemblyType {
    use WackLiteral::{Bool, Char, Int, NullPair, StringLit};
    match *lit {
        Int(_) => Longword,
        Bool(_) | Char(_) => Byte,
        StringLit(_) | NullPair => Quadword,
    }
}

impl AsmGen {
    fn new(counter: usize, symbol_table: WackIdentSymbolTable) -> Self {
        let symbol_table = symbol_table
            .0
            .into_iter()
            .map(|(k, v)| (k, convert_type(&v)))
            .collect();
        Self {
            counter,
            str_counter: 0,
            str_literals: BTreeMap::new(),
            symbol_table,
            function_regs: HashMap::new(),
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

    fn get_asm_type(&self, value: &WackValue) -> AssemblyType {
        use WackValue::{Literal, Var};
        match value {
            Literal(lit) => get_type_from_literal(lit),
            Var(ident) => *self
                .symbol_table
                .get(ident)
                .expect("Variable not in symbol table"),
        }
    }

    #[allow(dead_code)]
    fn get_asm_type_for_ident(&self, ident: &WackTempIdent) -> AssemblyType {
        *self
            .symbol_table
            .get(ident)
            .expect("Variable not in symbol table")
    }

    fn lower_main_asm(&mut self, instrs: Vec<WackInstr>) -> AsmFunction {
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
            src: Imm(0),
            dst: Reg(AX),
        });

        // We handle the stack pointers and base pointers later on
        asm_instructions.push(Ret);

        // any functions we generate ourselves are not external
        AsmFunction {
            name: MAIN_NAME.to_owned(),
            global: true,
            instructions: asm_instructions,
            directives: vec![],
            regs: RegisterSet::empty(),
        }
    }

    fn lower_function(&mut self, wack_function: WackFunction) -> AsmFunction {
        // when we move the pushing to this phase instead of emission
        // add the push and mov instructions for rbp and rsp
        let mut asm = Vec::new();
        let func_name: String = wack_function.name.into();
        let params = wack_function.params;

        // It'd be nice if we didn't have to do this
        // and insert at index 2 in future passes
        asm.push(Push(Reg(BP)));
        asm.push(Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        });

        // Handle parameters finally
        // Pair up the parameters and registers for the first 6 parameters.

        asm.push(Comment(
            "Push registers onto stack to prevent clobbering".to_owned(),
        ));
        let arg_regs = [DI, SI, DX, CX, R8, R9];
        let reg_amount = params.len().min(arg_regs.len());
        for (param, reg) in params.iter().zip(arg_regs.iter()) {
            let wack_value = WackValue::Var(param.clone());
            let typ = self.get_asm_type(&wack_value);
            let param_name = self.lower_value(wack_value, &mut asm);
            asm.push(Mov {
                typ,
                src: Reg(*reg),
                dst: param_name,
            });
        }

        // if there are extra parameters beyond the 6 registers
        // move them from the stack into the parameters
        asm.push(Comment(
            "Move remaining parameters from stack into our stack frame".to_owned(),
        ));
        let mut stack_index = 0;
        for param in params.iter().skip(arg_regs.len()) {
            let wack_value = WackValue::Var(param.clone());
            let typ = self.get_asm_type(&wack_value);
            let param_name = self.lower_value(wack_value, &mut asm);
            // Params are stored at 16(%rbp) and onwards
            asm.push(Mov {
                typ,
                src: Memory(BP, STACK_START_PARAM + stack_index),
                dst: param_name,
            });
            // Move to next parameter which are 8 bytes apart
            stack_index += PARAM_STACK_SIZE;
        }

        for instr in wack_function.body {
            self.lower_instruction(instr, &mut asm);
        }

        let rs: RegisterSet = match reg_amount {
            0 => RegisterSet::empty(),
            1 => crate::registers::RS1,
            2 => crate::registers::RS2,
            3 => crate::registers::RS3,
            4 => crate::registers::RS4,
            5 => crate::registers::RS5,
            6 => crate::registers::RS6,
            _ => panic!("Too many parameters for function"),
        };
        self.function_regs.insert(func_name.clone(), rs);

        // any functions we generate ourselves are not external
        AsmFunction {
            name: func_name,
            global: false,
            instructions: asm,
            directives: vec![],
            regs: rs,
        }
    }

    fn lower_print(&mut self, src: WackValue, ty: WackPrintType, asm: &mut Vec<AsmInstruction>) {
        let operand = self.lower_value(src, asm);
        let func_name = match ty {
            WackPrintType::Int => {
                asm.push(Mov {
                    typ: Longword,
                    src: operand,
                    dst: Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_INT);
                INBUILT_PRINT_INT.to_owned()
            }
            WackPrintType::Bool => {
                asm.push(Mov {
                    typ: Byte,
                    src: operand,
                    dst: Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_BOOLEAN);
                INBUILT_PRINT_BOOL.to_owned()
            }
            WackPrintType::Char => {
                asm.push(Mov {
                    typ: Byte,
                    src: operand,
                    dst: Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_CHR);
                INBUILT_PRINT_CHAR.to_owned()
            }
            WackPrintType::StringOrCharArray => {
                match operand {
                    Data(_, _) => {
                        asm.push(Lea {
                            src: operand,
                            dst: Reg(DI),
                        });
                    }
                    _ => {
                        asm.push(Mov {
                            typ: Quadword,
                            src: operand,
                            dst: Reg(DI),
                        });
                    }
                }
                insert_flag_gbl(GenFlags::PRINT_STR);
                INBUILT_PRINT_STRING.to_owned()
            }
            WackPrintType::OtherArray | WackPrintType::Pair => {
                asm.push(Mov {
                    typ: Quadword,
                    src: operand,
                    dst: Reg(DI),
                });
                insert_flag_gbl(GenFlags::PRINT_PTR);
                INBUILT_PRINT_PTR.to_owned()

                //TODO: does this print `nil` for null pointers??
            }
        };
        asm.push(Call(func_name, false));
    }

    #[allow(clippy::too_many_lines)]
    fn lower_instruction(&mut self, instr: WackInstr, asm: &mut Vec<AsmInstruction>) {
        match instr {
            Return(value) => self.lower_return(value, asm),
            WackUnary { op, src, dst } => self.lower_unary(op, src, dst, asm),
            WackBinary {
                op,
                src1,
                src2,
                dst,
            } => self.lower_binary(op, src1, src2, dst, asm),
            // Any Jump from MiddleIR is a Label Jump, perhaps a change of name is better
            // ie JumpToLabel, I agree - Ahmad
            Jump(target) => asm.push(Jmp(target, LABEL)),
            JumpIfZero { condition, target } => {
                let typ = self.get_asm_type(&condition);
                let condition = self.lower_value(condition, asm);
                asm.push(Cmp {
                    typ,
                    op1: Imm(0),
                    op2: condition,
                });
                asm.push(JmpCC {
                    condition: E,
                    label: target,
                    is_func: false,
                });
            }
            JumpIfNotZero { condition, target } => {
                let typ = self.get_asm_type(&condition);
                let condition = self.lower_value(condition, asm);
                asm.push(Cmp {
                    typ,
                    op1: Imm(0),
                    op2: condition,
                });
                asm.push(JmpCC {
                    condition: NE,
                    label: target,
                    is_func: false,
                });
            }
            JumpToHandler(predefined_func_name) => {
                let predefined_func_name = AsmLabel::new(predefined_func_name.as_str(), GEN_USIZE);
                asm.push(Jmp(predefined_func_name, FUNCTION));
            }
            Copy { src, dst } => {
                let src_typ = self.get_asm_type(&src);
                let src_operand = self.lower_value(src, asm);
                let dst_operand = self.lower_value(WackValue::Var(dst), asm);
                match src_operand {
                    Data(_, _) => asm.push(Lea {
                        src: src_operand,
                        dst: dst_operand,
                    }),
                    _ => asm.push(Mov {
                        typ: src_typ,
                        src: src_operand,
                        dst: dst_operand,
                    }),
                }
            }
            Label(id) => asm.push(AsmInstruction::Label(id)),
            FunCall {
                fun_name,
                args,
                dst,
            } => self.lower_fun_call((&fun_name).into(), &args, dst, asm),
            Print { src, ty } => self.lower_print(src, ty, asm),
            Println { src, ty } => {
                self.lower_print(src, ty, asm);
                insert_flag_gbl(GenFlags::PRINT_LN);
                asm.push(Call(INBUILT_PRINTLN.to_owned(), false));
            }
            Exit(value) => {
                insert_flag_gbl(GenFlags::EXIT);
                let operand = self.lower_value(value, asm);
                asm.push(Mov {
                    typ: Longword,
                    src: operand,
                    dst: Reg(DI),
                });
                asm.push(Call(INBUILT_EXIT.to_owned(), false));
            }
            Read { dst, ty } => {
                let asm_typ = match ty {
                    WackReadType::Int => Longword,
                    WackReadType::Char => Byte,
                };
                let dst = WackValue::Var(dst);
                let operand = self.lower_value(dst, asm);
                asm.push(Mov {
                    typ: asm_typ,
                    src: operand.clone(),
                    dst: Reg(DI),
                });
                match ty {
                    WackReadType::Int => {
                        insert_flag_gbl(GenFlags::READ_INT);
                        asm.push(Call(INBUILT_READ_INT.to_owned(), false));
                    }
                    WackReadType::Char => {
                        insert_flag_gbl(GenFlags::READ_CHR);
                        asm.push(Call(INBUILT_READ_CHAR.to_owned(), false));
                    }
                }
                asm.push(Mov {
                    typ: asm_typ,
                    src: Reg(AX),
                    dst: operand,
                });
            }
            FreeUnchecked(value) => {
                let operand = self.lower_value(value, asm);
                insert_flag_gbl(GenFlags::FREE);
                asm.push(Mov {
                    typ: Quadword,
                    src: operand,
                    dst: Reg(DI),
                });
                asm.push(Call(INBUILT_FREE.to_owned(), false));
            }
            FreeChecked(value) => {
                let operand = self.lower_value(value, asm);
                insert_flag_gbl(GenFlags::FREE_PAIR);
                asm.push(Mov {
                    typ: Quadword,
                    src: operand,
                    dst: Reg(DI),
                });
                asm.push(Call(INBUILT_FREE_PAIR.to_owned(), false));
            }
            Alloc { size, dst_ptr } => {
                let dst_ptr = WackValue::Var(dst_ptr);
                let operand = self.lower_value(dst_ptr, asm);
                // Moving size to RDI for malloc function
                asm.push(Mov {
                    typ: Longword,
                    src: Imm(size as i32),
                    dst: Reg(DI),
                });
                insert_flag_gbl(GenFlags::MALLOC);
                asm.push(Call(INBUILT_MALLOC.to_owned(), false));
                // Moving pointer received to dst_ptr
                asm.push(Mov {
                    typ: Quadword,
                    src: Reg(AX),
                    dst: operand,
                });
            }
            CopyToOffset {
                src,
                dst_ptr,
                offset,
            } => {
                let typ = self.get_asm_type(&src);
                let operand = self.lower_value(src, asm);
                let dst = WackValue::Var(dst_ptr);
                let operand2 = self.lower_value(dst, asm);

                let get_base_dst_instr = Mov {
                    typ: Quadword,
                    src: operand2,
                    dst: Reg(AX),
                };
                asm.push(get_base_dst_instr);
                let new_dst = Memory(AX, offset);
                if let Data(_, _) = operand {
                    asm.push(Lea {
                        src: operand,
                        dst: new_dst,
                    });
                } else {
                    asm.push(Mov {
                        typ,
                        src: operand,
                        dst: new_dst,
                    });
                }
            }
            Load { src_ptr, dst } => {
                // let dst = WackValue::Var(dst);
                // let dst_typ = self.get_asm_type(&dst);
                // let operand_src_ptr = self.lower_value(src_ptr, asm);
                // let operand_dst = self.lower_value(dst, asm);
                // asm.push(Mov {
                //     typ: Quadword,
                //     src: operand_src_ptr,
                //     dst: Reg(Register::R10),
                // });
                // asm.push(Mov {
                //     typ: Quadword,
                //     src: Memory(Register::R10, 0),
                //     dst: Reg(Register::R11),
                // });
                // asm.push(Mov {
                //     typ: dst_typ,
                //     src: Reg(Register::R11),
                //     dst: operand_dst,
                // });
                let dst = WackValue::Var(dst);
                let dst_typ = self.get_asm_type(&dst);
                let operand_src_ptr = self.lower_value(src_ptr, asm);
                let operand_dst = self.lower_value(dst, asm);

                // Move the pointer value into a register
                asm.push(Mov {
                    typ: Quadword, // Pointers are always 64-bit (8 bytes)
                    src: operand_src_ptr,
                    dst: Reg(Register::R11),
                });

                // Dereference the pointer with the appropriate size
                asm.push(Mov {
                    typ: dst_typ, // Use the destination's type for the load
                    src: Memory(Register::R11, 0),
                    dst: operand_dst,
                });
            }
            NullPtrGuard(ptr) => {
                let operand = self.lower_value(ptr, asm);
                // Compare pointer with 0 to check if it's null
                asm.push(Cmp {
                    typ: Quadword,
                    op1: operand,
                    op2: Imm(0),
                });
                insert_flag_gbl(GenFlags::NULL_DEREF);
                // Jump to _errNull code if pointer is null
                asm.push(JmpCC {
                    condition: E,
                    label: AsmLabel::new(INBUILT_NULL_ACCESS, GEN_USIZE),
                    is_func: true,
                });
            }
            AddPtr {
                src_ptr,
                index,
                scale,
                offset,
                dst_ptr,
            } => {
                let src_ptr_operand = self.lower_value(src_ptr, asm);
                let value = WackValue::Var(dst_ptr);
                let dst_ptr_operand = self.lower_value(value, asm);
                let index_operand = self.lower_value(index, asm);
                asm.push(Mov {
                    typ: Quadword,
                    src: src_ptr_operand,
                    dst: Reg(AX),
                });
                asm.push(Mov {
                    typ: Quadword,
                    src: index_operand,
                    dst: Reg(DX),
                });
                asm.push(Lea {
                    src: Indexed {
                        offset,
                        base: AX,
                        index: DX,
                        scale: scale as i32,
                    },
                    dst: dst_ptr_operand,
                });
            }
            ArrayAccess {
                src_array_ptr,
                index,
                scale,
                dst_elem_ptr,
            } => {
                let src_array_operand = self.lower_value(src_array_ptr, asm);
                let value = WackValue::Var(dst_elem_ptr);
                let dst_ptr_operand = self.lower_value(value, asm);
                asm.push(Mov {
                    typ: Quadword,
                    src: src_array_operand,
                    dst: Reg(ARR_PTR_REG),
                });
                let index_operand = self.lower_value(index, asm);
                asm.push(Mov {
                    typ: Longword,
                    src: index_operand,
                    dst: Reg(ARR_INDEX_REG),
                });

                let inbuilt_instr = match scale {
                    1 => {
                        insert_flag_gbl(GenFlags::ARRAY_ACCESS1);
                        INBUILT_ARR_LOAD1
                    }
                    4 => {
                        insert_flag_gbl(GenFlags::ARRAY_ACCESS4);
                        INBUILT_ARR_LOAD4
                    }
                    8 => {
                        insert_flag_gbl(GenFlags::ARRAY_ACCESS8);
                        INBUILT_ARR_LOAD8
                    }
                    _ => unreachable!("unexpected scale value in array access"),
                };
                asm.push(Call(inbuilt_instr.to_owned(), false));
                asm.push(Mov {
                    typ: Quadword,
                    src: Reg(ARR_LOAD_RETURN),
                    dst: dst_ptr_operand,
                });
            }
        }
    }

    fn lower_fun_call(
        &mut self,
        fun_name: &str,
        args: &[WackValue],
        dst: WackTempIdent,
        asm: &mut Vec<AsmInstruction>,
    ) {
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
            PARAM_STACK_SIZE
        } else {
            0
        };

        // used to align the stack to 16 bytes for function calls
        if stack_padding != 0 {
            asm.push(Comment("This aligns stack to 16 bytes".to_owned()));
            asm.push(AllocateStack(stack_padding));
        }

        // pass args in registers
        for (reg_index, tacky_arg) in register_args.iter().enumerate() {
            let r = arg_regs[reg_index];
            let typ = self.get_asm_type(tacky_arg);
            let assembly_arg = self.lower_value(tacky_arg.clone(), asm);
            if let Data(_, _) = assembly_arg {
                asm.push(Lea {
                    src: assembly_arg,
                    dst: Reg(r),
                });
            } else {
                asm.push(Mov {
                    typ,
                    src: assembly_arg,
                    dst: Reg(r),
                });
            }
        }

        // pass rest of args on stack
        // Note that the SystemV ABI requires that the stack is 16-byte aligned
        // and that each argument is 8-byte aligned
        // it might not matter now but it will matter if we ever link with C code
        // Note there is an edge case which we don't handle rn which is when we push
        // a 4-byte memory operand and those 4 bytes after it aren't readable memory

        if let Some(stack_args) = stack_args {
            asm.push(Comment(
                "Pushing arguments to stack in reverse order".to_owned(),
            ));
            for tacky_arg in stack_args.iter().rev() {
                let typ = self.get_asm_type(tacky_arg);
                let assembly_arg: Operand = self.lower_value(tacky_arg.clone(), asm);
                if let Reg(_) = assembly_arg {
                    asm.push(Push(assembly_arg));
                } else if let Imm(_) = assembly_arg {
                    asm.push(Push(assembly_arg));
                } else if typ == Quadword {
                    asm.push(Push(assembly_arg));
                } else {
                    // This solves the issue of pushing -4(%rbp) to the stack
                    asm.push(Mov {
                        typ: Longword,
                        src: assembly_arg,
                        dst: Reg(AX),
                    });
                    asm.push(Push(Reg(AX)));
                }
            }
        }

        // Any function call that the user makes will only call
        // other wacc functions which are all internal, hence external flag is false
        asm.push(Call(fun_name.into(), false));

        // adjust stack pointer
        #[allow(clippy::cast_possible_truncation)]
        let bytes_to_remove: i32 =
            PARAM_STACK_SIZE * stack_args.map_or(0, |args| args.len()) as i32;
        if bytes_to_remove != 0 {
            asm.push(DeallocateStack(bytes_to_remove));
        }

        // handle return value
        let dst = WackValue::Var(dst);
        let dst_typ = self.get_asm_type(&dst);
        let assembly_dst = self.lower_value(dst, asm);
        asm.push(Mov {
            typ: dst_typ,
            src: Reg(AX),
            dst: assembly_dst,
        });
    }

    fn lower_return(&mut self, value: WackValue, asm_instructions: &mut Vec<AsmInstruction>) {
        let typ = self.get_asm_type(&value);
        let operand = self.lower_value(value, asm_instructions);
        // TODO: most of these arms aren't correct apart from Imm
        // in particular, these dont take into account types
        // also this can be refactored
        match operand {
            Imm(_) => asm_instructions.push(Mov {
                typ,
                src: operand,
                dst: Reg(AX),
            }),
            Reg(_) => asm_instructions.push(Mov {
                typ,
                src: operand,
                dst: Reg(AX),
            }),
            Pseudo(_) => asm_instructions.push(Mov {
                typ,
                src: operand,
                dst: Reg(AX),
            }),
            Memory(_, _) => asm_instructions.push(Mov {
                typ,
                src: operand,
                dst: Reg(AX),
            }),
            Data(_, _) => asm_instructions.push(Mov {
                typ,
                src: operand,
                dst: Reg(AX),
            }),
            Indexed { .. } => {
                unimplemented!("for now we don't make indexed at this lowering stage")
            }
        }

        // TODO: if my thing doesn't work, we can just do this
        // asm_instructions.push(Mov {
        //     typ: Quadword,
        //     src: Reg(BP),
        //     dst: Reg(SP),
        // });
        // asm_instructions.push(Pop(BP));

        asm_instructions.push(Ret);
    }

    fn lower_unary(
        &mut self,
        op: UnaryOp,
        src: WackValue,
        dst: WackTempIdent,
        asm: &mut Vec<AsmInstruction>,
    ) {
        let dst = WackValue::Var(dst);
        let src_typ = self.get_asm_type(&src);
        let dst_typ = self.get_asm_type(&dst);
        let src_operand = self.lower_value(src, asm);
        let dst_operand = self.lower_value(dst, asm);

        #[allow(clippy::single_match_else)]
        match op {
            UnaryOp::LNot => {
                let new_instrs = vec![
                    Cmp {
                        typ: src_typ,
                        op1: Imm(0),
                        op2: src_operand,
                    },
                    Mov {
                        typ: dst_typ,
                        src: Imm(0),
                        dst: dst_operand.clone(),
                    },
                    SetCC {
                        condition: E,
                        operand: dst_operand,
                    },
                ];
                asm.extend(new_instrs);
            }
            UnaryOp::Chr => {
                let new_instrs = vec![
                    Mov {
                        typ: src_typ,
                        src: src_operand,
                        dst: Reg(AX),
                    },
                    Test {
                        typ: src_typ,
                        op1: Imm(-128),
                        op2: Reg(AX),
                    },
                    // Must be 64 bit so it doesn't truncate if the move fails
                    Cmov {
                        condition: NE,
                        typ: Quadword,
                        src: Reg(AX),
                        dst: Reg(SI),
                    },
                    JmpCC {
                        condition: NE,
                        label: AsmLabel::new(INBUILT_BAD_CHAR, GEN_USIZE),
                        is_func: true,
                    },
                    Mov {
                        typ: dst_typ,
                        src: Reg(AX),
                        dst: dst_operand,
                    },
                ];
                insert_flag_gbl(GenFlags::CHR_BOUNDS);
                asm.extend(new_instrs);
            }
            UnaryOp::Ord => asm.push(MovZeroExtend {
                src_type: src_typ,
                dst_type: dst_typ,
                src: src_operand,
                dst: dst_operand,
            }),
            _ => {
                asm.push(Mov {
                    typ: src_typ,
                    src: src_operand,
                    dst: dst_operand.clone(),
                });
                asm.push(AsmUnary {
                    operator: op.into(),
                    typ: src_typ,
                    operand: dst_operand,
                });
            }
        }
    }

    fn lower_binary(
        &mut self,
        op: BinaryOp,
        src1: WackValue,
        src2: WackValue,
        dst: WackTempIdent,
        asm: &mut Vec<AsmInstruction>,
    ) {
        use BinaryOp::{Add, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Neq, Sub};

        let src_typ = self.get_asm_type(&src1);
        let dst = WackValue::Var(dst);
        let dst_typ = self.get_asm_type(&dst);
        let src1_operand = self.lower_value(src1, asm);
        let src2_operand = self.lower_value(src2, asm);
        let dst_operand = self.lower_value(dst, asm);

        // We handle Div and Remainder operations differently
        // than the rest since it has specific semantics in x86-64
        match op {
            Div | Mod => {
                insert_flag_gbl(GenFlags::DIV_BY_ZERO);
                // We need to sign extend EAX into EAX:EDX
                // Handle div by zero here
                // While not necessary, could add a typ check
                assert_eq!(src_typ, Longword);

                let new_instrs = vec![
                    Cmp {
                        typ: Longword,
                        op1: src2_operand.clone(),
                        op2: Imm(0),
                    },
                    JmpCC {
                        condition: E,
                        label: AsmLabel::new(INBUILT_DIV_ZERO, GEN_USIZE),
                        is_func: true,
                    },
                    Mov {
                        typ: Longword,
                        src: src1_operand,
                        dst: Reg(AX),
                    },
                    Cdq,
                    Idiv(src2_operand),
                ];
                // Result is stored in different registers
                // Depending on operation
                let dst_reg = match op {
                    Div => Reg(AX),
                    Mod => Reg(DX),
                    _ => unreachable!(),
                };
                asm.extend(new_instrs);
                asm.push(Mov {
                    typ: Longword,
                    src: dst_reg,
                    dst: dst_operand,
                });
            }
            Mul | Add | Sub => {
                // Handle other binary operations in the same way
                assert_eq!(src_typ, Longword);
                asm.push(Mov {
                    typ: Longword,
                    src: src1_operand,
                    dst: dst_operand.clone(),
                });
                let asm_op = convert_arith_binop(op);
                // For now its binary operations so insert flag here
                insert_flag_gbl(GenFlags::OVERFLOW);
                asm.push(Binary {
                    operator: asm_op,
                    typ: Longword,
                    op1: src2_operand,
                    op2: dst_operand,
                });
            }
            Gt | Gte | Lt | Lte | Eq | Neq => {
                let new_instrs = vec![
                    Cmp {
                        typ: src_typ,
                        op1: src2_operand,
                        op2: src1_operand,
                    },
                    Mov {
                        typ: dst_typ,
                        src: Imm(0),
                        dst: dst_operand.clone(),
                    },
                    SetCC {
                        condition: convert_code(op),
                        operand: dst_operand,
                    },
                ];
                asm.extend(new_instrs);
            }
            _ => unimplemented!(),
        }
    }

    // This lowers a WackValue to an Asm Operand
    fn lower_value(&mut self, value: WackValue, _asm_instructions: &[AsmInstruction]) -> Operand {
        use WackLiteral::{Bool, Char, Int, NullPair, StringLit};
        use WackValue::{Literal, Var};
        match value {
            Literal(Int(i)) => Imm(i),
            Literal(Bool(b)) => Imm(i32::from(b)),
            Literal(Char(c)) => Imm(i32::from(c)),
            // TODO: StringLit, need to add to symbol table with current function probably
            // while keeping track of a unique counter just for string constants
            // so we can emit properly, also this should emit a thing for RIP relative addressing
            // honestly its kinda similar to ConstDouble from the book
            Literal(StringLit(s)) => {
                let label = self.generate_str_label(&s);
                Data(label, 0)
            }
            Literal(NullPair) => Imm(0),
            // TODO: need to figure out if its a Scalar or Aggregate Value
            // so I can do either Pseudo or PseudoMem, for now its Pseudo
            Var(ident) => Pseudo(ident.into()),
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
    use BinaryOp::{Eq, Gt, Gte, Lt, Lte, Neq};

    match code {
        Gt => G,
        Gte => GE,
        Lt => L,
        Lte => LE,
        Eq => E,
        Neq => NE,
        _ => panic!("Cannot convert wack binary operator to assembly condition code"),
    }
}
