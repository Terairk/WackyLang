use core::panic;

use util::gen_flags::get_flags_gbl;

use crate::{
    assembly_ast::{
        AsmBinaryOperator, AsmFunction, AsmInstruction, AsmProgram, AsmUnaryOperator, AssemblyType,
        CondCode, Operand, Register,
    },
    gen_predefined::emit_predefined_asm,
};

// TODO: this code was mostly generated by an LLM, please have mercy on it
// Also its very wrong, I'm so sorry, most of these are just placeholders
// and will definitely need to be looked at again
// Also it doesn't use the GenFlags struct
// Also it doesn't emit any data section with Strings yet
// I also just realised I dont need to handle any external stuff as they're all
// gonna be part of the Pre-defined Functions/Widget Abstraction pre-generated code
// All I'm doing is calling my own wrapper of the functions

// TODO: create the abstraction for Pre-defined Functions

pub struct AssemblyFormatter;

// Return has to be handled specially to avoid being indented
const RETURN_STRING: &str = "movq %rbp, %rsp\npopq %rbp\nret\n";

// This might be a temporary thing but I'll put this into emission phase for now
// as we always want to do this anyways
const JO_OVERFLOW: &str = "jo _errOverflow";

impl AssemblyFormatter {
    // Format an entire program by printing each function.

    #[inline]
    pub fn format_program(program: &AsmProgram) -> String {
        let mut output = String::new();
        // TODO: emit string constants here with their length

        // have a .text directive here once per assembly file
        output.push_str(".text\n");
        for func in &program.asm_functions {
            output.push_str(&Self::format_function(func));
        }

        // now emit assembly based on flags
        let global_flags = get_flags_gbl();
        output.push_str(&emit_predefined_asm(global_flags));
        output
    }
    // Format a single function. This prints any global,
    // a label for the function, the instructions and adds a blank line at the end.
    #[inline]
    pub fn format_function(func: &AsmFunction) -> String {
        let mut lines = Vec::new();
        if func.global {
            // Global directives start with a dot, so they will have no indent.
            lines.push(format!(".globl {}", func.name));
        }
        // Print function label (ends with ":" so we omit the indent).
        lines.push(format!("{}:", func.name));
        lines.push(Self::apply_indentation("pushq %rbp"));
        lines.push(Self::apply_indentation("movq %rsp, %rbp"));
        for inst in &func.instructions {
            let inst_line = Self::format_instruction(inst);
            // For each instruction line, apply the proper indentation.
            for single_line in inst_line.lines() {
                lines.push(Self::apply_indentation(single_line));
            }
        }
        // Ensure at least one blank line at the end of the function.
        lines.push(String::new());
        let mut result = lines.join("\n");
        result.push('\n');
        result
    }

    // Format a single instruction by matching on the AST enum.
    #[inline]
    pub fn format_instruction(inst: &AsmInstruction) -> String {
        match inst {
            AsmInstruction::Mov { typ, src, dst } => {
                let suffix = Self::assembly_type_suffix(typ);
                let src_str = Self::format_operand(src, typ);
                let dst_str = Self::format_operand(dst, typ);
                format!("mov{} {}, {}", suffix, src_str, dst_str)
            }
            AsmInstruction::MovZeroExtend {
                src_type,
                dst_type,
                src,
                dst,
            } => {
                // Using the destination type to determine the suffix.
                let suffix = Self::assembly_type_suffix(dst_type);
                let src_str = Self::format_operand(src, src_type);
                let dst_str = Self::format_operand(dst, dst_type);
                format!("movzx{} {}, {}", suffix, src_str, dst_str)
            }
            AsmInstruction::Lea { src, dst } => {
                let src_str = Self::format_operand(src, &AssemblyType::Quadword);
                let dst_str = Self::format_operand(dst, &AssemblyType::Quadword);
                format!("lea {}, {}", src_str, dst_str)
            }
            AsmInstruction::Unary {
                operator,
                typ,
                operand,
            } => {
                let suffix = Self::assembly_type_suffix(typ);
                let operand_str = Self::format_operand(operand, typ);
                let op = Self::format_unary_operator(operator);
                format!("{op}{suffix} {operand_str}")
            }
            AsmInstruction::Binary {
                operator,
                typ,
                op1,
                op2,
            } => {
                let suffix = Self::assembly_type_suffix(typ);
                let op1_str = Self::format_operand(op1, typ);
                let op2_str = Self::format_operand(op2, typ);
                let op = Self::format_binary_operator(operator);

                // For now its just arithmetic operation
                // So we just handle overflow here

                format!("{op}{suffix} {op1_str}, {op2_str}\n{JO_OVERFLOW}")
            }
            AsmInstruction::Cmp { typ, op1, op2 } => {
                let suffix = Self::assembly_type_suffix(typ);
                let op1_str = Self::format_operand(op1, typ);
                let op2_str = Self::format_operand(op2, typ);
                format!("cmp{} {}, {}", suffix, op1_str, op2_str)
            }
            AsmInstruction::Idiv(op) => {
                // idivl because we only work with ints in WACC
                let op_str = Self::format_operand(op, &AssemblyType::Longword);
                format!("idivl {op_str}")
            }
            AsmInstruction::Cdq => "cdq".to_owned(),
            AsmInstruction::Jmp(label) => format!("jmp .L_{label}"),
            AsmInstruction::JmpCC { condition, label } => {
                let cond = Self::format_cond_code(condition);
                format!("j{cond} .L_{label}")
            }
            AsmInstruction::SetCC { condition, operand } => {
                let cond = Self::format_cond_code(condition);
                let op_str = Self::format_operand(operand, &AssemblyType::Byte);
                format!("set{} {}", cond, op_str)
            }
            AsmInstruction::Label(name) => format!(".L_{name}:"),
            AsmInstruction::AllocateStack(bytes) => {
                // In AT&T syntax, stack allocation is often performed by subtracting from %rsp.
                format!("subq ${bytes}, %rsp")
            }
            AsmInstruction::Push(op) => {
                let op_str = Self::format_operand(op, &AssemblyType::Quadword);
                format!("push {op_str}")
            }
            // Note these are our own calls so these don't need to be with @PLT
            // all @PLT external calls are already pre-generated
            AsmInstruction::Call(name, external) => {
                if *external {
                    format!("call {name}@PLT")
                } else {
                    format!("call {name}")
                }
            }
            AsmInstruction::Ret => {
                // Since return is special we'll have to handle it specially
                RETURN_STRING.to_owned()
            }
            AsmInstruction::DeallocateStack(bytes) => {
                // In AT&T syntax, stack deallocation is often performed by adding to %rsp.
                format!("addq ${bytes}, %rsp")
            }
        }
    }

    // Return the appropriate suffix for the given AssemblyType.
    // (e.g. "b" for byte, "l" for longword, "q" for quadword)
    #[inline]
    pub fn assembly_type_suffix(typ: &AssemblyType) -> &'static str {
        match typ {
            AssemblyType::Byte => "b",
            AssemblyType::Longword => "l",
            AssemblyType::Quadword => "q",
            AssemblyType::ByteArray { .. } => "by", // Default fallback for byte arrays - maybe not
                                                    // necessary
        }
    }

    /// Format an operand into its AT&T assembly representation.
    #[inline]
    pub fn format_operand(op: &Operand, typ: &AssemblyType) -> String {
        match op {
            Operand::Imm(val) => format!("${}", val),
            Operand::Reg(reg) => Self::format_register(reg, typ),
            Operand::Memory(reg, offset) => {
                // Format as: offset(%reg) (with no offset if the value is 0)
                let reg_str = Self::format_register(reg, &AssemblyType::Quadword);
                if *offset == 0 {
                    format!("({})", reg_str)
                } else {
                    format!("{}({})", offset, reg_str)
                }
            }
            Operand::Data(label, offset) => {
                // RIP-relative addressing for data.
                // TODO: needs an assembly symbol table to resolve if its local
                // label or a regular label etc but figure this out later
                if *offset == 0 {
                    format!("{}(%rip)", label)
                } else {
                    format!("{}+{}(%rip)", label, offset)
                }
            }
            Operand::Indexed { base, index, scale } => {
                // Format: (base, index, scale)
                let reg1 = Self::format_register(base, &AssemblyType::Quadword);
                let reg2 = Self::format_register(index, &AssemblyType::Quadword);
                format!("({reg1}, {reg2}, {scale})")
            }
            Operand::Stack(offset) => {
                // Using %rbp as the base pointer for a stack reference.
                format!("{}(%rbp)", offset)
            }

            // Printing out pseudoregisters is only for debugging,
            // these should not occur in regular assembly
            Operand::Pseudo(name) => name.clone(),
            Operand::PseudoMem(name, offset) => {
                // Similar convention as for Data.
                if *offset == 0 {
                    format!("{}(%rip)", name)
                } else {
                    format!("{}+(%%{})", offset, name)
                }
            }
        }
    }

    pub fn format_register(reg: &Register, typ: &AssemblyType) -> String {
        match typ {
            AssemblyType::Byte => Self::format_byte_reg(reg).to_owned(),
            AssemblyType::Longword => Self::format_long_reg(reg).to_owned(),
            AssemblyType::Quadword => Self::format_quad_reg(reg).to_owned(),
            AssemblyType::ByteArray { .. } => {
                // Default to quadword for byte arrays.
                Self::format_quad_reg(reg).to_owned()
            }
        }
    }

    /// Format a register – adds a "%" prefix and maps our enum to the 64-bit register names.
    fn format_quad_reg(reg: &Register) -> &'static str {
        match *reg {
            Register::AX => "%rax",
            Register::CX => "%rcx",
            Register::DX => "%rdx",
            Register::DI => "%rdi",
            Register::SI => "%rsi",
            Register::R8 => "%r8",
            Register::R9 => "%r9",
            Register::R10 => "%r10",
            Register::R11 => "%r11",
            Register::SP => "%rsp",
            Register::BP => "%rbp",
        }
    }

    // 32 bit registers
    fn format_long_reg(reg: &Register) -> &'static str {
        match *reg {
            Register::AX => "%eax",
            Register::CX => "%ecx",
            Register::DX => "%edx",
            Register::DI => "%edi",
            Register::SI => "%esi",
            Register::R8 => "%r8d",
            Register::R9 => "%r9d",
            Register::R10 => "%r10d",
            Register::R11 => "%r11d",
            Register::SP => panic!("Stack pointer cannot be 32-bit"),
            Register::BP => panic!("Base pointer cannot be 32-bit"),
        }
    }

    #[inline]
    // 8 bit registers
    fn format_byte_reg(reg: &Register) -> &'static str {
        match *reg {
            Register::AX => "%al",
            Register::CX => "%cl",
            Register::DX => "%dl",
            Register::DI => "%dil",
            Register::SI => "%sil",
            Register::R8 => "%r8b",
            Register::R9 => "%r9b",
            Register::R10 => "%r10b",
            Register::R11 => "%r11b",
            Register::SP => panic!("Stack pointer cannot be 8-bit"),
            Register::BP => panic!("Base pointer cannot be 8-bit"),
        }
    }

    /// Format the binary operator as the proper mnemonic.
    #[inline]
    pub const fn format_binary_operator(op: &AsmBinaryOperator) -> &'static str {
        match *op {
            AsmBinaryOperator::Add => "add",
            AsmBinaryOperator::Sub => "sub",
            AsmBinaryOperator::Mult => "imul", // AT&T multiplication
            AsmBinaryOperator::And => "and",
            AsmBinaryOperator::Or => "or",
            AsmBinaryOperator::Xor => "xor",
            AsmBinaryOperator::Shl => "sal", // shift left (sal is used in AT&T)
            AsmBinaryOperator::ShrTwoOp => "shr",
        }
    }

    /// Format the unary operator as the proper mnemonic.
    #[inline]
    pub const fn format_unary_operator(op: &AsmUnaryOperator) -> &'static str {
        match *op {
            AsmUnaryOperator::Neg => "neg",
            AsmUnaryOperator::Not => "not",
            AsmUnaryOperator::Shr => "shr",
            AsmUnaryOperator::Len => panic!("Len asm operator not implemented yet"),
            AsmUnaryOperator::Ord => {
                panic!("Ord asm operator not implemented yet, this should be a no-op anyway")
            }
            AsmUnaryOperator::Chr => panic!("Chr asm operator not implemented yet"),
        }
    }

    /// Map your condition codes to the AT&T conditional suffix.
    #[inline]
    pub const fn format_cond_code(cc: &CondCode) -> &'static str {
        match *cc {
            CondCode::E => "e",
            CondCode::NE => "ne",
            CondCode::G => "g",
            CondCode::GE => "ge",
            CondCode::L => "l",
            CondCode::LE => "le",
            CondCode::A => "a",
            CondCode::AE => "ae",
            CondCode::B => "b",
            CondCode::BE => "be",
        }
    }

    /// Apply indentation rules to a line:
    /// - If the line starts with a dot (e.g. directives) or ends with ":" (labels), do not indent.
    /// - Otherwise, indent with 4 spaces.
    #[inline]
    pub fn apply_indentation(line: &str) -> String {
        if line.is_empty() {
            return line.to_owned();
        }

        if line.starts_with('.') || line.ends_with(':') {
            line.to_owned()
        } else {
            format!("    {}", line)
        }
    }
}
