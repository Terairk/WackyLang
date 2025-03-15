use core::panic;
use std::collections::BTreeMap;

use crate::assembly_ast::{
    AsmBinaryOperator, AsmFunction, AsmInstruction, AsmLabel, AsmProgram, AsmUnaryOperator,
    AssemblyType, CondCode, Directive, Operand, Register,
};

pub struct AssemblyFormatter;

// This might be a temporary thing but I'll put this into emission phase for now
// as we always want to do this anyways

impl AssemblyFormatter {
    // Format an entire program by printing each function.

    #[inline]
    pub fn format_program(program: &AsmProgram, str_literals: BTreeMap<String, String>) -> String {
        let mut output = String::new();
        // Printing string literals with their respective lengths
        output.push_str(".section .rodata\n");
        for (string, label) in str_literals {
            output.push_str(format!("# length of {}\n", label).as_str());
            output.push_str(format!("   .int {}\n", string.len()).as_str());
            output.push_str(format!(".L_{}:\n", label).as_str());
            let string = escape_string(&string);
            output.push_str(format!("   .asciz \"{}\"\n", string).as_str());
        }

        // have a .text directive here once per assembly file
        for func in &program.asm_functions {
            output.push_str(&Self::format_function(func));
        }

        output
    }
    // Format a single function. This prints any global,
    // a label for the function, the instructions and adds a blank line at the end.
    #[inline]
    pub fn format_function(func: &AsmFunction) -> String {
        let mut lines: Vec<String> = Vec::new();
        if !func.directives.is_empty() {
            // If there are no directives, we can skip the .section directive.
            lines.push(".section .rodata".to_owned());
        }

        for directive in &func.directives {
            let Directive(label, value) = *directive;
            lines.push(format!("# length of {label}"));
            lines.push(format!("    .int {}", value.len()));
            lines.push(format!(".L_{label}:"));
            // escape string here as the string might have special characters
            let value = escape_string(value);
            lines.push(format!("    .asciz \"{value}\""));
        }
        lines.push(".text".to_owned());
        if func.global {
            // Global directives start with a dot, so they will have no indent.
            lines.push(format!(".globl {}", func.name));
        }
        // Print function label (ends with ":" so we omit the indent).
        lines.push(format!("{}:", func.name));
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
        use crate::assembly_ast::AsmInstruction::{
            AllocateStack, Binary, Call, Cdq, Cmov, Cmp, Comment, DeallocateStack, Idiv, Jmp,
            JmpCC, Label, Lea, Mov, MovZeroExtend, Pop, Push, Ret, SetCC, Test, Unary,
        };
        match *inst {
            Mov {
                typ,
                ref src,
                ref dst,
            } => {
                let suffix = Self::assembly_type_suffix(typ);
                let src_str = Self::format_operand(src, typ);
                let dst_str = Self::format_operand(dst, typ);
                format!("mov{} {}, {}", suffix, src_str, dst_str)
            }
            Cmov {
                condition,
                typ,
                ref src,
                ref dst,
            } => {
                let condition = Self::format_cond_code(condition);
                let src_str = Self::format_operand(src, typ);
                let dst_str = Self::format_operand(dst, typ);
                format!("cmov{} {}, {}", condition, src_str, dst_str)
            }
            MovZeroExtend {
                src_type,
                dst_type,
                ref src,
                ref dst,
            } => {
                // Using the destination type to determine the suffix.
                let src_suffix = Self::assembly_type_suffix(src_type);
                let suffix = Self::assembly_type_suffix(dst_type);
                let src_str = Self::format_operand(src, src_type);
                let dst_str = Self::format_operand(dst, dst_type);
                format!("movz{}{} {}, {}", src_suffix, suffix, src_str, dst_str)
            }
            Lea { ref src, ref dst } => {
                let src_str = Self::format_operand(src, AssemblyType::Quadword);
                let dst_str = Self::format_operand(dst, AssemblyType::Quadword);
                format!("leaq {}, {}", src_str, dst_str)
            }
            Unary {
                operator,
                typ,
                ref operand,
            } => {
                let suffix = Self::assembly_type_suffix(typ);
                let operand_str = Self::format_operand(operand, typ);
                let op = Self::format_unary_operator(operator);
                format!("{op}{suffix} {operand_str}")
            }
            Binary {
                operator,
                typ,
                ref op1,
                ref op2,
            } => {
                let suffix = Self::assembly_type_suffix(typ);
                let op1_str = Self::format_operand(op1, typ);
                let op2_str = Self::format_operand(op2, typ);
                let op = Self::format_binary_operator(operator);

                // For now its just arithmetic operation
                // So we just handle overflow here

                format!("{op}{suffix} {op1_str}, {op2_str}")
            }
            Cmp {
                typ,
                ref op1,
                ref op2,
            } => {
                let suffix = Self::assembly_type_suffix(typ);
                let op1_str = Self::format_operand(op1, typ);
                let op2_str = Self::format_operand(op2, typ);
                format!("cmp{} {}, {}", suffix, op1_str, op2_str)
            }
            Test {
                typ,
                ref op1,
                ref op2,
            } => {
                let suffix = Self::assembly_type_suffix(typ);
                let op1_str = Self::format_operand(op1, typ);
                let op2_str = Self::format_operand(op2, typ);
                format!("test{} {}, {}", suffix, op1_str, op2_str)
            }
            Idiv(ref op) => {
                // idivl because we only work with ints in WACC
                let op_str = Self::format_operand(op, AssemblyType::Longword);
                format!("idivl {op_str}")
            }
            Cdq => "cdq".to_owned(),
            Jmp(ref label, is_function) => {
                let label = Self::format_asm_label(label.clone(), is_function);
                if is_function {
                    return format!("jmp {label}");
                }
                format!("jmp .L_{label}")
            }
            JmpCC {
                condition,
                ref label,
                is_func,
            } => {
                let cond = Self::format_cond_code(condition);
                let label = Self::format_asm_label(label.clone(), is_func);
                if is_func {
                    return format!("j{cond} {label}");
                }
                format!("j{cond} .L_{label}")
            }
            SetCC {
                condition,
                ref operand,
            } => {
                let cond = Self::format_cond_code(condition);
                let op_str = Self::format_operand(operand, AssemblyType::Byte);
                format!("set{} {}", cond, op_str)
            }
            Label(ref name) => {
                let label = Self::format_asm_label(name.clone(), false);
                format!(".L_{label}:")
            }
            AllocateStack(bytes) => {
                // In AT&T syntax, stack allocation is often performed by subtracting from %rsp.
                format!("subq ${bytes}, %rsp")
            }
            Push(ref op) => {
                let op_str = Self::format_operand(op, AssemblyType::Quadword);
                format!("push {op_str}")
            }
            Pop(reg) => {
                let op_str = Self::format_register(reg, AssemblyType::Quadword);
                format!("pop {op_str}")
            }
            // Note these are our own calls so these don't need to be with @PLT
            // all @PLT external calls are already pre-generated
            Call(ref name, external) => {
                if external {
                    format!("call {name}@PLT")
                } else {
                    format!("call {name}")
                }
            }
            Ret => "ret".to_owned(),
            DeallocateStack(bytes) => {
                // In AT&T syntax, stack deallocation is often performed by adding to %rsp.
                format!("addq ${bytes}, %rsp")
            }
            Comment(ref comment) => format!("# {comment}"),
        }
    }

    // Return the appropriate suffix for the given AssemblyType.
    // (e.g. "b" for byte, "l" for longword, "q" for quadword)
    #[inline]
    pub fn assembly_type_suffix(typ: AssemblyType) -> &'static str {
        use AssemblyType::{Byte, Longword, Quadword};
        match typ {
            Byte => "b",
            Longword => "l",
            Quadword => "q",
        }
    }

    /// Format an operand into its AT&T assembly representation.
    #[inline]
    pub fn format_operand(op: &Operand, typ: AssemblyType) -> String {
        use Operand::{Data, Imm, Indexed, Memory, Pseudo, Reg};
        match *op {
            Imm(ref val) => format!("${}", val),
            Reg(reg) => Self::format_register(reg, typ),
            Memory(reg, offset) => {
                // Format as: offset(%reg) (with no offset if the value is 0)
                let reg_str = Self::format_register(reg, AssemblyType::Quadword);
                if offset == 0 {
                    format!("({})", reg_str)
                } else {
                    format!("{}({})", offset, reg_str)
                }
            }
            Data(ref label, offset) => {
                // RIP-relative addressing for data.
                if offset == 0 {
                    format!(".L_{}(%rip)", label)
                } else {
                    format!(".L_{}+{}(%rip)", label, offset)
                }
            }
            Indexed {
                offset,
                base,
                index,
                scale,
            } => {
                // Format: offset(base, index, scale)
                let reg1 = Self::format_register(base, AssemblyType::Quadword);
                let reg2 = Self::format_register(index, AssemblyType::Quadword);
                let offset_str = if offset == 0 {
                    String::new()
                } else {
                    format!("{}", offset)
                };
                if scale == 1 {
                    return format!("{offset_str}({reg1}, {reg2})");
                }
                format!("{offset_str}({reg1}, {reg2}, {scale})")
            }

            // Printing out pseudoregisters is only for debugging,
            // these should not occur in regular assembly
            Pseudo(ref name) => name.clone(),
        }
    }

    #[must_use]
    #[inline]
    pub fn format_register(reg: Register, typ: AssemblyType) -> String {
        use AssemblyType::{Byte, Longword, Quadword};
        match typ {
            Byte => Self::format_byte_reg(reg).to_owned(),
            Longword => Self::format_long_reg(reg).to_owned(),
            Quadword => Self::format_quad_reg(reg).to_owned(),
        }
    }

    /// Format a register – adds a "%" prefix and maps our enum to the 64-bit register names.
    const fn format_quad_reg(reg: Register) -> &'static str {
        use Register::{AX, BP, BX, CX, DI, DX, R8, R9, R10, R11, R12, R13, R14, R15, SI, SP};
        match reg {
            AX => "%rax",
            BX => "%rbx",
            CX => "%rcx",
            DX => "%rdx",
            DI => "%rdi",
            SI => "%rsi",
            R8 => "%r8",
            R9 => "%r9",
            R10 => "%r10",
            R11 => "%r11",
            R12 => "%r12",
            R13 => "%r13",
            R14 => "%r14",
            R15 => "%r15",
            SP => "%rsp",
            BP => "%rbp",
        }
    }

    // 32 bit registers
    fn format_long_reg(reg: Register) -> &'static str {
        use Register::{AX, BP, BX, CX, DI, DX, R8, R9, R10, R11, R12, R13, R14, R15, SI, SP};
        match reg {
            AX => "%eax",
            BX => "%ebx",
            CX => "%ecx",
            DX => "%edx",
            DI => "%edi",
            SI => "%esi",
            R8 => "%r8d",
            R9 => "%r9d",
            R10 => "%r10d",
            R11 => "%r11d",
            R12 => "%r12d",
            R13 => "%r13d",
            R14 => "%r14d",
            R15 => "%r15d",
            SP => panic!("Stack pointer cannot be 32-bit"),
            BP => panic!("Base pointer cannot be 32-bit"),
        }
    }

    #[inline]
    // 8 bit registers
    fn format_byte_reg(reg: Register) -> &'static str {
        use Register::{AX, BP, BX, CX, DI, DX, R8, R9, R10, R11, R12, R13, R14, R15, SI, SP};
        match reg {
            AX => "%al",
            BX => "%bl",
            CX => "%cl",
            DX => "%dl",
            DI => "%dil",
            SI => "%sil",
            R8 => "%r8b",
            R9 => "%r9b",
            R10 => "%r10b",
            R11 => "%r11b",
            R12 => "%r12b",
            R13 => "%r13b",
            R14 => "%r14b",
            R15 => "%r15b",
            SP => panic!("Stack pointer cannot be 8-bit"),
            BP => panic!("Base pointer cannot be 8-bit"),
        }
    }

    /// Format the binary operator as the proper mnemonic.
    #[inline]
    #[must_use]
    pub const fn format_binary_operator(op: AsmBinaryOperator) -> &'static str {
        use crate::assembly_ast::AsmBinaryOperator::{Add, And, Mult, Or, Sub};
        match op {
            Add => "add",
            Sub => "sub",
            Mult => "imul", // AT&T multiplication
            And => "and",
            Or => "or",
        }
    }

    /// Format the unary operator as the proper mnemonic.
    #[inline]
    #[must_use]
    pub const fn format_unary_operator(op: AsmUnaryOperator) -> &'static str {
        use AsmUnaryOperator::Not;
        match op {
            Not => "not",
        }
    }

    /// Map your condition codes to the AT&T conditional suffix.
    #[inline]
    #[must_use]
    pub const fn format_cond_code(cc: CondCode) -> &'static str {
        use CondCode::{A, AE, B, BE, E, G, GE, L, LE, NE, OF};
        match cc {
            E => "e",
            NE => "ne",
            G => "g",
            GE => "ge",
            L => "l",
            LE => "le",
            A => "a",
            AE => "ae",
            B => "b",
            BE => "be",
            OF => "o",
        }
    }

    #[inline]
    #[must_use]
    pub fn format_asm_label(label: AsmLabel, is_func: bool) -> String {
        let id = label.get_id();
        let name = label.get_str();
        if is_func {
            return format!("{name}");
        }
        format!("{name}_{id}")
    }

    /// Apply indentation rules to a line:
    /// - If the line starts with a dot (e.g. directives) or ends with ":" (labels), do not indent.
    /// - Otherwise, indent with 4 spaces.
    #[inline]
    #[must_use]
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

fn escape_string(s: &str) -> String {
    // println!("Escaping string: {s}");
    // replace " with \"
    s.replace("\"", "\\\"").replace("\n", "\\n")
}

use std::fmt;

impl fmt::Display for AsmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Reuse the existing format_instruction method
        write!(f, "{}", AssemblyFormatter::format_instruction(self))
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Reuse the existing format_operand method
        write!(
            f,
            "{}",
            AssemblyFormatter::format_operand(self, AssemblyType::Quadword)
        )
    }
}
