/* ================ Assembly AST Structure ====================== */

use middle::wackir::{UnaryOp, WackTempIdent};
use std::fmt::Debug;
use util::{Instruction, SimpleInstr};

use crate::registers::RegisterSet;
pub type IsFunction = bool;
// This is a flag to guide code emission to know if it should add a .L_
pub const FUNCTION: IsFunction = true;
pub const LABEL: IsFunction = false;

// If you don't like being reliant on an invariant on the implementation of
// Display on WackTempIdent, make this its own type
// and implement create_new for it
pub type AsmLabel = WackTempIdent;

// implement Debug below
#[derive(Clone)]
pub struct AsmProgram {
    pub asm_functions: Vec<AsmFunction>,
}

#[derive(Debug, Clone)]
pub struct AsmFunction {
    pub name: String,
    pub global: bool,
    pub instructions: Vec<AsmInstruction>,
    pub directives: Vec<Directive>,
    pub regs: RegisterSet, // What registers are passed as parameters, used for liveness analysis
}

#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Mov {
        typ: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    Cmov {
        condition: CondCode,
        typ: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    MovZeroExtend {
        src_type: AssemblyType,
        dst_type: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    Lea {
        src: Operand,
        dst: Operand,
    },
    Unary {
        operator: AsmUnaryOperator,
        typ: AssemblyType,
        operand: Operand,
    },
    Binary {
        operator: AsmBinaryOperator,
        typ: AssemblyType,
        op1: Operand,
        op2: Operand,
    },
    Cmp {
        typ: AssemblyType,
        op1: Operand,
        op2: Operand,
    },
    Test {
        typ: AssemblyType,
        op1: Operand,
        op2: Operand,
    },
    Idiv(Operand), // We convert from Binary(Div, _, _, _) -> IDiv
    Cdq,           // Need this for division
    Jmp(AsmLabel, IsFunction),
    JmpCC {
        condition: CondCode,
        label: AsmLabel,
        is_func: IsFunction, // Field to guide code emission to know if its local or not
    },
    SetCC {
        condition: CondCode,
        operand: Operand,
    },
    Label(AsmLabel),
    Comment(String),
    // These are high level instructions that serve a purpose
    // of not incurring runtime checks for overflows
    // Alternative would be adding a flag to Binary
    AllocateStack(i32),
    DeallocateStack(i32),
    Push(Operand),
    Pop(Register), // You can only pop a register
    // True = external call, false
    Call(String, bool),
    Ret,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(String),
    Memory(Register, i32),
    Data(String, i32), // Used for RIP relative addressing
    Indexed {
        base: Register,
        index: Register,
        scale: i32,
        offset: i32,
    },
}

// For now we only need these directives
// but eventually we may use way more
// so feel free to change this into an Enum
// we only really need a String, we can calculate its length easily
// this is just for representing strings for printing mainly
// Strings are Label + Data and we have a list of these
#[derive(Debug, Clone)]
pub struct Directive(pub &'static str, pub &'static str);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmBinaryOperator {
    Add,
    Sub,
    Mult,
    And,
    Or,
}

// sadly we don't use any other AsmUnaryOperator's
// -smth is translated as 0 - smth else so it turns into binary
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmUnaryOperator {
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CondCode {
    E,  // Equal
    NE, // Not equal
    G,  // Greater
    GE, // Greater or equal
    L,  // Less
    LE, // Less or equal
    A,  // Above
    AE, // Above or equal
    B,  // Below
    BE, // Below or equal
    OF, // Overflow
}

// Other registers are callee saved
// which will be added later
// when optimisations are done
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Register {
    AX,
    BX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    SP,
    BP,
}

#[derive(Debug, Clone, PartialEq, Copy, Eq)]
pub enum AssemblyType {
    Byte,     // 1 byte
    Longword, // 4 bytes
    Quadword, // 8 bytes
}

/* ================ PRETTY PRINTER ============== */
impl Debug for AsmProgram {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "AsmProgram {{")?;
        for function in &self.asm_functions {
            writeln!(f, "  Function {} {{", function.name)?;
            if function.global {
                writeln!(f, "    global: true")?;
            }
            writeln!(f, "    instructions: [")?;
            for instruction in &function.instructions {
                writeln!(f, "      {:?},", instruction)?;
            }
            writeln!(f, "    ]")?;
            writeln!(f, "  }}")?;
        }
        write!(f, "}}")
    }
}

/* ================ ASM Impl's for Conversions or otherwise ============ */

// TODO: this is a place holder, idt we should have Len, Ord, Chr here
impl From<UnaryOp> for AsmUnaryOperator {
    #[inline]
    fn from(op: UnaryOp) -> Self {
        match op {
            UnaryOp::LNot => Self::Not,
            _ => panic!("Invalid ASM unary operator"),
        }
    }
}

impl Instruction for AsmInstruction {
    #[inline]
    fn simplify(&self) -> SimpleInstr {
        use AsmInstruction::{Jmp, JmpCC, Label, Ret};
        match *self {
            Label(ref name) => SimpleInstr::Label(name.clone().into()),
            JmpCC { ref label, .. } => SimpleInstr::ConditionalJump(label.clone().into()),
            Jmp(ref name, LABEL) => SimpleInstr::UnconditionalJump(name.clone().into()),
            Jmp(_, FUNCTION) => SimpleInstr::ErrorJump,
            Ret => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }
}
