/* ================ Assembly AST Structure ======================
 *
 * *program = Program(top_level*)
    assembly_type = Byte | Longword | Quadword | ByteArray(int size, int alignment)
    top_level = Function(identifier name, bool global = true, instruction* instructions)
    instruction =
          Mov(assembly_type, operand src, operand dst)
        | MovZeroExtend(assembly_type src_type, assembly_type dst_type,
                        operand src, operand dst)
        | Lea(operand src, operand dst)
        | Unary(unary_operator, assembly_type, operand)
        | Binary(binary_operator, assembly_type, operand, operand)
        | Cmp(assembly_type, operand, operand)
        | Idiv(operand)
        | Cdq
        | Jmp(identifier)
        | JmpCC(cond_code, identifier)
        | SetCC(cond_code, operand)
        | Label(identifier)
        | AllocateStack(int) -- temporary thing probs
        | Push(operand)
        | Call(identifier)
        |
        | Ret
    unary_operator = Neg | Not | Shr
    binary_operator = Add | Sub | Mult | And | Or | Xor (might need this) | Shl | ShrTwoOp
    operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Memory(reg, int) | Data(identifier, int)
    | PseudoMem(identifier, int) | Indexed(reg base, reg index, int scale)
    cond_code = E | NE | G | GE | L | LE | A | AE | B | BE
    reg = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11 | SP | BP
    (missing regs are R12-R15, BX, these are all callee saved registers, will add later)
*/

// Top-level structures
// Use String here for now because i'm getting tired
// and it'd be easier for me probably
// Change later if you want

use middle::wackir::UnaryOp;
use std::fmt::Debug;

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
    Jmp(String),
    JmpCC {
        condition: CondCode,
        label: String,
        is_func: bool, // Field to guide code emission to know if its local or not
    },
    SetCC {
        condition: CondCode,
        operand: Operand,
    },
    Label(String),
    Comment(String),
    // These are high level instructions that serve a purpose
    // of not incurring runtime checks for overflows
    // Alternative would be adding a flag to Binary
    AllocateStack(i32),
    DeallocateStack(i32),
    Push(Operand),
    Pop(Operand),
    // True = external call, false
    Call(String, bool),
    Ret,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(String),
    Memory(Register, i32), // I think is used for array/pair access
    Data(String, i32),     // i think used for RIP relative
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

#[derive(Debug, Clone)]
pub enum AsmBinaryOperator {
    Add,
    Sub,
    Mult,
    And,
    Or,
}

// sadly we don't use any other AsmUnaryOperator's
// -smth is translated as 0 - smth else so it turns into binary
#[derive(Debug, Clone)]
pub enum AsmUnaryOperator {
    Not,
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, Copy)]
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
            UnaryOp::Not => Self::Not,
            _ => panic!("Invalid ASM unary operator"),
        }
    }
}
