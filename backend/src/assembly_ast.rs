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

#[derive(Debug, Clone)]
pub struct AsmProgram {
    pub top_level: Vec<AsmFunction>,
}

#[derive(Debug, Clone)]
pub struct AsmFunction {
    pub name: String,
    pub global: bool,
    pub external: bool,
    pub instructions: Vec<AsmInstruction>,
}

#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Mov {
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
    Idiv(Operand),
    Cdq,
    Jmp(String),
    JmpCC {
        condition: CondCode,
        label: String,
    },
    SetCC {
        condition: CondCode,
        operand: Operand,
    },
    Label(String),
    AllocateStack(i32),
    Push(Operand),
    Call(String),
    Ret,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(String),
    Memory(Register, i32),
    Data(String, i32),
    PseudoMem(String, i32),
    Indexed {
        base: Register,
        index: Register,
        scale: i32,
    },
}

#[derive(Debug, Clone)]
pub enum AsmBinaryOperator {
    Add,
    Sub,
    Mult,
    And,
    Or,
    Xor,
    Shl,
    ShrTwoOp,
}

#[derive(Debug, Clone)]
pub enum AsmUnaryOperator {
    Neg,
    Not,
    Shr,
}

#[derive(Debug, Clone)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    A,
    AE,
    B,
    BE,
}

#[derive(Debug, Clone)]
pub enum Register {
    AX,
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

#[derive(Debug, Clone)]
pub enum AssemblyType {
    Byte,
    Longword,
    Quadword,
    ByteArray { size: i32, alignment: i32 },
}
