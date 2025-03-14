// This file contains the definitions for predefined functions
// that our code will then call.

use crate::assembly_ast::{
    AsmFunction, AsmInstruction, AsmLabel, AsmProgram, AssemblyType, CondCode, Directive, LABEL,
    Operand, Register,
};

use crate::assembly_ast::AsmBinaryOperator::{Add, And, Sub};
use crate::assembly_ast::AsmInstruction::{
    Binary, Call, Cmov, Cmp, Jmp, JmpCC, Label, Lea, Mov, Pop, Push, Ret, Test,
};
use crate::assembly_trans::AsmGen;
use crate::registers::{ARR_INDEX_REG, ARR_LOAD_RETURN, ARR_PTR_REG, RS_ARR, RS1, RegisterSet};
use AssemblyType::{Byte, Longword, Quadword};
use CondCode::{E, GE, L, NE};
use Operand::{Data, Imm, Indexed, Memory, Reg};
use Register::{AX, BP, BX, DI, DX, R10, SI, SP};
use once_cell::sync::Lazy;
use std::collections::HashMap;
use util::gen_flags::{GenFlags, get_flags_gbl, rewrite_global_flag};
use util::gen_flags::{
    INBUILT_ARR_LOAD1, INBUILT_ARR_LOAD4, INBUILT_ARR_LOAD8, INBUILT_BAD_CHAR, INBUILT_DIV_ZERO,
    INBUILT_EXIT, INBUILT_FREE, INBUILT_FREE_PAIR, INBUILT_MALLOC, INBUILT_NULL_ACCESS,
    INBUILT_OOM, INBUILT_OUT_OF_BOUNDS, INBUILT_OVERFLOW, INBUILT_PRINT_BOOL, INBUILT_PRINT_CHAR,
    INBUILT_PRINT_INT, INBUILT_PRINT_PTR, INBUILT_PRINT_STRING, INBUILT_PRINTLN, INBUILT_READ_CHAR,
    INBUILT_READ_INT,
};

#[inline]
pub fn generate_predefined(program: &mut AsmProgram) {
    rewrite_global_flag();
    let flags = get_flags_gbl();
    let new_funcs: Vec<AsmFunction> = PREDEFINED_FUNCTIONS2
        .iter()
        .filter(|&(flag, _)| flags.contains(*flag))
        .map(|(_, func)| func)
        .cloned()
        .collect();

    program.asm_functions.extend(new_funcs);
}

#[inline]
pub fn add_regsets(asm_gen: &mut AsmGen) {
    let function_regs = &mut asm_gen.function_regs;
    rewrite_global_flag();
    let flags = get_flags_gbl();
    let new_funcs: Vec<AsmFunction> = PREDEFINED_FUNCTIONS2
        .iter()
        .filter(|&(flag, _)| flags.contains(*flag))
        .map(|(_, func)| func)
        .cloned()
        .collect();

    for func in new_funcs {
        function_regs.insert(func.name.clone(), func.regs);
    }
}

// TODO change these to be Ident's eventually
pub static PREDEFINED_FUNCTIONS2: Lazy<HashMap<GenFlags, AsmFunction>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(GenFlags::DIV_BY_ZERO, ERR_DIV_ZERO.clone());
    m.insert(GenFlags::MALLOC, MALLOC.clone());
    m.insert(GenFlags::FREE, FREE.clone());
    m.insert(GenFlags::FREE_PAIR, FREEPAIR.clone());
    m.insert(GenFlags::OOM, ERR_OUT_OF_MEMORY.clone());
    m.insert(GenFlags::PRINT_PTR, PRINTP.clone());
    m.insert(GenFlags::PRINT_STR, PRINTS.clone());
    m.insert(GenFlags::PRINT_CHR, PRINTC.clone());
    m.insert(GenFlags::PRINT_BOOLEAN, PRINTB.clone());
    m.insert(GenFlags::PRINT_INT, PRINTI.clone());
    m.insert(GenFlags::PRINT_LN, PRINTLN.clone());
    m.insert(GenFlags::ARRAY_ACCESS1, ARR_LOAD1.clone());
    m.insert(GenFlags::ARRAY_ACCESS4, ARR_LOAD4.clone());
    m.insert(GenFlags::ARRAY_ACCESS8, ARR_LOAD8.clone());
    m.insert(GenFlags::ARR_BOUNDS, ERR_OUT_OF_BOUNDS.clone());
    m.insert(GenFlags::CHR_BOUNDS, ERR_BAD_CHAR.clone());
    m.insert(GenFlags::OVERFLOW, ERR_OVERFLOW.clone());
    m.insert(GenFlags::READ_INT, READI.clone());
    m.insert(GenFlags::READ_CHR, READC.clone());
    m.insert(GenFlags::EXIT, EXIT.clone());
    m.insert(GenFlags::NULL_DEREF, ERR_NULL.clone());
    m
});

/* ================== EXTERNAL C Functions ================= */
// These are the functions that are defined in the C lib that we use
static C_EXIT: &str = "exit";
static C_MALLOC: &str = "malloc";
static C_FREE: &str = "free";
static C_PRINTF: &str = "printf";
static C_FFLUSH: &str = "fflush";
static C_SCANF: &str = "scanf";
static C_PUTS: &str = "puts";

/* ================== HELPER FUNCTIONS ================== */

// Function prologue (creates stack frame, aligns stack to 16 bytes)
fn function_prologue() -> Vec<AsmInstruction> {
    vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
    ]
}

// Function epilogue (restores stack pointer, returns)
fn function_epilogue() -> Vec<AsmInstruction> {
    vec![
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ]
}

// Creates a complete function with proper prologue/epilogue
fn create_function(
    name: String,
    body_instructions: Vec<AsmInstruction>,
    directives: Vec<Directive>,
    registers: RegisterSet,
) -> AsmFunction {
    let mut instructions = function_prologue();
    instructions.extend(body_instructions);
    instructions.extend(function_epilogue());

    AsmFunction {
        name,
        global: false,
        instructions,
        directives,
        regs: registers,
    }
}

static ALIGN_STACK: AsmInstruction = Binary {
    operator: And,
    typ: Quadword,
    op1: Imm(-16),
    op2: Reg(SP),
};

// TODO: If conflicts ever arise, change this to a lazy function which generates
// a usize. For now, we don't do optimizations on these so it should be fine
// For now set to a random number like 13
pub const GEN_USIZE: usize = 13;

/* ================== LABELS ====================== */
static INBUILT_OOM_LABEL: Lazy<AsmLabel> = Lazy::new(|| AsmLabel::new(INBUILT_OOM, GEN_USIZE));
static INBUILT_OOB_LABEL: Lazy<AsmLabel> =
    Lazy::new(|| AsmLabel::new(INBUILT_OUT_OF_BOUNDS, GEN_USIZE));

/* ================== FUNC_DEFINITIONS ============  */
// TODO: Use the above helper functions to redefine the functions below
// TODO: Use the bon crate to make it clearer what the functions are doing

static ERR_DIV_ZERO_STR0: &str = "ERR_DIV_ZERO_STR0";
static ERR_DIV_ZERO: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_DIV_ZERO.to_owned(),
    global: false,
    instructions: vec![
        ALIGN_STACK.clone(),
        Lea {
            src: Data(ERR_DIV_ZERO_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(INBUILT_PRINT_STRING.to_owned(), false),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(C_EXIT.to_owned(), true),
    ],
    directives: vec![Directive(ERR_DIV_ZERO_STR0, "Error: Division by zero\\n")],
    regs: RegisterSet::DI,
});

// Example usage
static MALLOC: Lazy<AsmFunction> = Lazy::new(|| {
    create_function(
        INBUILT_MALLOC.to_owned(),
        vec![
            Call(C_MALLOC.to_owned(), true),
            Cmp {
                typ: Quadword,
                op1: Imm(0),
                op2: Reg(AX),
            },
            JmpCC {
                condition: E,
                label: INBUILT_OOM_LABEL.clone(),
                is_func: true,
            },
        ],
        vec![],
        RS1,
    )
});

static FREE: Lazy<AsmFunction> = Lazy::new(|| {
    create_function(
        INBUILT_FREE.to_owned(),
        vec![Call(C_FREE.to_owned(), true)],
        vec![],
        RS1,
    )
});

static FREEPAIR: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_FREE_PAIR.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Cmp {
            typ: Quadword,
            op1: Imm(0),
            op2: Reg(DI),
        },
        JmpCC {
            condition: E,
            label: AsmLabel::new(INBUILT_NULL_ACCESS, GEN_USIZE),
            is_func: true,
        },
        Call(C_FREE.to_owned(), true), // true indicates external PLT call
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ],
    directives: vec![],
    regs: RS1,
});

static ERR_OUT_OF_MEMORY_STR0: &str = "ERR_OUT_OF_MEMORY_STR0";
static ERR_OUT_OF_MEMORY: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_OOM.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(ERR_OUT_OF_MEMORY_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(INBUILT_PRINT_STRING.to_owned(), false),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(C_EXIT.to_owned(), true),
    ],
    directives: vec![Directive(
        ERR_OUT_OF_MEMORY_STR0,
        "fatal error: out of memory\\n",
    )],
    regs: RS1,
});

static PRINTP_STR0: &str = "PRINTP_STR0";
static PRINTP: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_PRINT_PTR.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Mov {
            typ: Quadword,
            src: Reg(DI),
            dst: Reg(SI),
        },
        Lea {
            src: Data(PRINTP_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call("printf".to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call("fflush".to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ],
    directives: vec![Directive(PRINTP_STR0, "%p")],
    regs: RS1,
});

static PRINTS_STR0: &str = "PRINTS_STR0";
static PRINTS: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_PRINT_STRING.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Mov {
            typ: Quadword,
            src: Reg(DI),
            dst: Reg(DX),
        },
        Mov {
            typ: Longword,
            src: Memory(DI, -4),
            dst: Reg(SI),
        },
        Lea {
            src: Data(PRINTS_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(C_PRINTF.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(C_FFLUSH.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ],
    directives: vec![Directive(PRINTS_STR0, "%.*s")],
    regs: RS1,
});

static PRINTC_STR0: &str = "PRINTC_STR0";
static PRINTC: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_PRINT_CHAR.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Mov {
            typ: Byte,
            src: Reg(DI),
            dst: Reg(SI),
        },
        Lea {
            src: Data(PRINTC_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(C_PRINTF.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(C_FFLUSH.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ],
    directives: vec![Directive(PRINTC_STR0, "%c")],
    regs: RS1,
});

static PRINTB_STR0: &str = "PRINTB_STR0";
static PRINTB_STR1: &str = "PRINTB_STR1";
static PRINTB_STR2: &str = "PRINTB_STR2";
static PRINTB_0_LABEL: Lazy<AsmLabel> = Lazy::new(|| AsmLabel::new("printb0", GEN_USIZE));
static PRINTB_1_LABEL: Lazy<AsmLabel> = Lazy::new(|| AsmLabel::new("printb1", GEN_USIZE));

static PRINTB: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_PRINT_BOOL.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Cmp {
            typ: Byte,
            op1: Imm(0),
            op2: Reg(DI),
        },
        JmpCC {
            condition: NE,
            label: PRINTB_0_LABEL.clone(),
            is_func: false,
        },
        Lea {
            src: Data(PRINTB_STR0.to_owned(), 0),
            dst: Reg(DX),
        },
        Jmp(PRINTB_1_LABEL.to_owned(), LABEL),
        Label(PRINTB_0_LABEL.to_owned()),
        Lea {
            src: Data(PRINTB_STR1.to_owned(), 0),
            dst: Reg(DX),
        },
        Label(PRINTB_1_LABEL.to_owned()),
        Mov {
            typ: Longword,
            src: Memory(DX, -4),
            dst: Reg(SI),
        },
        Lea {
            src: Data(PRINTB_STR2.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(C_PRINTF.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(C_SCANF.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ],
    directives: vec![
        Directive(PRINTB_STR0, "false"),
        Directive(PRINTB_STR1, "true"),
        Directive(PRINTB_STR2, "%.*s"),
    ],
    regs: RS1,
});

static PRINTI_STR0: &str = "PRINTI_STR0";
static PRINTI: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_PRINT_INT.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Mov {
            typ: Longword,
            src: Reg(DI),
            dst: Reg(SI),
        },
        Lea {
            src: Data(PRINTI_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(C_PRINTF.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(C_FFLUSH.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ],
    directives: vec![Directive(PRINTI_STR0, "%d")],
    regs: RS1,
});

static PRINTLN_STR0: &str = "PRINTLN_STR0";
static PRINTLN: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_PRINTLN.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(PRINTLN_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(C_PUTS.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(C_FFLUSH.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ],
    directives: vec![Directive(PRINTLN_STR0, "")],
    regs: RegisterSet::empty(),
});

// TODO: change these to not use R10, R11, what if they stuck with default registers?
fn create_arr_load_function(name: String, scale: i32) -> AsmFunction {
    AsmFunction {
        name,
        global: false,
        instructions: vec![
            Push(Reg(BX)),
            Test {
                typ: Longword,
                op1: Reg(ARR_INDEX_REG),
                op2: Reg(ARR_INDEX_REG),
            },
            Cmov {
                condition: L,
                typ: Quadword,
                src: Reg(ARR_INDEX_REG),
                dst: Reg(R10),
            },
            JmpCC {
                condition: L,
                label: INBUILT_OOB_LABEL.to_owned(),
                is_func: true,
            },
            Mov {
                typ: Longword,
                src: Memory(ARR_PTR_REG, -4),
                dst: Reg(BX),
            },
            Cmp {
                typ: Longword,
                op1: Reg(BX),
                op2: Reg(ARR_INDEX_REG),
            },
            Cmov {
                condition: GE,
                typ: Quadword,
                src: Reg(ARR_INDEX_REG),
                dst: Reg(R10),
            },
            JmpCC {
                condition: GE,
                label: INBUILT_OOB_LABEL.to_owned(),
                is_func: true,
            },
            Lea {
                src: Indexed {
                    offset: 0,
                    base: ARR_PTR_REG,
                    index: ARR_INDEX_REG,
                    scale,
                },
                dst: Reg(ARR_LOAD_RETURN),
            },
            Pop(BX),
            Ret,
        ],
        directives: vec![],
        regs: RS_ARR,
    }
}

// Then use it to define your functions
static ARR_LOAD1: Lazy<AsmFunction> =
    Lazy::new(|| create_arr_load_function(INBUILT_ARR_LOAD1.to_owned(), 1));

static ARR_LOAD4: Lazy<AsmFunction> =
    Lazy::new(|| create_arr_load_function(INBUILT_ARR_LOAD4.to_owned(), 4));

static ARR_LOAD8: Lazy<AsmFunction> =
    Lazy::new(|| create_arr_load_function(INBUILT_ARR_LOAD8.to_owned(), 8));

static ERR_OUT_OF_BOUNDS_STR0: &str = "ERR_OUT_OF_BOUNDS_STR0";
static ERR_OUT_OF_BOUNDS: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_OUT_OF_BOUNDS.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(ERR_OUT_OF_BOUNDS_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(C_PRINTF.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(C_FFLUSH.to_owned(), true),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(C_EXIT.to_owned(), true),
    ],
    directives: vec![Directive(
        ERR_OUT_OF_BOUNDS_STR0,
        "fatal error: array index %d out of bounds",
    )],
    regs: RegisterSet::empty(),
});

static ERR_BAD_CHAR_STR0: &str = "ERR_BAD_CHAR_STR0";
static ERR_BAD_CHAR: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_BAD_CHAR.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(ERR_BAD_CHAR_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(C_PRINTF.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(C_FFLUSH.to_owned(), true),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(C_EXIT.to_owned(), true),
    ],
    directives: vec![Directive(
        ERR_BAD_CHAR_STR0,
        "fatal error: int %d is not ascii character 0-127 \\n",
    )],
    regs: RegisterSet::empty(),
});

static ERR_OVERFLOW_STR0: &str = "ERR_OVERFLOW_STR0";
static ERR_OVERFLOW: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_OVERFLOW.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(ERR_OVERFLOW_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(INBUILT_PRINT_STRING.to_owned(), false),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(C_EXIT.to_owned(), true),
    ],
    directives: vec![Directive(
        ERR_OVERFLOW_STR0,
        "fatal error: integer overflow or underflow occurred\\n",
    )],
    regs: RegisterSet::empty(),
});

fn create_read_function(
    name: String,
    str_const_name: &'static str,
    format_str: &'static str,
    data_type: AssemblyType,
) -> AsmFunction {
    AsmFunction {
        name,
        global: false,
        instructions: vec![
            Push(Reg(BP)),
            Mov {
                typ: Quadword,
                src: Reg(SP),
                dst: Reg(BP),
            },
            Binary {
                operator: And,
                typ: Quadword,
                op1: Imm(-16),
                op2: Reg(SP),
            },
            Binary {
                operator: Sub,
                typ: Quadword,
                op1: Imm(16),
                op2: Reg(SP),
            },
            Mov {
                typ: data_type,
                src: Reg(DI),
                dst: Memory(SP, 0),
            },
            Lea {
                src: Memory(SP, 0),
                dst: Reg(SI),
            },
            Lea {
                src: Data(str_const_name.to_owned(), 0),
                dst: Reg(DI),
            },
            Mov {
                typ: Byte,
                src: Imm(0),
                dst: Reg(AX),
            },
            Call(C_SCANF.to_owned(), true),
            Mov {
                typ: data_type,
                src: Memory(SP, 0),
                dst: Reg(AX),
            },
            Binary {
                operator: Add,
                typ: Quadword,
                op1: Imm(16),
                op2: Reg(SP),
            },
            Mov {
                typ: Quadword,
                src: Reg(BP),
                dst: Reg(SP),
            },
            Pop(BP),
            Ret,
        ],
        directives: vec![Directive(str_const_name, format_str)],
        regs: RS1,
    }
}

static READI_STR0: &str = "READI_STR0";
static READI: Lazy<AsmFunction> =
    Lazy::new(|| create_read_function(INBUILT_READ_INT.to_owned(), READI_STR0, "%d", Longword));

static READC_STR0: &str = "READC_STR0";
static READC: Lazy<AsmFunction> =
    Lazy::new(|| create_read_function(INBUILT_READ_CHAR.to_owned(), READC_STR0, " %c", Byte));

static ERR_NULL_STR0: &str = "ERR_NULL_STR0";
static ERR_NULL: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_NULL_ACCESS.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(ERR_NULL_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(INBUILT_PRINT_STRING.to_owned(), false),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(C_EXIT.to_owned(), true),
    ],
    directives: vec![Directive(
        ERR_NULL_STR0,
        "fatal error: null pair dereferenced or freed\\n",
    )],
    regs: RegisterSet::empty(),
});

static EXIT: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_EXIT.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BP)),
        Mov {
            typ: Quadword,
            src: Reg(SP),
            dst: Reg(BP),
        },
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Call(C_EXIT.to_owned(), true), // true indicates external PLT call
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(BP),
        Ret,
    ],
    directives: vec![],
    regs: RS1,
});
