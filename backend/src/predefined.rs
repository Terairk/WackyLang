// This file contains the definitions for predefined functions
// that our code will then call.

use crate::assembly_ast::{AsmBinaryOperator::*, AsmFunction, AsmProgram, Directive};
use crate::assembly_ast::{
    AsmInstruction::*, AssemblyType::*, CondCode::*, Operand::*, Register::*,
};

use once_cell::sync::Lazy;
use std::collections::HashMap;
use util::gen_flags::{get_flags_gbl, rewrite_global_flag, GenFlags};

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
    m.insert(GenFlags::ARRAY_STORE1, ARR_STORE1.clone());
    m.insert(GenFlags::ARRAY_STORE4, ARR_STORE4.clone());
    m.insert(GenFlags::ARRAY_STORE8, ARR_STORE8.clone());
    m
});

/* ================== INTERNAL FUNC_NAMES ================== */
// Anytime you want to call an inbuilt function please use the
// constant strings, that way the code is more modular and you
// dont have to change strings in multiple places

pub static INBUILT_DIV_ZERO: &str = "_errDivZero";
pub static INBUILT_PRINT_STRING: &str = "_prints";
pub static INBUILT_MALLOC: &str = "_malloc";
pub static INBUILT_OOM: &str = "_errOutOfMemory";
pub static INBUILT_FREE: &str = "_free";
pub static INBUILT_FREE_PAIR: &str = "_freepair";
pub static INBUILT_NULL_ACCESS: &str = "_errNull";
pub static INBUILT_PRINT_PTR: &str = "_printp";
pub static INBUILT_PRINT_CHAR: &str = "_printc";
pub static INBUILT_PRINT_BOOL: &str = "_printb";
pub static INBUILT_PRINT_INT: &str = "_printi";
pub static INBUILT_PRINTLN: &str = "_println";
pub static INBUILT_ARR_LOAD1: &str = "_arrLoad1";
pub static INBUILT_ARR_LOAD4: &str = "_arrLoad4";
pub static INBUILT_ARR_LOAD8: &str = "_arrLoad8";
pub static INBUILT_OUT_OF_BOUNDS: &str = "_errOutOfBounds";
pub static INBUILT_BAD_CHAR: &str = "_errBadChar";
pub static INBUILT_OVERFLOW: &str = "_errOverflow";
pub static INBUILT_READ_INT: &str = "_readi";
pub static INBUILT_READ_CHAR: &str = "_readc";
pub static INBUILT_EXIT: &str = "_exit";
pub static INBUILT_ARR_STORE1: &str = "_arrStore1";
pub static INBUILT_ARR_STORE4: &str = "_arrStore4";
pub static INBUILT_ARR_STORE8: &str = "_arrStore8";

/* ================== EXTERNAL C Functions ================= */
// These are the functions that are defined in the C lib that we use
static C_EXIT: &str = "exit";
static C_MALLOC: &str = "malloc";
static C_FREE: &str = "free";
static C_PRINTF: &str = "printf";
static C_FFLUSH: &str = "fflush";
static C_SCANF: &str = "scanf";
static C_PUTS: &str = "puts";

/* ================== FUNC_DEFINITIONS ============  */

static ERR_DIV_ZERO_STR0: &str = "ERR_DIV_ZERO_STR0";
static ERR_DIV_ZERO: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_DIV_ZERO.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
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
});

static MALLOC: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_MALLOC.to_owned(),
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
        Call(C_MALLOC.to_owned(), true),
        Cmp {
            typ: Quadword,
            op1: Imm(0),
            op2: Reg(AX),
        },
        JmpCC {
            condition: E,
            label: INBUILT_OOM.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![],
});

static FREE: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_FREE.to_owned(),
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
        Call(C_FREE.to_owned(), true), // true indicates external PLT call
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![],
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
            label: INBUILT_NULL_ACCESS.to_owned(),
            is_func: true,
        },
        Call(C_FREE.to_owned(), true), // true indicates external PLT call
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![],
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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(PRINTP_STR0, "%p")],
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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(PRINTS_STR0, "%.*s")],
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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(PRINTC_STR0, "%c")],
});

static PRINTB_STR0: &str = "PRINTB_STR0";
static PRINTB_STR1: &str = "PRINTB_STR1";
static PRINTB_STR2: &str = "PRINTB_STR2";
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
            label: "printb0".to_owned(),
            is_func: false,
        },
        Lea {
            src: Data(PRINTB_STR0.to_owned(), 0),
            dst: Reg(DX),
        },
        Jmp("printb1".to_owned()),
        Label("printb0".to_owned()),
        Lea {
            src: Data(PRINTB_STR1.to_owned(), 0),
            dst: Reg(DX),
        },
        Label("printb1".to_owned()),
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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![
        Directive(PRINTB_STR0, "false"),
        Directive(PRINTB_STR1, "true"),
        Directive(PRINTB_STR2, "%.*s"),
    ],
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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(PRINTI_STR0, "%d")],
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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(PRINTLN_STR0, "")],
});

static ARR_LOAD1: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_ARR_LOAD1.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BX)),
        Test {
            typ: Longword,
            op1: Reg(R10),
            op2: Reg(R10),
        },
        Cmov {
            condition: L,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: L,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Longword,
            src: Memory(R9, -4),
            dst: Reg(BX),
        },
        Cmp {
            typ: Longword,
            op1: Reg(BX),
            op2: Reg(R10),
        },
        Cmov {
            condition: GE,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: GE,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Lea {
            src: Indexed {
                offset: 0,
                base: R9,
                index: R10,
                scale: 1,
            },
            dst: Reg(R9),
        },
        Pop(Reg(BX)),
        Ret,
    ],
    directives: vec![],
});

static ARR_LOAD4: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_ARR_LOAD4.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BX)),
        Test {
            typ: Longword,
            op1: Reg(R10),
            op2: Reg(R10),
        },
        Cmov {
            condition: L,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: L,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Longword,
            src: Memory(R9, -4),
            dst: Reg(BX),
        },
        Cmp {
            typ: Longword,
            op1: Reg(BX),
            op2: Reg(R10),
        },
        Cmov {
            condition: GE,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: GE,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Lea {
            // typ: Longword,
            src: Indexed {
                offset: 0,
                base: R9,
                index: R10,
                scale: 4,
            },
            dst: Reg(R9),
        },
        Pop(Reg(BX)),
        Ret,
    ],
    directives: vec![],
});

static ARR_LOAD8: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_ARR_LOAD8.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BX)),
        Test {
            typ: Longword,
            op1: Reg(R10),
            op2: Reg(R10),
        },
        Cmov {
            condition: L,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: L,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Longword,
            src: Memory(R9, -4),
            dst: Reg(BX),
        },
        Cmp {
            typ: Longword,
            op1: Reg(BX),
            op2: Reg(R10),
        },
        Cmov {
            condition: GE,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: GE,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Lea {
            src: Indexed {
                offset: 0,
                base: R9,
                index: R10,
                scale: 8,
            },
            dst: Reg(R9),
        },
        Pop(Reg(BX)),
        Ret,
    ],
    directives: vec![],
});

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
});

static READI_STR0: &str = "READI_STR0";
static READI: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_READ_INT.to_owned(),
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
            typ: Longword,
            src: Reg(DI),
            dst: Memory(SP, 0),
        },
        Lea {
            src: Memory(SP, 0),
            dst: Reg(SI),
        },
        Lea {
            src: Data(READI_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(C_SCANF.to_owned(), true),
        Mov {
            typ: Longword,
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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(READI_STR0, "%d")],
});

static READC_STR0: &str = "READC_STR0";
static READC: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_READ_CHAR.to_owned(),
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
            typ: Byte,
            src: Reg(DI),
            dst: Memory(SP, 0),
        },
        Lea {
            src: Memory(SP, 0),
            dst: Reg(SI),
        },
        Lea {
            src: Data(READC_STR0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(C_SCANF.to_owned(), true),
        Mov {
            typ: Byte,
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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(READC_STR0, " %c")],
});

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
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![],
});

static ARR_STORE4: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_ARR_STORE1.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BX)),
        Test {
            typ: Longword,
            op1: Reg(R10),
            op2: Reg(R10),
        },
        Cmov {
            condition: L,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: L,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Longword,
            src: Memory(R9, -4),
            dst: Reg(BX),
        },
        Cmp {
            typ: Longword,
            op1: Reg(BX),
            op2: Reg(R10),
        },
        Cmov {
            condition: GE,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: GE,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Longword,
            src: Reg(AX),
            dst: Indexed {
                offset: 0,
                base: R9,
                index: R10,
                scale: 4,
            },
        },
        Pop(Reg(BX)),
        Ret,
    ],
    directives: vec![],
});

static ARR_STORE1: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_ARR_STORE4.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BX)),
        Test {
            typ: Longword,
            op1: Reg(R10),
            op2: Reg(R10),
        },
        Cmov {
            condition: L,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: L,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Longword,
            src: Memory(R9, -4),
            dst: Reg(BX),
        },
        Cmp {
            typ: Longword,
            op1: Reg(BX),
            op2: Reg(R10),
        },
        Cmov {
            condition: GE,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: GE,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Byte,
            src: Reg(AX),
            dst: Indexed {
                offset: 0,
                base: R9,
                index: R10,
                scale: 1,
            },
        },
        Pop(Reg(BX)),
        Ret,
    ],
    directives: vec![],
});

static ARR_STORE8: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: INBUILT_ARR_STORE4.to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BX)),
        Test {
            typ: Longword,
            op1: Reg(R10),
            op2: Reg(R10),
        },
        Cmov {
            condition: L,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: L,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Longword,
            src: Memory(R9, -4),
            dst: Reg(BX),
        },
        Cmp {
            typ: Longword,
            op1: Reg(BX),
            op2: Reg(R10),
        },
        Cmov {
            condition: GE,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: GE,
            label: INBUILT_OUT_OF_BOUNDS.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Quadword,
            src: Reg(AX),
            dst: Indexed {
                offset: 0,
                base: R9,
                index: R10,
                scale: 8,
            },
        },
        Pop(Reg(BX)),
        Ret,
    ],
    directives: vec![],
});
