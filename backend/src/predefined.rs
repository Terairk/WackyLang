// This file contains the definitions for predefined functions
// that our code will then call.

use crate::assembly_ast::{
    AsmBinaryOperator::*, AsmFunction, AsmInstruction, AsmProgram, AsmUnaryOperator::*, Directive,
};
use crate::assembly_ast::{
    AsmInstruction::*, AssemblyType::*, CondCode::*, Operand::*, Register::*,
};

use once_cell::sync::Lazy;
use std::collections::HashMap;
use util::gen_flags::{GenFlags, get_flags_gbl, rewrite_global_flag};

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
    m.insert(GenFlags::DIV_BY_ZERO, errDivZero.clone());
    m.insert(GenFlags::MALLOC, malloc.clone());
    m.insert(GenFlags::FREE, free.clone());
    m.insert(GenFlags::FREE_PAIR, freepair.clone());
    m.insert(GenFlags::OOM, errOutOfMemory.clone());
    m.insert(GenFlags::PRINT_PTR, printp.clone());
    m.insert(GenFlags::PRINT_STR, prints.clone());
    m.insert(GenFlags::PRINT_CHR, printc.clone());
    m.insert(GenFlags::PRINT_BOOLEAN, printb.clone());
    m.insert(GenFlags::PRINT_INT, printi.clone());
    m.insert(GenFlags::PRINT_LN, println.clone());
    m.insert(GenFlags::ARRAY_ACCESS1, arrLoad1.clone());
    m.insert(GenFlags::ARRAY_ACCESS4, arrLoad4.clone());
    m.insert(GenFlags::ARRAY_ACCESS8, arrLoad8.clone());
    m.insert(GenFlags::ARR_BOUNDS, errOutOfBounds.clone());
    m.insert(GenFlags::CHR_BOUNDS, errBadChar.clone());
    m.insert(GenFlags::OVERFLOW, errOverflow.clone());
    m.insert(GenFlags::READ_INT, readi.clone());
    m.insert(GenFlags::READ_CHR, readc.clone());
    m.insert(GenFlags::EXIT, exit.clone());
    m
});

/* ================== INTERNAL FUNC_NAMES ================== */
// Anytime you want to call an inbuilt function please use the
// constant strings, that way the code is more modular and you
// dont have to change strings in multiple places

pub static inbuiltDivZero: &str = "_errDivZero";
pub static inbuiltPrintString: &str = "_prints";
pub static inbuiltMalloc: &str = "_malloc";
pub static inbuiltOOM: &str = "_errOutOfMemory";
pub static inbuiltFree: &str = "_free";
pub static inbuiltFreePair: &str = "_freepair";
pub static inbuiltNullAccess: &str = "_errNull";
pub static inbuiltPrintPtr: &str = "_printp";
pub static inbuiltPrintChar: &str = "_printc";
pub static inbuiltPrintBool: &str = "_printb";
pub static inbuiltPrintInt: &str = "_printi";
pub static inbuiltPrintln: &str = "_println";
pub static inbuiltArrLoad1: &str = "_arrLoad1";
pub static inbuiltArrLoad4: &str = "_arrLoad4";
pub static inbuiltArrLoad8: &str = "_arrLoad8";
pub static inbuiltOutOfBounds: &str = "_errOutOfBounds";
pub static inbuiltBadChar: &str = "_errBadChar";
pub static inbuiltOverflow: &str = "_errOverflow";
pub static inbuiltReadInt: &str = "_readi";
pub static inbuiltReadChar: &str = "_readc";
pub static inbuiltExit: &str = "_exit";

/* ================== EXTERNAL C Functions ================= */
// These are the functions that are defined in the C lib that we use
static cExit: &str = "exit";
static cMalloc: &str = "malloc";
static cFree: &str = "free";
static cPrintf: &str = "printf";
static cFflush: &str = "fflush";
static cScanf: &str = "scanf";
static cPuts: &str = "puts";

/* ================== FUNC_DEFINITIONS ============  */

static errDivZero_str0: &str = "errDivZero_str0";
static errDivZero: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltDivZero.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(errDivZero_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(inbuiltPrintString.to_owned(), false),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(cExit.to_owned(), true),
    ],
    directives: vec![Directive(errDivZero_str0, "Error: Division by zero\\n")],
});

static malloc: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltMalloc.to_owned(),
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
        Call(cMalloc.to_owned(), true),
        Cmp {
            typ: Quadword,
            op1: Imm(0),
            op2: Reg(AX),
        },
        JmpCC {
            condition: E,
            label: inbuiltOOM.to_owned(),
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

static free: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltFree.to_owned(),
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
        Call(cFree.to_owned(), true), // true indicates external PLT call
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

static freepair: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltFreePair.to_owned(),
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
            label: inbuiltNullAccess.to_owned(),
            is_func: true,
        },
        Call(cFree.to_owned(), true), // true indicates external PLT call
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

static errOutOfMemory_str0: &str = "errOutOfMemory_str0";
static errOutOfMemory: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltOOM.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(errOutOfMemory_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(inbuiltPrintString.to_owned(), false),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(cExit.to_owned(), true),
    ],
    directives: vec![Directive(
        errOutOfMemory_str0,
        "fatal error: out of memory\\n",
    )],
});

static printp_str0: &str = "printp_str0";
static printp: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltPrintPtr.to_owned(),
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
            src: Data(printp_str0.to_owned(), 0),
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
    directives: vec![Directive(printp_str0, "%p")],
});

static prints_str0: &str = "prints_str0";
static prints: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltPrintString.to_owned(),
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
            src: Data(prints_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(cPrintf.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(cFflush.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(prints_str0, "%.*s")],
});

static printc_str0: &str = "printc_str0";
static printc: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltPrintChar.to_owned(),
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
            src: Data(printc_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(cPrintf.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(cFflush.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(printc_str0, "%c")],
});

static printb_str0: &str = "printb_str0";
static printb_str1: &str = "printb_str1";
static printb_str2: &str = "printb_str2";
static printb: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltPrintBool.to_owned(),
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
            src: Data(printb_str0.to_owned(), 0),
            dst: Reg(DX),
        },
        Jmp("printb1".to_owned()),
        Label("printb0".to_owned()),
        Lea {
            src: Data(printb_str1.to_owned(), 0),
            dst: Reg(DX),
        },
        Label("printb1".to_owned()),
        Mov {
            typ: Longword,
            src: Memory(DX, -4),
            dst: Reg(SI),
        },
        Lea {
            src: Data(printb_str2.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(cPrintf.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(cScanf.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![
        Directive(printb_str0, "false"),
        Directive(printb_str1, "true"),
        Directive(printb_str2, "%.*s"),
    ],
});

static printi_str0: &str = "printi_str0";
static printi: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltPrintInt.to_owned(),
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
            src: Data(printi_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(cPrintf.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(cFflush.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(printi_str0, "%d")],
});

static println_str0: &str = "println_str0";
static println: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltPrintln.to_owned(),
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
            src: Data(println_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(cPuts.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(cFflush.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Reg(BP),
            dst: Reg(SP),
        },
        Pop(Reg(BP)),
        Ret,
    ],
    directives: vec![Directive(println_str0, "")],
});

static arrLoad1: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltArrLoad1.to_owned(),
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
            label: inbuiltOutOfBounds.to_owned(),
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
            label: inbuiltOutOfBounds.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Byte,
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

static arrLoad4: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltArrLoad4.to_owned(),
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
            label: inbuiltOutOfBounds.to_owned(),
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
            label: inbuiltOutOfBounds.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Longword,
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

static arrLoad8: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltArrLoad8.to_owned(),
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
            label: inbuiltOutOfBounds.to_owned(),
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
            label: inbuiltOutOfBounds.to_owned(),
            is_func: true,
        },
        Mov {
            typ: Quadword,
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

static errOutOfBounds_str0: &str = "errOutOfBounds_str0";
static errOutOfBounds: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltOutOfBounds.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(errOutOfBounds_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(cPrintf.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(cFflush.to_owned(), true),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(cExit.to_owned(), true),
    ],
    directives: vec![Directive(
        errOutOfBounds_str0,
        "fatal error: array index %d out of bounds",
    )],
});

static errBadChar_str0: &str = "errBadChar_str0";
static errBadChar: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltBadChar.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(errBadChar_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(cPrintf.to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call(cFflush.to_owned(), true),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(cExit.to_owned(), true),
    ],
    directives: vec![Directive(
        errBadChar_str0,
        "fatal error: int %d is not ascii character 0-127 \n",
    )],
});

static errOverflow_str0: &str = "errOverflow_str0";
static errOverflow: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltOverflow.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(errOverflow_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(inbuiltPrintString.to_owned(), false),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(cExit.to_owned(), true),
    ],
    directives: vec![Directive(
        errOverflow_str0,
        "fatal error: integer overflow or underflow occurred\\n",
    )],
});

static readi_str0: &str = "readi_str0";
static readi: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltReadInt.to_owned(),
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
            src: Data(readi_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(cScanf.to_owned(), true),
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
    directives: vec![Directive(readi_str0, "%d")],
});

static readc_str0: &str = "readc_str0";
static readc: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltReadChar.to_owned(),
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
            src: Reg(SP),
            dst: Reg(SI),
        },
        Lea {
            src: Data(readc_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Mov {
            typ: Byte,
            src: Imm(0),
            dst: Reg(AX),
        },
        Call(cScanf.to_owned(), true),
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
    directives: vec![Directive(readc_str0, " %c")],
});

static errNull_str0: &str = "errNull_str0";
static errNull: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltNullAccess.to_owned(),
    global: false,
    instructions: vec![
        Binary {
            operator: And,
            typ: Quadword,
            op1: Imm(-16),
            op2: Reg(SP),
        },
        Lea {
            src: Data(errNull_str0.to_owned(), 0),
            dst: Reg(DI),
        },
        Call(inbuiltPrintString.to_owned(), false),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call(cExit.to_owned(), true),
    ],
    directives: vec![Directive(
        errNull_str0,
        "fatal error: null pair dereferenced or freed\\n",
    )],
});

static exit: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: inbuiltExit.to_owned(),
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
        Call(cExit.to_owned(), true), // true indicates external PLT call
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
