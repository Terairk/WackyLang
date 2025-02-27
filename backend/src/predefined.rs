// This file contains the definitions for predefined functions
// that our code will then call.

use crate::assembly_ast::{
    AsmBinaryOperator::*, AsmFunction, AsmInstruction, AsmUnaryOperator::*, Directive,
};
use crate::assembly_ast::{
    AsmInstruction::*, AssemblyType::*, CondCode::*, Operand::*, Register::*,
};

use once_cell::sync::Lazy;
use std::collections::HashMap;
use util::gen_flags::GenFlags;

// TODO change these to be Ident's eventually
pub static PREDEFINED_FUNCTIONS2: Lazy<HashMap<GenFlags, &'static AsmFunction>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(GenFlags::DIV_BY_ZERO, &*errDivZero);
    m.insert(GenFlags::MALLOC, &*malloc);
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

/* ================== EXTERNAL C Functions ================= */
// These are the functions that are defined in the C lib that we use
pub static cExit: &str = "exit";
pub static cMalloc: &str = "malloc";
pub static cFree: &str = "free";

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
    name: "_printp".to_owned(),
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
    directives: vec![Directive(prints_str0, "%.*s")],
});

static printc_str0: &str = "printc_str0";
static printc: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: "_printc".to_owned(),
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
    directives: vec![Directive(printc_str0, "%c")],
});

static printb_str0: &str = "printb_str0";
static printb_str1: &str = "printb_str1";
static printb_str2: &str = "printb_str2";
static printb: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: "_printb".to_owned(),
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
    directives: vec![
        Directive(printb_str0, "false"),
        Directive(printb_str1, "true"),
        Directive(printb_str2, "%.*s"),
    ],
});

static printi_str0: &str = "printi_str0";
static printi: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: "_printi".to_owned(),
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
    directives: vec![Directive(printi_str0, "%d")],
});

static println_str0: &str = "println_str0";
static println: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: "_println".to_owned(),
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
        Call("puts".to_owned(), true),
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
    directives: vec![Directive(println_str0, "")],
});

static arrLoad4: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: "_arrLoad4".to_owned(),
    global: false,
    instructions: vec![
        Push(Reg(BX)),
        Cmp {
            typ: Longword,
            op1: Reg(R10),
            op2: Imm(0),
        },
        Cmov {
            condition: L,
            typ: Quadword,
            src: Reg(R10),
            dst: Reg(SI),
        },
        JmpCC {
            condition: L,
            label: "_errOutOfBounds".to_owned(),
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
            label: "_errOutOfBounds".to_owned(),
        },
        Mov {
            typ: Longword,
            src: Indexed {
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

static errOutOfBounds_str0: &str = "errOutOfBounds_str0";
static errOutOfBounds: Lazy<AsmFunction> = Lazy::new(|| AsmFunction {
    name: "_errOutOfBounds".to_owned(),
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
        Call("printf".to_owned(), true),
        Mov {
            typ: Quadword,
            src: Imm(0),
            dst: Reg(DI),
        },
        Call("fflush".to_owned(), true),
        Mov {
            typ: Byte,
            src: Imm(-1),
            dst: Reg(DI),
        },
        Call("exit".to_owned(), true),
    ],
    directives: vec![Directive(
        errOutOfBounds_str0,
        "fatal error: array index %d out of bounds\n",
    )],
});
