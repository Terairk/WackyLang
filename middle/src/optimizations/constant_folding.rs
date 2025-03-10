use crate::wackir::{
    UnaryOp::{self, BNot, Chr, LNot, Len, Negate, Ord},
    WackChar,
    WackInstr::{self, Binary, Copy, JumpIfNotZero, JumpIfZero, JumpToHandler, Unary},
    WackLiteral::{self, Char},
    WackTempIdent,
    WackValue::{self, Literal},
};

use util::gen_flags::{GenFlags, INBUILT_BAD_CHAR, insert_flag_gbl};

// Constants for the bounds of the chr function
const MAX_CHR: i32 = 127;
const MIN_CHR: i32 = 0;

// TODO: figure out what I want to do with runtime errors such as invalid value passed to ord
// and divide by zero. Any compile time errors caused by this won't have source info though
// A few ideas include:
// 1) Compile Time Error via a Panic (simple)
// 2) Compile Time Error via changing signatures to Result<T, E> (more complex) - maybe anyhow?
// 3) Generate a Jmp to a runtime error handler (complex) - need to remove runtime checks
// For now I'll just ignore it and let the runtime handle it. I'm leaning more towards 3) though
// because it's the most flexible and allows for more complex error handling in the future
// Seems more inline with the extensions specification. It's a little complex to figure out right
// now.

/// Constant folds a function. Turns unary and binary operations with constant operands into a
/// single copy instruction
#[inline]
pub(crate) fn constant_fold_function(function_body: Vec<WackInstr>) -> Vec<WackInstr> {
    // TODO: if all helper functions return just one function, change how this function body works
    let mut optimized_body = Vec::new();
    for instr in function_body {
        let new_instrs = match instr {
            Unary { op, src, dst } => eval_unary_op(op, src, dst),
            Binary {
                ref op,
                ref src1,
                ref src2,
                ref dst,
            } => {
                unimplemented!()
            }
            JumpIfZero {
                ref condition,
                ref target,
            } => {
                unimplemented!()
            }
            JumpIfNotZero {
                ref condition,
                ref target,
            } => {
                unimplemented!()
            }
            _ => vec![instr.clone()],
        };

        optimized_body.extend(new_instrs);
    }

    optimized_body
}

fn eval_unary_op(op: UnaryOp, src: WackValue, dst: WackTempIdent) -> Vec<WackInstr> {
    if let WackValue::Literal(value) = src {
        match op {
            Chr => eval_chr(value, dst),
            _ => unimplemented!(),
        }
    } else {
        vec![Unary {
            op,
            src: src.clone(),
            dst: dst.clone(),
        }]
    }
}

fn eval_chr(value: WackLiteral, dst: WackTempIdent) -> Vec<WackInstr> {
    if let WackLiteral::Int(value) = value {
        // chr has some runtime checks which we can do at compile time
        let new_instr = if value >= MIN_CHR && value <= MAX_CHR {
            // Safe to unwrap because we know the value is within bounds
            let char = unsafe { WackChar::from_u8_unchecked(value as u8) };
            Copy {
                src: Literal(Char(char)),
                dst: dst.clone(),
            }
        } else {
            insert_flag_gbl(GenFlags::CHR_BOUNDS);
            JumpToHandler(INBUILT_BAD_CHAR.to_owned())
        };

        vec![new_instr]
    } else {
        panic!("Invalid value passed to chr. Indicates an error in the front-end");
    }
}
