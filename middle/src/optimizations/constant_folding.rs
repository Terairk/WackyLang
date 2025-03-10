use crate::wackir::{
    UnaryOp::{self, BNot, Chr, LNot, Len, Negate, Ord},
    WackBool, WackChar,
    WackInstr::{self, Binary, Copy, JumpIfNotZero, JumpIfZero, JumpToHandler, Unary},
    WackLiteral::{self, Char},
    WackTempIdent,
    WackValue::{self, Literal},
};

use util::gen_flags::{GenFlags, INBUILT_BAD_CHAR, INBUILT_OVERFLOW, insert_flag_gbl};

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
                op,
                src1,
                src2,
                dst,
            } => unimplemented!(),
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
            Chr => eval_chr(&value, dst),
            Ord => {
                if let WackLiteral::Char(value) = value {
                    vec![Copy {
                        src: Literal(WackLiteral::Int(i32::from(value.into_u8()))),
                        dst,
                    }]
                } else {
                    panic!("Invalid value passed to ord. Indicates an error in the front-end");
                }
            }
            Len => {
                if let WackLiteral::StringLit(value) = value {
                    // Safe to unwrap because wacc strings should fit into an i32
                    vec![Copy {
                        src: Literal(WackLiteral::Int(
                            i32::try_from(value.len()).expect("String length is too large"),
                        )),
                        dst,
                    }]
                } else {
                    panic!("Invalid value passed to len. Indicates an error in the front-end");
                }
            }
            Negate => eval_negate(&value, dst),
            BNot => todo!("Implement bitwise not"),
            LNot => {
                let new_instr = match value {
                    WackLiteral::Bool(value) => {
                        let negated: WackBool = (!value.into_bool()).into();
                        Copy {
                            src: Literal(WackLiteral::Bool(negated)),
                            dst,
                        }
                    }
                    _ => {
                        panic!("Invalid value passed to lnot. Indicates an error in the front-end")
                    }
                };

                vec![new_instr]
            }
        }
    } else {
        vec![Unary { op, src, dst }]
    }
}

fn eval_chr(value: &WackLiteral, dst: WackTempIdent) -> Vec<WackInstr> {
    if let WackLiteral::Int(ref value) = *value {
        // chr has some runtime checks which we can do at compile time
        let new_instr = if (MIN_CHR..=MAX_CHR).contains(value) {
            // Safe to unwrap because we know the value is within bounds
            let char = unsafe { WackChar::from_u8_unchecked(*value as u8) };
            Copy {
                src: Literal(Char(char)),
                dst,
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

fn eval_negate(value: &WackLiteral, dst: WackTempIdent) -> Vec<WackInstr> {
    if let WackLiteral::Int(value) = value {
        let new_instr = match value.checked_neg() {
            Some(value) => Copy {
                src: Literal(WackLiteral::Int(value)),
                dst,
            },
            None => {
                insert_flag_gbl(GenFlags::OVERFLOW);
                JumpToHandler(INBUILT_OVERFLOW.to_owned())
            }
        };

        vec![new_instr]
    } else {
        panic!("Invalid value passed to negate. Indicates an error in the front-end");
    }
}
