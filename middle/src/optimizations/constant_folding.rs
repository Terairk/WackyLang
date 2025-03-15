use crate::wackir::{
    WackInstr::{
        self, Binary, FreeChecked, Jump, JumpIfNotZero, JumpIfZero, JumpToHandler, NullPtrGuard,
        Unary,
    },
    WackLiteral::{self},
    WackValue::Literal,
};

use binary_op::eval_binary_op;
use unary_op::eval_unary_op;
use util::gen_flags::{insert_flag_gbl, GenFlags, INBUILT_NULL_ACCESS};

/// Constant folds a function. Turns unary and binary operations with constant operands into a
/// single copy instruction
#[inline]
pub fn constant_fold_function(function_body: Vec<WackInstr>) -> Vec<WackInstr> {
    // TODO: if all helper functions return just one function, change how this function body works
    // seems like its only one instruction each but I'll leave it as Vec for more flexibility
    // println!("Optimizing function");
    let mut optimized_body = Vec::new();
    for instr in function_body {
        let new_instrs = match instr {
            Unary { op, src, dst } => eval_unary_op(op, src, dst),
            Binary {
                op,
                src1,
                src2,
                dst,
            } => eval_binary_op(op, src1, src2, dst),
            JumpIfZero { condition, target } => {
                if let Literal(WackLiteral::Bool(value)) = condition {
                    if value { vec![] } else { vec![Jump(target)] }
                } else {
                    vec![JumpIfZero { condition, target }]
                }
            }
            JumpIfNotZero { condition, target } => {
                if let Literal(WackLiteral::Bool(value)) = condition {
                    if value { vec![Jump(target)] } else { vec![] }
                } else {
                    vec![JumpIfNotZero { condition, target }]
                }
            }
            FreeChecked(ptr) => {
                if let Literal(WackLiteral::NullPair) = ptr {
                    insert_flag_gbl(GenFlags::NULL_DEREF);
                    vec![JumpToHandler(INBUILT_NULL_ACCESS.to_owned())]
                } else {
                    vec![FreeChecked(ptr)]
                }
            }
            NullPtrGuard(ptr) => {
                if let Literal(WackLiteral::NullPair) = ptr {
                    insert_flag_gbl(GenFlags::NULL_DEREF);
                    vec![JumpToHandler(INBUILT_NULL_ACCESS.to_owned())]
                } else {
                    vec![NullPtrGuard(ptr)]
                }
            }

            _ => vec![instr.clone()],
        };

        optimized_body.extend(new_instrs);
    }

    optimized_body
}

pub mod unary_op {
    use crate::optimizations::constant_folding::insert_flag_gbl;
    use crate::wackir::{UnaryOp, WackBool, WackInstr, WackLiteral, WackTempIdent, WackValue};
    use util::gen_flags::{GenFlags, INBUILT_BAD_CHAR, INBUILT_OVERFLOW};
    use UnaryOp::{Chr, LNot, Len, Negate, Ord};
    use WackInstr::{Copy, JumpToHandler, Unary};
    use WackLiteral::Char;
    use WackValue::Literal;

    // Constants for the bounds of the chr function
    const MAX_CHR: i32 = 127;
    const MIN_CHR: i32 = 0;
    pub fn eval_unary_op(op: UnaryOp, src: WackValue, dst: WackTempIdent) -> Vec<WackInstr> {
        if let WackValue::Literal(value) = src {
            match op {
                Chr => eval_chr(&value, dst),
                Ord => {
                    if let WackLiteral::Char(value) = value {
                        vec![Copy {
                            src: Literal(WackLiteral::Int(i32::from(value))),
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
                LNot => {
                    let new_instr = match value {
                        WackLiteral::Bool(value) => {
                            let negated: WackBool = (!value).into();
                            Copy {
                                src: Literal(WackLiteral::Bool(negated)),
                                dst,
                            }
                        }
                        _ => {
                            panic!(
                                "Invalid value passed to lnot. Indicates an error in the front-end"
                            )
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
                // Safe to unwrap because we know the value is within bounds via parser
                let char =
                    u8::try_from(*value).expect("Our checks above should ensure this is safe");
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
}

pub mod binary_op {
    use crate::optimizations::constant_folding::insert_flag_gbl;
    use crate::wackir::{BinaryOp, WackBool, WackInstr, WackLiteral, WackTempIdent, WackValue};
    use std::cmp::Ordering;
    use util::gen_flags::{GenFlags, INBUILT_DIV_ZERO, INBUILT_OVERFLOW};
    use BinaryOp::{Add, Div, Eq, Gt, Gte, LAnd, LOr, Lt, Lte, Mod, Mul, Neq, Sub};
    use WackInstr::{Binary, Copy, JumpToHandler};
    use WackLiteral::{Bool, Char, Int};
    use WackValue::Literal;

    // Trait for types that can be used in binary operations
    pub trait BinaryEvaluatable {
        type Output;

        fn add(self, other: Self) -> Result<Self::Output, ()>;
        fn sub(self, other: Self) -> Result<Self::Output, ()>;
        fn mul(self, other: Self) -> Result<Self::Output, ()>;
        fn div(self, other: Self) -> Result<Self::Output, ()>;
        fn modulo(self, other: Self) -> Result<Self::Output, ()>;
        fn compare(self, other: Self) -> Option<Ordering>;
        fn to_literal(self) -> WackLiteral;
    }

    // Implementation for i32
    impl BinaryEvaluatable for i32 {
        type Output = i32;

        fn add(self, other: Self) -> Result<Self::Output, ()> {
            self.checked_add(other).ok_or(())
        }

        fn sub(self, other: Self) -> Result<Self::Output, ()> {
            self.checked_sub(other).ok_or(())
        }

        fn mul(self, other: Self) -> Result<Self::Output, ()> {
            self.checked_mul(other).ok_or(())
        }

        fn div(self, other: Self) -> Result<Self::Output, ()> {
            if other == 0 {
                return Err(());
            }
            self.checked_div(other).ok_or(())
        }

        fn modulo(self, other: Self) -> Result<Self::Output, ()> {
            if other == 0 {
                return Err(());
            }
            self.checked_rem(other).ok_or(())
        }

        fn compare(self, other: Self) -> Option<Ordering> {
            Some(self.cmp(&other))
        }

        fn to_literal(self) -> WackLiteral {
            Int(self)
        }
    }

    // Implementation for u8 (Char)
    impl BinaryEvaluatable for u8 {
        type Output = u8;

        fn add(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for chars
        }

        fn sub(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for chars
        }

        fn mul(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for chars
        }

        fn div(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for chars
        }

        fn modulo(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for chars
        }

        fn compare(self, other: Self) -> Option<Ordering> {
            Some(self.cmp(&other))
        }

        fn to_literal(self) -> WackLiteral {
            Char(self)
        }
    }

    // Implementation for WackBool
    impl BinaryEvaluatable for WackBool {
        type Output = WackBool;

        fn add(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for bools
        }

        fn sub(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for bools
        }

        fn mul(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for bools
        }

        fn div(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for bools
        }

        fn modulo(self, _other: Self) -> Result<Self::Output, ()> {
            Err(()) // Not supported for bools
        }

        fn compare(self, other: Self) -> Option<Ordering> {
            Some(self.cmp(&other))
        }

        fn to_literal(self) -> WackLiteral {
            Bool(self)
        }
    }

    pub fn eval_binary_op(
        op: BinaryOp,
        src1: WackValue,
        src2: WackValue,
        dst: WackTempIdent,
    ) -> Vec<WackInstr> {
        if let (WackValue::Literal(src1), WackValue::Literal(src2)) = (src1.clone(), src2.clone()) {
            // Handle constant folding for literal values
            // These are cheap clones, strings are Interned
            match (op, src1.clone(), src2.clone()) {
                // Integer arithmetic operations
                (Add, Int(v1), Int(v2)) => eval_int_arithmetic(v1, v2, dst, false, |a, b| a.add(b)),
                (Sub, Int(v1), Int(v2)) => eval_int_arithmetic(v1, v2, dst, false, |a, b| a.sub(b)),
                (Mul, Int(v1), Int(v2)) => eval_int_arithmetic(v1, v2, dst, false, |a, b| a.mul(b)),
                (Div, Int(v1), Int(v2)) => eval_int_arithmetic(v1, v2, dst, true, |a, b| a.div(b)),
                (Mod, Int(v1), Int(v2)) => {
                    eval_int_arithmetic(v1, v2, dst, true, |a, b| a.modulo(b))
                }

                // Comparison operations for integers
                (Eq, Int(v1), Int(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord == Ordering::Equal)
                }
                (Neq, Int(v1), Int(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord != Ordering::Equal)
                }
                (Gt, Int(v1), Int(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord == Ordering::Greater)
                }
                (Gte, Int(v1), Int(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord != Ordering::Less)
                }
                (Lt, Int(v1), Int(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord == Ordering::Less)
                }
                (Lte, Int(v1), Int(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord != Ordering::Greater)
                }

                // Comparison operations for chars
                (Eq, Char(v1), Char(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord == Ordering::Equal)
                }
                (Neq, Char(v1), Char(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord != Ordering::Equal)
                }
                (Gt, Char(v1), Char(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord == Ordering::Greater)
                }
                (Gte, Char(v1), Char(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord != Ordering::Less)
                }
                (Lt, Char(v1), Char(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord == Ordering::Less)
                }
                (Lte, Char(v1), Char(v2)) => {
                    eval_compare_and_fold(v1, v2, dst, |ord| ord != Ordering::Greater)
                }

                // Logical operations for bools
                (LAnd, Bool(v1), Bool(v2)) => {
                    let result = v1 && v2;
                    vec![Copy {
                        src: Literal(Bool(result)),
                        dst,
                    }]
                }
                (LOr, Bool(v1), Bool(v2)) => {
                    let result = v1 || v2;
                    vec![Copy {
                        src: Literal(Bool(result)),
                        dst,
                    }]
                }

                // Any type equality operations
                (Eq, v1, v2) => vec![Copy {
                    src: Literal(Bool(v1 == v2)),
                    dst,
                }],
                (Neq, v1, v2) => vec![Copy {
                    src: Literal(Bool(v1 != v2)),
                    dst,
                }],

                // Unsupported operations
                _ => vec![Binary {
                    op,
                    src1: Literal(src1),
                    src2: Literal(src2),
                    dst,
                }],
            }
        } else {
            // If not both literals, emit the binary operation instruction
            vec![Binary {
                op,
                src1,
                src2,
                dst,
            }]
        }
    }

    // Helper function to evaluate arithmetic operations on integers with error handling
    fn eval_int_arithmetic<F>(
        v1: i32,
        v2: i32,
        dst: WackTempIdent,
        is_div_like: bool,
        op_fn: F,
    ) -> Vec<WackInstr>
    where
        F: Fn(i32, i32) -> Result<i32, ()>,
    {
        match op_fn(v1, v2) {
            Ok(result) => vec![Copy {
                src: Literal(Int(result)),
                dst,
            }],
            Err(()) => {
                if v2 == 0 && is_div_like {
                    // Division by zero error
                    insert_flag_gbl(GenFlags::DIV_BY_ZERO);
                    vec![JumpToHandler(INBUILT_DIV_ZERO.to_owned())]
                } else {
                    // Overflow error
                    insert_flag_gbl(GenFlags::OVERFLOW);
                    vec![JumpToHandler(INBUILT_OVERFLOW.to_owned())]
                }
            }
        }
    }

    // Helper function to evaluate comparisons with ordered types
    fn eval_compare_and_fold<T, F>(v1: T, v2: T, dst: WackTempIdent, cmp_fn: F) -> Vec<WackInstr>
    where
        T: BinaryEvaluatable + std::marker::Copy,
        F: Fn(Ordering) -> bool,
    {
        if let Some(ordering) = v1.compare(v2) {
            let result = cmp_fn(ordering);
            vec![Copy {
                src: Literal(Bool(result)),
                dst,
            }]
        } else {
            // Fallback in case comparison isn't possible
            vec![Binary {
                op: BinaryOp::Eq,
                src1: Literal(v1.to_literal()),
                src2: Literal(v2.to_literal()),
                dst,
            }]
        }
    }
}
