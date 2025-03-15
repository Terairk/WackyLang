// File for helper functions related to registers
use crate::assembly_ast::{Operand, Register};
use Register::{AX, BP, BX, CX, DI, DX, R8, R9, R10, R11, R12, R13, R14, R15, SI, SP};
use bitflags::bitflags;

pub const ALL_HARDREGS: [Register; 16] = [
    AX, BX, CX, DX, DI, SI, R8, R9, R10, R11, R12, R13, R14, R15, SP, BP,
];

// These are the registers which we're gonna colour. Don't include BP and SP cus stack management
// Don't include R10, R11 cus we use them in instruction-fix up
pub const ALL_BASEREGS: [Register; 12] = [AX, BX, CX, DX, DI, SI, R8, R9, R12, R13, R14, R15];
pub const LEN_ALL_BASEREGS: usize = 12;
// Don't use R10 and R11 as we use them for our fixing instructions pass
pub const CALLER_SAVED: [Register; 7] = [AX, CX, DX, SI, DI, R8, R9];
pub const CALLEE_SAVED: [Register; 6] = [BX, R12, R13, R14, R15, BP];
pub const PARAM_REGS: [Register; 6] = [DI, SI, DX, CX, R8, R9];

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
    pub struct RegisterSet: u16 {
        const AX = 1 << 0;
        const BX = 1 << 1;
        const CX = 1 << 2;
        const DX = 1 << 3;
        const DI = 1 << 4;
        const SI = 1 << 5;
        const R8 = 1 << 6;
        const R9 = 1 << 7;
        const R12 = 1 << 8;
        const R13 = 1 << 9;
        const R14 = 1 << 10;
        const R15 = 1 << 11;
        const SP = 1 << 12;
        const BP = 1 << 13;
        const R10 = 1 << 14;
        const R11 = 1 << 15;
    }
}

#[must_use]
#[inline]
pub fn is_callee_saved(operand: &Operand) -> bool {
    match operand {
        Operand::Reg(reg) => CALLEE_SAVED.contains(reg),
        _ => false,
    }
}

#[must_use]
#[inline]
pub fn is_callee_saved_reg(reg: &Register) -> bool {
    CALLEE_SAVED.contains(reg)
}

#[must_use]
#[inline]
pub fn is_hard_reg(reg: &Register) -> bool {
    ALL_HARDREGS.contains(reg)
}

#[must_use]
#[inline]
pub fn is_hard_reg_op(operand: &Operand) -> bool {
    match operand {
        Operand::Reg(reg) => ALL_HARDREGS.contains(reg),
        _ => false,
    }
}

/// Converts a `RegisterSet` bitflag into a Vec of the corresponding Register values
#[must_use]
#[inline]
pub fn register_set_to_vec(reg_set: RegisterSet) -> Vec<Register> {
    ALL_HARDREGS
        .iter()
        .filter(|&reg| {
            // Map each register to its corresponding bit flag
            let flag = match reg {
                AX => RS::AX,
                BX => RS::BX,
                CX => RS::CX,
                DX => RS::DX,
                DI => RS::DI,
                SI => RS::SI,
                R8 => RS::R8,
                R9 => RS::R9,
                R10 => RS::R10,
                R11 => RS::R11,
                R12 => RS::R12,
                R13 => RS::R13,
                R14 => RS::R14,
                R15 => RS::R15,
                SP => RS::SP,
                BP => RS::BP,
            };

            // Check if the flag is present in the register set
            reg_set.contains(flag)
        })
        .copied()
        .collect()
}

use RegisterSet as RS;
pub const RS1: RegisterSet = RegisterSet::from_bits_truncate(RS::DI.bits());
pub const RS2: RegisterSet = RegisterSet::from_bits_truncate(RS::DI.bits() | RS::SI.bits());
pub const RS3: RegisterSet =
    RegisterSet::from_bits_truncate(RS::DI.bits() | RS::SI.bits() | RS::DX.bits());
pub const RS4: RegisterSet =
    RegisterSet::from_bits_truncate(RS::DI.bits() | RS::SI.bits() | RS::DX.bits() | RS::CX.bits());
pub const RS5: RegisterSet = RegisterSet::from_bits_truncate(
    RS::DI.bits() | RS::SI.bits() | RS::DX.bits() | RS::CX.bits() | RS::R8.bits(),
);
pub const RS6: RegisterSet = RegisterSet::from_bits_truncate(
    RS::DI.bits() | RS::SI.bits() | RS::DX.bits() | RS::CX.bits() | RS::R8.bits() | RS::R9.bits(),
);

// Use DI, SI for my sanity since we don't use them for our fixing instructions pass
// Also reduces chances of conflict with function
// ie treat arrays as function calls. ALso please see the predefined functions for ARR_LOAD
// as they use registers which you do not want to conflict with
pub const ARR_PTR_REG: Register = DI;
pub const ARR_INDEX_REG: Register = SI;
pub const ARR_LOAD_RETURN: Register = AX;

// Unfortunately these do not change automatically with the above
// pub const RS_ARR: RegisterSet = RegisterSet::from_bits_truncate(RS::R8.bits() | RS::R9.bits());

pub const RS_ARR: RegisterSet = RegisterSet::from_bits_truncate(RS::DI.bits() | RS::SI.bits());
