// File for helper functions related to registers
use crate::assembly_ast::{AssemblyType, Register};
use Register::{AX, BP, BX, CX, DI, DX, R8, R9, R10, R11, R12, R13, R14, R15, SI, SP};
use bitflags::bitflags;

pub const ALL_HARDREGS: [Register; 16] = [
    AX, BX, CX, DX, DI, SI, R8, R9, R10, R11, R12, R13, R14, R15, SP, BP,
];
// Don't use R10 and R11 as we use them for our fixing instructions pass
pub const CALLER_SAVED: [Register; 7] = [AX, CX, DX, SI, DI, R8, R9];

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

// Use R8 and R9 since we don't use them for our fixing instructions pass
// Also reduces chances of conflict with function
pub const ARR_PTR_REG: Register = R8;
pub const ARR_INDEX_REG: Register = R9;
pub const ARR_LOAD_RETURN: Register = AX;
pub const ARR_STORE_RETURN: Register = AX;

// Unfortunately these do not change automatically with the above
pub const RS_ARR: RegisterSet = RegisterSet::from_bits_truncate(RS::R9.bits() | RS::R10.bits());
