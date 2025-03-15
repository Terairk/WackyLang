use util::{CFG, Instruction, SimpleInstr, cfg::Location};
// temporary bCFG cus I want to leave everything the same

use crate::alias::InternStr;
use crate::wackir::{WackInstr, WackTempIdent};
use WackInstr::{
    AddPtr, Alloc, ArrayAccess, Binary, Copy, CopyToOffset, Exit, FreeChecked, FreeUnchecked,
    FunCall, Jump, JumpIfNotZero, JumpIfZero, JumpToHandler, Label, Load, NullPtrGuard, Print,
    Println, Read, Return, Unary,
};

pub type EmptyCFG = CFG<WackInstr, ()>;

#[must_use]
#[inline]
pub fn make_cfg(instrs: Vec<WackInstr>, func_name: &str) -> EmptyCFG {
    CFG::<WackInstr, ()>::from_instructions(func_name.to_owned(), instrs)
}

#[must_use]
#[inline]
pub fn cfg_to_instrs(cfg: EmptyCFG) -> Vec<WackInstr> {
    CFG::to_instructions(cfg)
}

// Implementation of the Instruction trait for WackInstr
// TODO: as said earlier, see if we can change things to take ownership
// so we don't have to clone as much
impl Instruction for WackInstr {
    #[inline]
    fn simplify(&self) -> SimpleInstr {
        use WackInstr::{Jump, JumpIfNotZero, JumpIfZero, JumpToHandler, Label, Return};
        match *self {
            Label(ref name) => SimpleInstr::Label(name.clone().into()),
            Jump(ref name) => SimpleInstr::UnconditionalJump(name.clone().into()),
            JumpIfZero { ref target, .. } | JumpIfNotZero { ref target, .. } => {
                SimpleInstr::ConditionalJump(target.clone().into())
            }
            JumpToHandler(_) => SimpleInstr::ErrorJump,
            Return(..) => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }
}

impl From<WackTempIdent> for Location {
    #[inline]
    fn from(ident: WackTempIdent) -> Self {
        let interned_string: InternStr = ident.clone().into();
        let id = ident.get_id();
        Self::new(interned_string, id)
    }
}

pub const fn get_dst(instr: &WackInstr) -> Option<&WackTempIdent> {
    match *instr {
        Return(_) => None,
        Unary { ref dst, .. }
        | Binary { ref dst, .. }
        | FunCall { ref dst, .. }
        | Copy { ref dst, .. }
        | Read { ref dst, .. }
        | Load { ref dst, .. } => Some(dst),
        AddPtr { ref dst_ptr, .. }
        | CopyToOffset { ref dst_ptr, .. }
        | Alloc { ref dst_ptr, .. } => Some(dst_ptr),
        ArrayAccess {
            ref dst_elem_ptr, ..
        } => Some(dst_elem_ptr),
        Jump(_) | JumpIfZero { .. } | JumpIfNotZero { .. } | JumpToHandler(_) | Label(_) => None,
        FreeUnchecked(_)
        | FreeChecked(_)
        | NullPtrGuard(_)
        | Print { .. }
        | Println { .. }
        | Exit(_) => None,
    }
}
