use util::{CFG as bCFG, Instruction, SimpleInstr};
// temporary bCFG cus I want to leave everything the same

use crate::wackir::WackInstr;

// TODO: Change the CFG to the proper CFG type
// I'll figure this out as I go along
pub type CFG = bCFG<WackInstr, ()>;
pub type EmptyCFG = bCFG<WackInstr, ()>;

pub fn make_cfg(instrs: Vec<WackInstr>, func_name: &str) -> EmptyCFG {
    bCFG::<WackInstr, ()>::from_instructions(func_name.to_owned(), instrs)
}

pub fn cfg_to_instrs(cfg: EmptyCFG) -> Vec<WackInstr> {
    bCFG::to_instructions(cfg)
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
