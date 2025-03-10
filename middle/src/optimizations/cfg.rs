use crate::wackir::WackInstr;

pub type CFG = Vec<WackInstr>;

pub fn make_cfg(instrs: Vec<WackInstr>) -> CFG {
    instrs
}

pub fn cfg_to_instrs(cfg: CFG) -> Vec<WackInstr> {
    cfg
}
