// Flags used to signal that we should emit some code
use once_cell::sync::Lazy;
use std::collections::HashMap;
use util::gen_flags::GenFlags;

/* ================== PUBLIC API ================== */

/// Returns the combined assembly code given the flags given,
/// this will be called during emission
#[inline]
pub fn emit_predefined_asm(flags: GenFlags) -> String {
    PREDEFINED_FUNCTIONS
        .iter()
        .filter(|&(flag, _)| flags.contains(*flag))
        .map(|(_, asm)| *asm)
        .collect::<Vec<&str>>()
        .join("\n")
}

pub const ERR_DIVZERO: &str = "_errDivZero";

/* ================== INTERNALS ================== */

const OVERFLOW_ASM: &str = include_str!("predefined_funcs/overflow.txt");
const MALLOC_ASM: &str = include_str!("predefined_funcs/malloc.txt");
const FREE_ASM: &str = include_str!("predefined_funcs/free.txt");
const FREE_PAIR_ASM: &str = include_str!("predefined_funcs/free_pair.txt");
const OOM_ASM: &str = include_str!("predefined_funcs/oom.txt");
const PRINT_PTR_ASM: &str = include_str!("predefined_funcs/print_ptr.txt");
const PRINT_STR_ASM: &str = include_str!("predefined_funcs/print_str.txt");
const PRINT_CHR_ASM: &str = include_str!("predefined_funcs/print_chr.txt");
const PRINT_BOOLEAN_ASM: &str = include_str!("predefined_funcs/print_bool.txt");
const PRINT_INT_ASM: &str = include_str!("predefined_funcs/print_int.txt");
const PRINTLN_ASM: &str = include_str!("predefined_funcs/println.txt");
const ARRAY_ACCESS_ASM: &str = include_str!("predefined_funcs/array_access.txt");
const CHR_BOUNDS_ASM: &str = include_str!("predefined_funcs/chr_bounds.txt");
const READ_INT_ASM: &str = include_str!("predefined_funcs/read_int.txt");
const READ_CHR_ASM: &str = include_str!("predefined_funcs/read_chr.txt");
const DIV_BY_ZERO_ASM: &str = include_str!("predefined_funcs/div_zero.txt");
const NULL_DEREF_ASM: &str = include_str!("predefined_funcs/null_access.txt");

static PREDEFINED_FUNCTIONS: Lazy<HashMap<GenFlags, &'static str>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert(GenFlags::OVERFLOW, OVERFLOW_ASM);
    m.insert(GenFlags::MALLOC, MALLOC_ASM);
    m.insert(GenFlags::FREE, FREE_ASM);
    m.insert(GenFlags::FREE_PAIR, FREE_PAIR_ASM);
    m.insert(GenFlags::OOM, OOM_ASM);
    m.insert(GenFlags::PRINT_PTR, PRINT_PTR_ASM);
    m.insert(GenFlags::PRINT_STR, PRINT_STR_ASM);
    m.insert(GenFlags::PRINT_CHR, PRINT_CHR_ASM);
    m.insert(GenFlags::PRINT_BOOLEAN, PRINT_BOOLEAN_ASM);
    m.insert(GenFlags::PRINT_INT, PRINT_INT_ASM);
    m.insert(GenFlags::PRINT_LN, PRINTLN_ASM);
    m.insert(GenFlags::ARRAY_ACCESS, ARRAY_ACCESS_ASM);
    m.insert(GenFlags::CHR_BOUNDS, CHR_BOUNDS_ASM);
    m.insert(GenFlags::READ_INT, READ_INT_ASM);
    m.insert(GenFlags::READ_CHR, READ_CHR_ASM);
    m.insert(GenFlags::DIV_BY_ZERO, DIV_BY_ZERO_ASM);
    m.insert(GenFlags::NULL_DEREF, NULL_DEREF_ASM);
    m
});
