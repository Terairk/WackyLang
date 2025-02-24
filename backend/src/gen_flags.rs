// Flags used to signal that we should emit some code
use bitflags::bitflags;

/* ================== PUBLIC API ================== */
bitflags! {
    pub struct GenFlags: u32 {
        const OVERFLOW      = 0x0000_0001;
        const MALLOC        = 0x0000_0002;
        const FREE          = 0x0000_0004;
        const FREE_PAIR     = 0x0000_0008;
        const OOM           = 0x0000_0010;
        const PRINT_PTR     = 0x0000_0020;
        const PRINT_STR     = 0x0000_0040;
        const PRINT_CHR     = 0x0000_0080;
        const PRINT_BOOLEAN = 0x0000_0100;
        const PRINT_INT     = 0x0000_0200;
        const PRINT_LN      = 0x0000_0400;
        const ARRAY_ACCESS  = 0x0000_0800; // comes with error message
        const CHR_BOUNDS    = 0x0000_1000;
        const READ_INT      = 0x0000_2000;
        const READ_CHR      = 0x0000_4000;
        const DIV_BY_ZERO   = 0x0000_8000;
        const NULL_DEREF    = 0x0001_0000; // handles null free's and null deref
        // TODO: pretty sure we need to add more
    }
}

/// Some flags like `PRINT_STR` should be set to true if we want to print strings
/// Which most of these flags want to do
#[inline]
pub fn rewrite_flags(flags: GenFlags) -> GenFlags {
    // Start with the original flag

    let mut new_flags = flags;

    // Define a combination of printing string flags.
    let not_printstr =
        GenFlags::PRINT_PTR | GenFlags::PRINT_CHR | GenFlags::PRINT_BOOLEAN | GenFlags::PRINT_INT;
    #[allow(clippy::arithmetic_side_effects)]
    let print_str_flags = GenFlags::all() - not_printstr;

    if new_flags.intersects(print_str_flags) {
        new_flags |= GenFlags::PRINT_STR;
    }

    if new_flags.contains(GenFlags::FREE_PAIR) {
        new_flags |= GenFlags::NULL_DEREF;
    }

    if new_flags.contains(GenFlags::MALLOC) {
        new_flags |= GenFlags::OOM;
    }

    if new_flags.contains(GenFlags::OOM) {
        new_flags |= GenFlags::PRINT_STR;
    }
    new_flags
}

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
