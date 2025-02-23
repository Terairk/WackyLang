// Flags used to signal that we should emit some code
use bitflags::bitflags;

/* ================== PUBLIC API ================== */

bitflags! {
    pub struct GenFlags: u32 {
        const OVERFLOW      = 0x0000_0001;
        const MALLOC        = 0x0000_0002;
        const FREE          = 0x0000_0004;
        const FREE_NULL     = 0x0000_0008;
        const OOM           = 0x0000_0010;
        const PRINT_PTR     = 0x0000_0020;
        const PRINT_STR     = 0x0000_0040;
        const PRINT_CHR     = 0x0000_0080;
        const PRINT_BOOLEAN = 0x0000_0100;
        const PRINT_INT     = 0x0000_0200;
        const PRINT_LN      = 0x0000_0400;
        const ARRAY_BOUNDS  = 0x0000_0800;
        const CHR_BOUNDS    = 0x0000_1000;
        const READ_INT      = 0x0000_2000;
        const READ_CHR      = 0x0000_4000;
        const DIV_BY_ZERO   = 0x0000_8000;
        const NULL_DEALLOC  = 0x0001_0000;
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

    new_flags
}

/* ================== INTERNALS ================== */
