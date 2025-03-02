use bitflags::bitflags;
use once_cell::sync::Lazy;
use std::sync::Mutex;

/* ================== PUBLIC API ================== */

static GLOBAL_FLAGS: Lazy<Mutex<GenFlags>> = Lazy::new(|| Mutex::new(GenFlags::empty()));

bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub struct GenFlags: u32 {
        const OVERFLOW       = 0x0000_0001;
        const MALLOC         = 0x0000_0002;
        const FREE           = 0x0000_0004;
        const FREE_PAIR      = 0x0000_0008;
        const OOM            = 0x0000_0010;
        const PRINT_PTR      = 0x0000_0020;
        const PRINT_STR      = 0x0000_0040;
        const PRINT_CHR      = 0x0000_0080;
        const PRINT_BOOLEAN  = 0x0000_0100;
        const PRINT_INT      = 0x0000_0200;
        const PRINT_LN       = 0x0000_0400;
        const ARRAY_ACCESS1  = 0x0000_0800;
        const CHR_BOUNDS     = 0x0000_1000;
        const READ_INT       = 0x0000_2000;
        const READ_CHR       = 0x0000_4000;
        const DIV_BY_ZERO    = 0x0000_8000;
        const NULL_DEREF     = 0x0001_0000; // handles null free's and null deref
        const EXIT           = 0x0002_0000;
        const ARR_BOUNDS     = 0x0004_0000;
        const ARRAY_ACCESS4  = 0x0008_0000;
        const ARRAY_ACCESS8  = 0x0010_0000;
        const ARRAY_STORE1   = 0x0020_0000;
        const ARRAY_STORE4   = 0x0040_0000;
        const ARRAY_STORE8   = 0x0080_0000;
    }
}

/// # Panics
///
/// This is fine because our thing is single threaded
/// and we don't want to handle errors
/// Inserts flags into the global flags
#[inline]
pub fn insert_flag_gbl(flag: GenFlags) {
    let mut flags = GLOBAL_FLAGS.lock().unwrap();
    *flags |= flag;
}

/// This is used for the backend step or any step
/// that wants a copy of `GenFlags`
pub fn get_flags_gbl() -> GenFlags {
    let flags = GLOBAL_FLAGS.lock().unwrap();
    *flags
}

pub fn reset_flags_gbl() {
    let mut flags = GLOBAL_FLAGS.lock().unwrap();
    *flags = GenFlags::empty();
}

/// Some flags like `PRINT_STR` should be set to true if we want to print strings
/// Which most of these flags want to do
// Furthermore, this function may not work if additional dependencies are required
// with a path-length of 2 or greater.
// In that case, refactor this to be a helper function which gets called multiple times
// until a fixed point is reached.
#[inline]
pub fn rewrite_global_flag() {
    // Start with the original flag

    // Define a combination of printing string flags.
    let not_printstr =
        GenFlags::PRINT_PTR | GenFlags::PRINT_CHR | GenFlags::PRINT_BOOLEAN | GenFlags::PRINT_INT;
    #[allow(clippy::arithmetic_side_effects)]
    let print_str_flags = GenFlags::all() - not_printstr;
    let mut global_flags = GLOBAL_FLAGS.lock().unwrap();

    if global_flags.intersects(print_str_flags) {
        *global_flags |= GenFlags::PRINT_STR;
    }

    if global_flags.contains(GenFlags::FREE_PAIR) {
        *global_flags |= GenFlags::NULL_DEREF;
    }

    if global_flags.contains(GenFlags::MALLOC) {
        *global_flags |= GenFlags::OOM;
    }

    if global_flags.contains(GenFlags::OOM) {
        *global_flags |= GenFlags::PRINT_STR;
    }

    let array_access_flags =
        GenFlags::ARRAY_ACCESS1 | GenFlags::ARRAY_ACCESS4 | GenFlags::ARRAY_ACCESS8;
    if global_flags.intersects(array_access_flags) {
        *global_flags |= GenFlags::ARR_BOUNDS;
    }
    let array_store_flags =
        GenFlags::ARRAY_STORE1 | GenFlags::ARRAY_STORE4 | GenFlags::ARRAY_STORE8;
    if global_flags.intersects(array_store_flags) {
        *global_flags |= GenFlags::ARR_BOUNDS;
    }
}
