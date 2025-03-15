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
    }
}

/* ================== INTERNAL FUNC_NAMES ================== */
// Anytime you want to call an inbuilt function please use the
// constant strings, that way the code is more modular and you
// dont have to change strings in multiple places

pub static INBUILT_DIV_ZERO: &str = "_errDivZero";
pub static INBUILT_PRINT_STRING: &str = "_prints";
pub static INBUILT_MALLOC: &str = "_malloc";
pub static INBUILT_OOM: &str = "_errOutOfMemory";
pub static INBUILT_FREE: &str = "_free";
pub static INBUILT_FREE_PAIR: &str = "_freepair";
pub static INBUILT_NULL_ACCESS: &str = "_errNull";
pub static INBUILT_PRINT_PTR: &str = "_printp";
pub static INBUILT_PRINT_CHAR: &str = "_printc";
pub static INBUILT_PRINT_BOOL: &str = "_printb";
pub static INBUILT_PRINT_INT: &str = "_printi";
pub static INBUILT_PRINTLN: &str = "_println";
pub static INBUILT_ARR_LOAD1: &str = "_arrLoad1";
pub static INBUILT_ARR_LOAD4: &str = "_arrLoad4";
pub static INBUILT_ARR_LOAD8: &str = "_arrLoad8";
pub static INBUILT_OUT_OF_BOUNDS: &str = "_errOutOfBounds";
pub static INBUILT_BAD_CHAR: &str = "_errBadChar";
pub static INBUILT_OVERFLOW: &str = "_errOverflow";
pub static INBUILT_READ_INT: &str = "_readi";
pub static INBUILT_READ_CHAR: &str = "_readc";
pub static INBUILT_EXIT: &str = "_exit";

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
}
