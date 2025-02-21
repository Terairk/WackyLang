// Flags used to signal that we should emit some code
use bitflags::bitflags;

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
        const NULL_DEALLOC  = 0x0000_0400;
        const ARRAY_BOUNDS  = 0x0000_0800;
        const CHR_BOUNDS    = 0x0000_1000;
        const READ_INT      = 0x0000_2000;
        const READ_CHR      = 0x0000_4000;
    }
}
