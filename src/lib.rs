extern crate core;

pub mod ast;
pub mod parser;
pub mod shared;
pub mod spanned;
pub mod token;

pub(crate) mod private {

    // sealed traits support
    pub trait Sealed {}
    impl<T> Sealed for T {}
}

pub(crate) trait CharExt: private::Sealed + Sized {
    /// Just like [char::to_string] but takes ownership of the character.
    fn to_string_owned(self) -> String;

    /// Any ASCII character except `\`, `'` and `"`.
    fn normal_wacc_char(&self) -> bool;

    /// One of `0`, `b`, `t`, `n`, `f`, `r`, `"`, `'` or `\`.
    fn escaped_wacc_char(&self) -> bool;

    /// Looks up the character that a WACC-escaped character represents:
    ///
    /// | WACC-escaped character | ASCII Value | Description     |
    /// |------------------------|-------------|-----------------|
    /// | `0`                    | `0x00`      | null terminator |
    /// | `b`                    | `0x08`      | backspace       |
    /// | `t`                    | `0x09`      | tab             |
    /// | `n`                    | `0x0a`      | new line        |
    /// | `f`                    | `0x0c`      | form feed       |
    /// | `r`                    | `0x0d`      | carriage return |
    /// | `"`                    | `0x22`      | double quote    |
    /// | `'`                    | `0x27`      | single quote    |
    /// | `\`                    | `0x5c`      | backslash       |
    ///
    /// Source: WACC-language spec, Table 2.
    fn lookup_escaped_wacc_char(&self) -> Option<Self>;
}

impl CharExt for char {
    fn to_string_owned(self) -> String {
        self.to_string()
    }

    fn normal_wacc_char(&self) -> bool {
        match self {
            '\\' | '\'' | '"' => false,
            _ => true,
        }
    }

    fn escaped_wacc_char(&self) -> bool {
        match self {
            '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\' => true,
            _ => false,
        }
    }

    fn lookup_escaped_wacc_char(&self) -> Option<Self> {
        match self {
            // Table-2 of the specification
            '0' => Some(0x00 as char),
            'b' => Some(0x08 as char),
            't' => Some(0x09 as char),
            'n' => Some(0x0a as char),
            'f' => Some(0x0c as char),
            'r' => Some(0x0d as char),
            '"' => Some(0x22 as char),
            '\'' => Some(0x27 as char),
            '\\' => Some(0x5c as char),
            _ => None,
        }
    }
}
