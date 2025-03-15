use crate::alias::InternStr;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Delim {
    Bracket = 0,
    Paren = 1,
}

impl Delim {
    /// The current number of variants.
    const NUM_VARIANTS: usize = 2;
    /// The current variants.
    const VARIANTS: [Self; Self::NUM_VARIANTS] = [Self::Paren, Self::Bracket];
    /// Placeholder element, useful for allocating arrays.
    const PLACEHOLDER: Self = Self::Paren;

    /// A copy of the current variants.
    #[must_use]
    #[inline]
    pub const fn variants() -> [Self; Self::NUM_VARIANTS] {
        Self::VARIANTS
    }

    /// A reference to the current variants.
    #[must_use]
    #[inline]
    pub const fn variants_ref() -> &'static [Self; Self::NUM_VARIANTS] {
        &Self::VARIANTS
    }

    #[allow(
        clippy::indexing_slicing,
        clippy::as_conversions,
        clippy::arithmetic_side_effects
    )]
    #[inline]
    #[must_use]
    pub const fn variants_except(self) -> [Self; Self::NUM_VARIANTS - 1] {
        // allocate output array
        let mut others: [Self; Self::NUM_VARIANTS - 1] = [Self::PLACEHOLDER];

        // for loops not allowed in const-rust
        let mut i = 0;
        let mut j = 0;
        while i < Self::NUM_VARIANTS {
            let delim = Self::VARIANTS[i];

            // `Eq` trait calls not allowed in const-rust,
            // have to cast to u8-repr and compare that instead
            if self as u8 != delim as u8 {
                others[j] = delim;
                j += 1;
            }
            i += 1;
        }

        others
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InvalidCharReason {
    NonAsciiChar,
    InvalidEscape,
    MoreThanOneChar,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    // literals
    Ident(InternStr),
    IntLiter(i32),
    CharLiter(char),
    StrLiter(InternStr),

    // delimiter symbols
    Open(Delim),
    Close(Delim),

    // other symbols
    Lte,
    Lt,
    Gte,
    Gt,
    BangEquals,
    Bang,
    EqualsEquals,
    Equals,
    Plus,
    Minus,
    Star,
    Percent,
    ForwardSlash,
    AmpersandAmpersand,
    PipePipe,
    Colon,
    Semicolon,
    Comma,

    // keywords
    Begin,
    End,
    Tailrec,
    Is,
    Skip,
    Read,
    Free,
    Return,
    Exit,
    Print,
    Println,
    If,
    Then,
    Elif,
    Else,
    Fi,
    While,
    Do,
    Done,
    Loop,
    Break,
    Nextloop,
    Newpair,
    Pair,
    Fst,
    Snd,
    Call,
    Int,
    Bool,
    Char,
    String,
    Len,
    Ord,
    Chr,
    Null,
    True,
    False,
}

impl fmt::Display for Token {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Ident(ref s) => write!(f, "{s}"),
            Self::IntLiter(ref i) => write!(f, "{i}"),
            Self::CharLiter(ref c) => write!(f, "{c}"), // TODO: unescape the character literal, i.e. newline -> '\c'
            Self::StrLiter(ref s) => write!(f, "{s}"), // TODO: unescape the string literal, as with char literal
            Self::Open(ref d) => match *d {
                Delim::Paren => write!(f, "("),
                Delim::Bracket => write!(f, "["),
            },
            Self::Close(ref d) => match *d {
                Delim::Paren => write!(f, ")"),
                Delim::Bracket => write!(f, "]"),
            },
            Self::Lte => write!(f, "<="),
            Self::Lt => write!(f, "<"),
            Self::Gte => write!(f, ">="),
            Self::Gt => write!(f, ">"),
            Self::BangEquals => write!(f, "!="),
            Self::Bang => write!(f, "!"),
            Self::EqualsEquals => write!(f, "=="),
            Self::Equals => write!(f, "="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Percent => write!(f, "%"),
            Self::ForwardSlash => write!(f, "/"),
            Self::AmpersandAmpersand => write!(f, "&&"),
            Self::PipePipe => write!(f, "||"),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Begin => write!(f, "begin"),
            Self::End => write!(f, "end"),
            Self::Tailrec => write!(f, "tailrec"),
            Self::Is => write!(f, "is"),
            Self::Skip => write!(f, "skip"),
            Self::Read => write!(f, "read"),
            Self::Free => write!(f, "free"),
            Self::Return => write!(f, "return"),
            Self::Exit => write!(f, "exit"),
            Self::Print => write!(f, "print"),
            Self::Println => write!(f, "println"),
            Self::If => write!(f, "if"),
            Self::Then => write!(f, "then"),
            Self::Elif => write!(f, "elif"),
            Self::Else => write!(f, "else"),
            Self::Fi => write!(f, "fi"),
            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::Done => write!(f, "done"),
            Self::Loop => write!(f, "loop"),
            Self::Break => write!(f, "break"),
            Self::Nextloop => write!(f, "nextloop"),
            Self::Newpair => write!(f, "newpair"),
            Self::Pair => write!(f, "pair"),
            Self::Fst => write!(f, "fst"),
            Self::Snd => write!(f, "snd"),
            Self::Call => write!(f, "call"),
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::String => write!(f, "string"),
            Self::Len => write!(f, "len"),
            Self::Ord => write!(f, "ord"),
            Self::Chr => write!(f, "chr"),
            Self::Null => write!(f, "null"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
        }
    }
}
