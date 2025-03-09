/* Best part about compilers is naming our IR
 * I'll try make a TAC IR called WACK-IR
 * Reasons for making a TAC IR instead of straight to assembly IR
 * 1) Easier to make optimisations (for a possible extension)
 * 2) Easier to convert into SSA form
 * 3) We don't need to decide on what assembly to do yet
 * 4) Easier to work with if we want to support multiple assembly architectures
 * 5) Several smaller transformations is easier to work with than one complex
 * transformation. Plus easier to split up work assuming invariants aren't violated
 * Invariants (TBD)
 * 6) Easier debuggability - its easier to work with TAC versus the abomination that
 * is the nested AST (there's lots of different nodes which aren't easy to use)
 *
 *
 * =================== Some notes on the IR ====================
 * 1) We'll have a TAC IR (Three Address Code)
 * 2) Have high level constructs for Read/Printing/Other libc calls
 * 3) I'm not sure if I want to have high level nodes for creating pairs/arrays,
 *    but most likely since it'll be easier to reason albeit more duplication
 *    but also less chance of screwing up if we ever optimise it.
 * 4) Some of the high level nodes will convert directly into some higher level assembly AST node
 * 5) This will just make our lives easier.
 * 6) There might be some redundancies in the initial IR so uhh remove any useless ones
 * */

/* ==================== WACK-IR Structure =====================
* program = Program(top_level*, instruction* body) // The main program
* top_level = Function(identifier, bool global = true, identifier* params, instruction* body)
*            | StaticConstant(identifier, type t, static_init init) - most likely not needed
*
* instruction = Return(val)
*             | ZeroExtend(val src, val dst) // Might need this for Pairs - not sure
*             | Unary(unary_operator, val src, val dst)
*             | Binary(binary_operator, val src1, val src2, val dst)
*             | Copy(val src, val dst)
*             | Load(val src_ptr, val dst) // Load from memory
*             | Store(val src, val dst_ptr) // Store to memory
*             | AddPtr(val ptr, val index, int scale, val dst) // maybe useful for arrays/pairs
*             but its initial purpose was pointer arithmetic which I feel like we don't need to do
*             ie we can use the others
*             | CopyToOffset(val src, identifier dst, int offset) - used for array initializers
*               i.e: int[] arr = [1, 2, 3] -> CopyToOffset(1, arr, 0), CopyToOffset(2, arr, 4),
*               CopyToOffset(3, arr, 8)
*             | CopyFromoffset(identifier src, int offset, val dst) - used for pair accesses maybe
*               howeva: it's a bit overkill since its meant for structs but pairs are kinda like
*               structs - i'm going with the assumption that pairs are 16 bytes
*             | Jump(identifier target)
*             | JumpIfZero(val condition, identifier target)
*             | JumpIfNotZero(val condition, identifier target)
*             | Label(identifier)
*             | FunCall(identifier fun_name, val* args, val dst) // I think fun calls always have
*             a destination
*             -- For the following im not sure if these should be vals or Identifiers
*             | Read(val dst, type t)
*             | Free(val src)
*             | Exit(val src)
*             | Print(val src, type t)
*             | Println(val src, type t) -- this may be merged with Print
*
* -- wacc_specific isn't being used for now but I may find it useful to refactor and use this
* -- for now if instruction's going to be long whats a few extra
* wacc_specific = Read(val dst, type t)
*              | Free(val src)
*              | Exit(val src)
*              | Print(val src, type t)
*              | Println(val src, type t)
* val = Constant(const) | Var(identifier) - this will definitely need clarification

* unary_operator = Not | Negate | Len | Ord | Chr -- not sure if Len, Ord, Chr should be here
* binary_operator = Mul | Div | Mod | Add | Sub | Gt | Gte | Lt | Lte | Eq | Neq | And | Or
*
* -- consts might just be our SemanticTypes tbh
 <const> ::= SemanticType - not sure on these yet
* */

/* Note: I'm using Vec cus i cba to use Box instead, its more convenient to use vec
 * albeit at some minor memory cost but its not a big deal
 * I wonder if I should use Strings or Idents or RenamedNames, seems like I'll mix them up a lot
 * For now my solution is to unify them all into a single type.
 * for now I'll use an (Ident, u32) similar to RenamedNames, and use u32's as my Eq,
 * However I might change them to regular old Strings if i do a lot of modification etc
 * */

use internment::ArcIntern;
use std::fmt::{self, Debug};
use std::hash::Hash;
use syntax::ast;
use syntax::types::SemanticType;

// Treat WackFunction's slightly differently from main
#[derive(Clone)]
pub struct WackProgram {
    pub functions: Vec<WackFunction>,
    pub main_body: Vec<WackInstr>,
}

#[derive(Clone)]
pub struct WackFunction {
    pub name: WackGlobIdent,
    pub params: Vec<WackTempIdent>,
    pub body: Vec<WackInstr>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum WackInstr {
    // TODO: at the end of this, remove redundant instructions
    // TODO: change any `src` and `dst` from "WackValue" bc it might not make sense to e.g. to "store" into a constant value
    // TODO: add code-docs to these instructions
    /// This instruction returns a value from within a function body.
    Return(WackValue),

    /// USAGE: `dst = op(src)`
    Unary {
        op: UnaryOp,
        src: WackValue,
        dst: WackTempIdent, // you can only store into an identifier
    },

    /// USAGE: `dst = op(src1, src2)`
    Binary {
        op: BinaryOp,
        src1: WackValue,
        src2: WackValue,
        dst: WackTempIdent, // you can only store into an identifier
    },

    /// USAGE: `dst = src`
    Copy {
        src: WackValue,
        dst: WackTempIdent, // you can only store into an identifier
    },

    // /// USAGE: `dst = &src`
    // ///
    // /// NOTE: [`src`] must be a variable, not constant.
    // GetAddress {
    //     src: WackValue,
    //     dst: WackValue,
    // }, // TODO: uncomment when its _actually_ needed
    //
    /// USAGE: `dst = *src_ptr`
    ///
    /// NOTE: [`src_ptr`] must be a pointer/memory address.h
    Load {
        src_ptr: WackValue,
        dst: WackTempIdent, // you can only store into an identifier
    },

    // /// USAGE: `*dst_ptr = src`
    // ///
    // /// NOTE: [`dst_ptr`] must be a pointer/memory address.
    // Store {
    //     src: WackValue,
    //     dst_ptr: WackValue,
    // },  // TODO: uncomment when its _actually_ needed
    //
    /// USAGE: `dst_ptr = offset + src_ptr + (index * scale)`
    ///
    /// NOTE: [`src_ptr`] and [`dst_ptr`] must be a pointers/memory addresses;
    ///       the base unit of the pointer-arithmetic is 1 byte, which is scaled by [`scale`].
    ///
    /// INTERNAL NOTE: DELETE LATER... -> this is a slightly modified version of `AddPtr(..)` that also includes offset
    ///                which I think is acceptable [since `LEA` can support that](https://stackoverflow.com/a/32011131/9700478),
    ///                e.g. `lea OFF(%rax, %rbx, SCALE), %rcx   # rcx = OFF + (rax + rbx*SCALE)`
    AddPtr {
        src_ptr: WackValue,
        index: WackValue,
        scale: usize,
        offset: i32,
        dst_ptr: WackTempIdent, // you can only store into an identifier
    },

    /// Copies the bytes of scalar value represented by [`src`], to the memory location (plus offset)
    /// of the object represented by the value [`dst_ptr`].
    ///
    /// It can be seen as an automated version of C's `memcpy`.
    CopyToOffset {
        src: WackValue,
        dst_ptr: WackValue, // you can only store into an identifier
        offset: i32,
    },

    /// Performs checked array-access, indexing into the array at [`src_array_ptr`] and storing
    /// the pointer of the element corresponding to [`index`] into the [`dst_elem_ptr`] operand.
    ///
    /// The index-value is checked against the array-length, and if not  0 <= index < length, then
    /// an out-of-bounds error will be thrown at runtime.
    /// The instruction will handle any offsetting that needs to be done.
    ArrayAccess {
        src_array_ptr: WackValue,
        index: WackValue,
        scale: usize,
        dst_elem_ptr: WackTempIdent, // you can only store into an identifier
    },

    Jump(WackTempIdent),
    JumpIfZero {
        condition: WackValue,
        target: WackTempIdent, // you can only store into an identifier
    },
    JumpIfNotZero {
        condition: WackValue,
        target: WackTempIdent, // you can only store into an identifier
    },
    Label(WackTempIdent),
    FunCall {
        fun_name: WackGlobIdent,
        args: Vec<WackValue>,
        dst: WackTempIdent, // you can only store into an identifier
    },
    Read {
        dst: WackTempIdent, // you can only store into an identifier
        ty: WackReadType,
    },

    /// Allocates [`size`] bytes on the heap (or crashes with out-of-memory runtime error) and
    /// stores the memory address of the start of the allocated memory-region in [`dst`].
    ///    /// It can be seen as an automated version of C's `malloc`.
    Alloc {
        size: usize,
        dst_ptr: WackTempIdent, // you can only store into an identifier
    },

    /// This frees the memory associated with the pointer that the value holds, without
    /// checking if the pointer is `null` or not.
    /// If it is `null`, nothing is done and no runtime errors occur.
    FreeUnchecked(WackValue),

    /// This frees the memory associated with the pointer that the value holds, checking that
    /// the pointer isn't `null`.
    /// If it is `null`, a runtime null-pointer-dereference error occurs.
    FreeChecked(WackValue),

    /// Checks that the pointer represented by this value isn't `null`.
    /// If it is `null`, a runtime null-pointer-dereference error occurs.
    NullPtrGuard(WackValue),

    Exit(WackValue),
    Print {
        src: WackValue,
        ty: WackPrintType,
    },
    Println {
        src: WackValue,
        ty: WackPrintType,
    },
}

impl WackInstr {
    // The default index for pointer arithmetic is zero, i.e. the very start
    const DEFAULT_ADD_PTR_INDEX: WackValue = WackValue::Literal(WackLiteral::Int(0));

    // The default scale for pointer arithmetic should be one byte.
    const DEFAULT_ADD_PTR_SCALE: usize = 1;

    // The default offset for pointer arithmetic is zero, i.e. no offset
    const DEFAULT_ADD_PTR_OFFSET: i32 = 0;

    /// Creates a pointer-arithmetic instruction that simply adds a fixed offset.
    #[inline]
    #[must_use]
    pub const fn add_ptr_offset(src_ptr: WackValue, offset: i32, dst_ptr: WackTempIdent) -> Self {
        Self::AddPtr {
            src_ptr,
            index: Self::DEFAULT_ADD_PTR_INDEX,
            scale: Self::DEFAULT_ADD_PTR_SCALE,
            offset,
            dst_ptr,
        }
    }

    /// Creates a pointer-arithmetic instruction that, provided the scale (in bytes) of each index,
    /// simply indexes into the array-like region with no offset.
    #[inline]
    #[must_use]
    pub const fn add_ptr_index(
        src_ptr: WackValue,
        index: WackValue,
        scale: usize,
        dst_ptr: WackTempIdent,
    ) -> Self {
        Self::AddPtr {
            src_ptr,
            index,
            scale,
            offset: Self::DEFAULT_ADD_PTR_OFFSET,
            dst_ptr,
        }
    }
}

/// A type-argument to the read instruction: this is a type-hint purely, the underlying data
/// could be of any type - for decoupling concerns since it doesn't need to be nested
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum WackReadType {
    Int,
    Char,
}

impl WackReadType {
    pub fn from_semantic_type(semantic_ty: SemanticType) -> Result<Self, Box<str>> {
        match semantic_ty {
            SemanticType::Int => Ok(Self::Int),
            SemanticType::Char => Ok(Self::Char),
            _ => Err(format!("incorrect semantic type found `{}`", semantic_ty).into()),
        }
    }
}

/// A type-argument to the print instruction: this is a type-hint purely, the underlying data
/// could be of any type - for decoupling concerns since it doesn't need to be nested
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum WackPrintType {
    Int,
    Bool,
    Char,
    StringOrCharArray,
    OtherArray,
    /// If null-ptr, must print "nil"
    Pair,
}

impl WackPrintType {
    pub fn from_semantic_type(semantic_ty: SemanticType) -> Result<Self, Box<str>> {
        match semantic_ty {
            SemanticType::Int => Ok(Self::Int),
            SemanticType::Bool => Ok(Self::Bool),
            SemanticType::Char => Ok(Self::Char),
            SemanticType::String => Ok(Self::StringOrCharArray),
            SemanticType::Array(inner_ty) => match &*inner_ty {
                SemanticType::Char => Ok(Self::StringOrCharArray),
                SemanticType::AnyType | SemanticType::Error(_) => {
                    Err(format!("found error/any semantic type `{}`", inner_ty).into())
                }
                _ => Ok(Self::OtherArray),
            },
            SemanticType::Pair(_, _) | SemanticType::ErasedPair => Ok(Self::Pair),
            SemanticType::AnyType | SemanticType::Error(_) => {
                Err(format!("found error/any semantic type `{}`", semantic_ty).into())
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WackValue {
    Literal(WackLiteral), // My only concern is the error type on SemanticType
    Var(WackTempIdent),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WackLiteral {
    Int(i32),
    Bool(WackBool), // smallest possible repr is 1 byte
    Char(WackChar), // 7-bit ASCII fits within 1 byte
    StringLit(ArcIntern<str>),
    NullPair,
}

/// A 1-byte representation of a boolean value - the smallest possible.
#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct WackBool(u8);

// impls related to `WackBool`
pub mod wack_bool {
    use crate::wackir::WackBool;
    use std::mem;
    use thiserror::Error;

    #[derive(Error, Debug)]
    #[error("Expected a boolean, found byte `{0}`")]
    pub struct NonBooleanByteError(u8);

    impl WackBool {
        pub const TRUE: Self = Self(1);
        pub const FALSE: Self = Self(0);

        #[allow(clippy::as_conversions)]
        #[inline]
        #[must_use]
        pub const fn from_bool(b: bool) -> Self {
            Self(b as u8)
        }

        #[inline]
        #[must_use]
        pub const fn into_bool(self) -> bool {
            // SAFETY: the only way to construct `WackBool` is by making sure it is a boolean.
            unsafe { mem::transmute(self) }
        }

        /// # Safety
        /// Only use this if you are _sure_ the [`u8`] byte is actually a boolean.
        #[inline]
        #[must_use]
        pub const unsafe fn from_u8_unchecked(r#u8: u8) -> Self {
            Self(r#u8)
        }

        /// # Errors
        /// Only [`u8`] bytes which are _actually_ booleans can be used to create [`WackBool`].
        #[inline]
        pub const fn try_from_u8(r#u8: u8) -> Result<Self, NonBooleanByteError> {
            match r#u8 {
                0 | 1 => Ok(Self(r#u8)),
                _ => Err(NonBooleanByteError(r#u8)),
            }
        }

        #[inline]
        #[must_use]
        pub const fn into_u8(self) -> u8 {
            self.0
        }
    }

    impl From<bool> for WackBool {
        #[inline]
        fn from(b: bool) -> Self {
            Self::from_bool(b)
        }
    }

    impl From<WackBool> for bool {
        #[inline]
        fn from(wack_bool: WackBool) -> Self {
            wack_bool.into_bool()
        }
    }

    impl TryFrom<u8> for WackBool {
        type Error = NonBooleanByteError;
        #[inline]
        fn try_from(value: u8) -> Result<Self, Self::Error> {
            Self::try_from_u8(value)
        }
    }

    impl From<WackBool> for u8 {
        #[inline]
        fn from(wack_bool: WackBool) -> Self {
            wack_bool.into_u8()
        }
    }
}

/// A 1-byte representation of 7-bit ASCII value - the smallest possible.
#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct WackChar(u8);

// impls related to `WackChar`
pub mod wack_char {
    use crate::wackir::WackChar;
    use thiserror::Error;

    #[derive(Error, Debug)]
    #[error("Expected an ASCII character, found non-ASCII character `{0}`")]
    pub struct NonAsciiCharError(char);

    #[derive(Error, Debug)]
    #[error("Expected an ASCII byte, found non-ASCII byte `{0}`")]
    pub struct NonAsciiByteError(u8);

    impl WackChar {
        /// # Safety
        /// Only use this if you are _sure_ the [`char`] is actually ASCII.
        #[inline]
        #[must_use]
        pub const unsafe fn from_char_unchecked(r#char: char) -> Self {
            Self(r#char as u8)
        }

        /// # Errors
        /// Only [`char`]s which are _actually_ ASCII can be used to create [`WackChar`].
        #[inline]
        pub const fn try_from_char(r#char: char) -> Result<Self, NonAsciiCharError> {
            match r#char {
                '\x00'..='\x7F' => Ok(Self(r#char as u8)),
                _ => Err(NonAsciiCharError(r#char)),
            }
        }

        #[allow(clippy::as_conversions)]
        #[inline]
        #[must_use]
        pub const fn into_char(self) -> char {
            self.0 as char
        }

        /// # Safety
        /// Only use this if you are _sure_ the [`u8`] byte is actually ASCII.
        #[inline]
        #[must_use]
        pub const unsafe fn from_u8_unchecked(r#u8: u8) -> Self {
            Self(r#u8)
        }

        /// # Errors
        /// Only [`u8`] bytes which are _actually_ ASCII can be used to create [`WackChar`].
        #[inline]
        pub const fn try_from_u8(r#u8: u8) -> Result<Self, NonAsciiByteError> {
            match r#u8 {
                0x00..=0x7F => Ok(Self(r#u8)),
                _ => Err(NonAsciiByteError(r#u8)),
            }
        }

        #[inline]
        #[must_use]
        pub const fn into_u8(self) -> u8 {
            self.0
        }
    }

    impl TryFrom<char> for WackChar {
        type Error = NonAsciiCharError;

        #[inline]
        fn try_from(value: char) -> Result<Self, Self::Error> {
            Self::try_from_char(value)
        }
    }

    impl From<WackChar> for char {
        #[inline]
        #[must_use]
        fn from(value: WackChar) -> Self {
            value.into_char()
        }
    }

    impl TryFrom<u8> for WackChar {
        type Error = NonAsciiByteError;
        #[inline]
        fn try_from(value: u8) -> Result<Self, Self::Error> {
            Self::try_from_u8(value)
        }
    }

    impl From<WackChar> for u8 {
        #[inline]
        #[must_use]
        fn from(value: WackChar) -> Self {
            value.into_u8()
        }
    }
}

// I know that these are the same as the ones in ast.rs but I'm not sure if I want to
// couple them together or not. For now I'll separate them just in case I need to move
// Len, Ord, Chr somewhere else
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum UnaryOp {
    BNot,
    LNot,
    Negate,
    Len,
    Ord,
    Chr,
}

// See UnaryOperator explanation above
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum BinaryOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
    BAnd,
    BXor,
    BOr,
    LAnd,
    LOr,
}

impl From<ast::BinaryOper> for BinaryOp {
    #[inline]
    fn from(binop: ast::BinaryOper) -> Self {
        match binop {
            ast::BinaryOper::Mul => Self::Mul,
            ast::BinaryOper::Div => Self::Div,
            ast::BinaryOper::Mod => Self::Mod,
            ast::BinaryOper::Add => Self::Add,
            ast::BinaryOper::Sub => Self::Sub,
            ast::BinaryOper::Lte => Self::Lte,
            ast::BinaryOper::Lt => Self::Lt,
            ast::BinaryOper::Gte => Self::Gte,
            ast::BinaryOper::Gt => Self::Gt,
            ast::BinaryOper::Eq => Self::Eq,
            ast::BinaryOper::Neq => Self::Neq,
            ast::BinaryOper::BAnd => Self::BAnd,
            ast::BinaryOper::BXor => Self::BXor,
            ast::BinaryOper::BOr => Self::BOr,
            ast::BinaryOper::LAnd => Self::LAnd,
            ast::BinaryOper::LOr => Self::LOr,
        }
    }
}

impl From<ast::UnaryOper> for UnaryOp {
    #[inline]
    fn from(unop: ast::UnaryOper) -> Self {
        match unop {
            ast::UnaryOper::BNot => Self::BNot,
            ast::UnaryOper::LNot => Self::LNot,
            ast::UnaryOper::Minus => Self::Negate,
            ast::UnaryOper::Len => Self::Len,
            ast::UnaryOper::Ord => Self::Ord,
            ast::UnaryOper::Chr => Self::Chr,
        }
    }
}

// TODO: check that these give the right answers
impl From<ast::Liter> for WackLiteral {
    #[inline]
    fn from(liter: ast::Liter) -> Self {
        match liter {
            ast::Liter::IntLiter(i) => Self::Int(i),
            ast::Liter::BoolLiter(b) => Self::Bool(b.into()),
            ast::Liter::CharLiter(c) => {
                // SAFETY: literal characters from the parser are guaranteed to be ASCII
                Self::Char(unsafe { WackChar::from_char_unchecked(c) })
            }
            ast::Liter::StrLiter(s) => Self::StringLit(s),
            ast::Liter::PairLiter => Self::NullPair,
        }
    }
}

/// An identified used for global-scope items like functions, who's names
/// do not need to be generated using an increasing counter.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct WackGlobIdent(ArcIntern<str>);

// impls relating to `WackGlobIdent`
mod wack_glob_ident {
    use crate::wackir::WackGlobIdent;
    use internment::ArcIntern;
    use syntax::ast;

    impl WackGlobIdent {
        #[must_use]
        #[inline]
        pub const fn new(r#str: ArcIntern<str>) -> Self {
            Self(r#str)
        }

        #[must_use]
        #[inline]
        pub fn from_ref(r#str: &ArcIntern<str>) -> Self {
            Self(r#str.clone())
        }

        #[must_use]
        #[inline]
        pub fn into_inner(self) -> ArcIntern<str> {
            self.0
        }
    }

    impl From<ast::Ident> for WackGlobIdent {
        #[must_use]
        #[inline]
        fn from(value: ast::Ident) -> Self {
            Self::new(value.into_inner())
        }
    }

    impl From<&ast::Ident> for WackGlobIdent {
        #[must_use]
        #[inline]
        fn from(value: &ast::Ident) -> Self {
            Self::from_ref(value.inner())
        }
    }

    impl From<WackGlobIdent> for String {
        #[inline]
        fn from(ident: WackGlobIdent) -> Self {
            ident.0.to_string()
        }
    }

    impl<'a> From<&'a WackGlobIdent> for &'a str {
        #[inline]
        fn from(mid_ident: &'a WackGlobIdent) -> &'a str {
            &mid_ident.0
        }
    }
}

/// Identifier used for more locally-scoped items, such as temporary variables,
/// or control-flow labels generated after lowering the [`ast`].
///
/// Invariant: The usize's are unique so should reuse the global counter when possible.
///            DO NOT UNDER ANY CIRCUMSTANCES USE THE SAME usize FOR TWO DIFFERENT IDENTIFIERS!!
#[derive(Clone)]
pub struct WackTempIdent(ast::Ident, usize);

// impls relating to `WackTempIdent`
pub mod wack_temp_ident {
    use crate::ast_transform::AstLoweringCtx;
    use crate::ast_transform::ast_lowering_ctx::With;
    use crate::wackir::WackTempIdent;
    use std::fmt;
    use std::fmt::{Debug, Formatter};
    use std::hash::{Hash, Hasher};
    use syntax::ast;
    use syntax::rename::RenamedName;

    impl Debug for WackTempIdent {
        #[inline]
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "{}.{}", self.0, self.1)
        }
    }

    impl PartialEq for WackTempIdent {
        #[inline]
        fn eq(&self, other: &Self) -> bool {
            self.1 == other.1
        }
    }

    impl Eq for WackTempIdent {}

    impl Hash for WackTempIdent {
        #[inline]
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.1.hash(state);
        }
    }

    impl From<RenamedName> for WackTempIdent {
        #[inline]
        fn from(value: RenamedName) -> Self {
            Self(value.ident.clone(), value.uuid)
        }
    }

    impl From<With<ast::Ident, &mut usize>> for WackTempIdent {
        #[inline]
        fn from(value: With<ast::Ident, &mut usize>) -> Self {
            let (ident, counter) = value.into_components();
            *counter += 1;
            Self(ident, *counter)
        }
    }

    impl From<With<ast::Ident, &mut AstLoweringCtx>> for WackTempIdent {
        #[inline]
        fn from(mut value: With<ast::Ident, &mut AstLoweringCtx>) -> Self {
            let counter = value.ctx_mut().inc_ident_counter();
            Self(value.into_inner(), counter)
        }
    }

    impl From<WackTempIdent> for String {
        #[inline]
        fn from(mid_ident: WackTempIdent) -> Self {
            format!("{}.{}", mid_ident.0, mid_ident.1)
        }
    }

    impl<'a> From<&'a WackTempIdent> for &'a str {
        #[inline]
        fn from(mid_ident: &'a WackTempIdent) -> &'a str {
            mid_ident.0.inner()
        }
    }
}

/* ====================== PRETTY PRINTER ====================== */

impl fmt::Debug for WackProgram {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "WackProgram {{")?;

        // Print functions
        writeln!(f, "  functions: [")?;
        for function in &self.functions {
            write!(f, "    ")?;
            function.fmt(f)?;
            writeln!(f)?;
        }
        writeln!(f, "  ]")?;

        // Print main body
        writeln!(f, "  main_body: [")?;
        for instruction in &self.main_body {
            write!(f, "    ")?;
            instruction.fmt(f)?;
            writeln!(f)?;
        }
        writeln!(f, "  ]")?;

        write!(f, "}}")
    }
}

impl fmt::Debug for WackFunction {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "WackFunction {{")?;
        writeln!(f, "    name: {:?},", self.name)?;
        writeln!(f, "    params: {:?},", self.params)?;
        writeln!(f, "    body: [")?;
        for instruction in &self.body {
            write!(f, "      ")?;
            instruction.fmt(f)?;
            writeln!(f)?;
        }
        write!(f, "    ]")?;
        write!(f, "  }}")
    }
}

impl fmt::Debug for WackInstr {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Return(val) => write!(f, "Return({:?})", val),
            // Self::ZeroExtend { src, dst } => {
            //     write!(f, "ZeroExtend {{ src: {:?}, dst: {:?} }}", src, dst)
            // } // TODO: uncomment when its _actually_ needed
            Self::Unary { op, src, dst } => write!(
                f,
                "Unary {{ op: {:?}, src: {:?}, dst: {:?} }}",
                op, src, dst
            ),
            Self::Binary {
                op,
                src1,
                src2,
                dst,
            } => write!(
                f,
                "Binary {{ op: {:?}, src1: {:?}, src2: {:?}, dst: {:?} }}",
                op, src1, src2, dst
            ),
            Self::Copy { src, dst } => {
                write!(f, "Copy {{ src: {:?}, dst: {:?} }}", src, dst)
            }
            Self::Load { src_ptr, dst } => {
                write!(f, "Load {{ src_ptr: {:?}, dst: {:?} }}", src_ptr, dst)
            }
            // Self::Store { src, dst_ptr } => {
            //     write!(f, "Store {{ src: {:?}, dst_ptr: {:?} }}", src, dst_ptr)
            // } // TODO: uncomment when its _actually_ needed
            Self::AddPtr {
                src_ptr,
                index,
                scale,
                offset,
                dst_ptr,
            } => write!(
                f,
                "AddPtr {{ src_ptr: {:?}, index: {:?}, scale: {:?}, offset: {:?}, dst_ptr: {:?} }}",
                src_ptr, index, scale, offset, dst_ptr
            ),
            Self::CopyToOffset {
                src,
                dst_ptr,
                offset,
            } => write!(
                f,
                "CopyToOffset {{ src: {:?}, dst_ptr: {:?}, offset: {:?} }}",
                src, dst_ptr, offset
            ),
            // Self::CopyFromOffset { src, offset, dst } => write!(
            //     f,
            //     "CopyFromOffset {{ src: {:?}, offset: {:?}, dst: {:?} }}",
            //     src, offset, dst
            // ), // TODO: uncomment when its _actually_ needed
            Self::Jump(target) => write!(f, "Jump({:?})", target),
            Self::JumpIfZero { condition, target } => write!(
                f,
                "JumpIfZero {{ condition: {:?}, target: {:?} }}",
                condition, target
            ),
            Self::JumpIfNotZero { condition, target } => write!(
                f,
                "JumpIfNotZero {{ condition: {:?}, target: {:?} }}",
                condition, target
            ),
            Self::Label(label) => write!(f, "Label({:?})", label),
            Self::FunCall {
                fun_name,
                args,
                dst,
            } => write!(
                f,
                "FunCall {{ fun_name: {:?}, args: {:?}, dst: {:?} }}",
                fun_name, args, dst
            ),
            Self::Read { dst, ty } => {
                write!(f, "Read {{ dst: {:?}, ty: {:?} }}", dst, ty)
            }
            Self::FreeChecked(val) => write!(f, "FreeChecked({:?})", val),
            Self::FreeUnchecked(val) => write!(f, "FreeUnchecked({:?})", val),
            Self::Exit(val) => write!(f, "Exit({:?})", val),
            Self::Print { src, ty } => {
                write!(f, "Print {{ src: {:?}, ty: {:?} }}", src, ty)
            }
            Self::Println { src, ty } => {
                write!(f, "Println {{ src: {:?}, ty: {:?} }}", src, ty)
            }
            Self::ArrayAccess {
                src_array_ptr,
                index,
                scale,
                dst_elem_ptr,
            } => {
                write!(
                    f,
                    "ArrayAccess {{ src_array_ptr: {:?}, index: {:?}, scale: {:?}, dst_elem_ptr: {:?} }}",
                    src_array_ptr, index, scale, dst_elem_ptr
                )
            }
            Self::Alloc { size, dst_ptr } => {
                write!(f, "Alloc {{ size: {:?}, dst_ptr: {:?} }}", size, dst_ptr)
            }
            Self::NullPtrGuard(ptr) => write!(f, "NullPtrGuard {{ ptr: {:?} }}", ptr),
        }
    }
}

/* ====================== TESTS * ====================== */
#[cfg(test)]
mod tests {
    use super::*;

    #[allow(clippy::undocumented_unsafe_blocks)]
    #[test]
    fn test_liter_conversion() {
        // Test integer literal conversion
        let int_liter = ast::Liter::IntLiter(42);
        assert_eq!(WackLiteral::from(int_liter), WackLiteral::Int(42));

        // Test boolean literal conversion
        let bool_liter_true = ast::Liter::BoolLiter(true);
        let bool_liter_false = ast::Liter::BoolLiter(false);
        assert_eq!(
            WackLiteral::from(bool_liter_true),
            WackLiteral::Bool(WackBool::TRUE)
        );
        assert_eq!(
            WackLiteral::from(bool_liter_false),
            WackLiteral::Bool(WackBool::FALSE)
        );

        // Test char literal conversion
        let char_liter = ast::Liter::CharLiter('A');
        let char_liter2 = ast::Liter::CharLiter('a');
        assert_eq!(
            WackLiteral::from(char_liter),
            WackLiteral::Char(unsafe { WackChar::from_u8_unchecked(65) })
        ); // ASCII value of 'A'
        assert_eq!(
            WackLiteral::from(char_liter2),
            WackLiteral::Char(unsafe { WackChar::from_u8_unchecked(97) })
        ); // ASCII value of 'a'

        // Test pair literal conversion
        let pair_liter = ast::Liter::PairLiter;
        assert_eq!(WackLiteral::from(pair_liter), WackLiteral::NullPair);
    }
}
