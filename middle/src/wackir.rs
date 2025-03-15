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
* -- consts might just be our thir::Type-s tbh
 <const> ::= thir::Type - not sure on these yet
* */

/* Note: I'm using Vec cus i cba to use Box instead, its more convenient to use vec
 * albeit at some minor memory cost but its not a big deal
 * I wonder if I should use Strings or Idents or RenamedNames, seems like I'll mix them up a lot
 * For now my solution is to unify them all into a single type.
 * for now I'll use an (Ident, u32) similar to RenamedNames, and use u32's as my Eq,
 * However I might change them to regular old Strings if i do a lot of modification etc
 * */

use crate::alias::InternStr;
use derive_more::Display;
use frontend::parsing::ast;
use frontend::wacc_hir::hir;
use frontend::wacc_thir::types::{BaseType, Type};
use std::fmt::{self, Debug};
use std::hash::Hash;

type PredefinedFunction = String;

// Treat WackFunction's slightly differently from main
#[derive(Clone)]
pub struct WackProgram {
    pub functions: Vec<WackFunction>,
    pub main_body: Vec<WackInstr>,
}

impl WackProgram {
    #[inline]
    #[must_use]
    pub fn has_read_instr(&self) -> bool {
        self.functions.iter().any(WackFunction::has_read_instr)
            || WackInstr::has_read_instr(&self.main_body)
    }
}

#[derive(Clone)]
pub struct WackFunction {
    pub name: WackGlobIdent,
    pub params: Vec<WackTempIdent>,
    pub body: Vec<WackInstr>,
}

impl WackFunction {
    #[inline]
    #[must_use]
    pub fn has_read_instr(&self) -> bool {
        WackInstr::has_read_instr(&self.body)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum WackInstr {
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

    //
    /// USAGE: `dst = *src_ptr`
    ///
    /// NOTE: [`src_ptr`] must be a pointer/memory address.h
    Load {
        src_ptr: WackValue,
        dst: WackTempIdent,
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
        dst_ptr: WackTempIdent,
    },

    /// Copies the bytes of scalar value represented by [`src`], to the memory location (plus offset)
    /// of the object represented by the value [`dst_ptr`].
    ///
    /// It can be seen as an automated version of C's `memcpy`.
    CopyToOffset {
        src: WackValue,
        dst_ptr: WackTempIdent,
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
        dst_elem_ptr: WackTempIdent,
    },

    Jump(WackTempIdent),
    JumpIfZero {
        condition: WackValue,
        target: WackTempIdent,
    },
    JumpIfNotZero {
        condition: WackValue,
        target: WackTempIdent,
    },
    JumpToHandler(PredefinedFunction), // Jump to a runtime error handler
    Label(WackTempIdent),
    FunCall {
        fun_name: WackGlobIdent,
        args: Vec<WackValue>,
        dst: WackTempIdent,
    },
    Read {
        dst: WackTempIdent,
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

/// A type-argument to the read instruction: this is a type-hint purely, the underlying data
/// could be of any type - for decoupling concerns since it doesn't need to be nested
#[derive(Clone, Copy, PartialEq, Eq, Debug, Display)]
pub enum WackReadType {
    #[display("int")]
    Int,
    #[display("char")]
    Char,
}

/// A type-argument to the print instruction: this is a type-hint purely, the underlying data
/// could be of any type - for decoupling concerns since it doesn't need to be nested
#[derive(Clone, Copy, PartialEq, Eq, Debug, Display)]
pub enum WackPrintType {
    #[display("int")]
    Int,
    #[display("bool")]
    Bool,
    #[display("char")]
    Char,
    #[display("string")]
    StringOrCharArray,
    #[display("array[]")]
    OtherArray,
    /// If null-ptr, must print "nil"
    #[display("pair")]
    Pair,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Display)]
pub enum WackValue {
    #[display("{_0}")]
    Literal(WackLiteral), // Literals are all constants
    #[display("{_0}")]
    Var(WackTempIdent),
}

pub type WackBool = bool;
pub type WackChar = u8;
pub(crate) const TRUE: WackBool = true;
pub(crate) const FALSE: WackBool = false;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WackLiteral {
    Int(i32),
    Bool(WackBool), // smallest possible repr is 1 byte
    Char(WackChar), // 7-bit ASCII fits within 1 byte
    StringLit(InternStr),
    NullPair,
}

// I know that these are the same as the ones in ast.rs but I'm not sure if I want to
// couple them together or not. For now I'll separate them just in case I need to move
// Len, Ord, Chr somewhere else
#[derive(Clone, Debug, PartialEq, Eq, Copy, Display)]
pub enum UnaryOp {
    #[display("!")]
    LNot,
    #[display("-")]
    Negate,
    #[display("len")]
    Len,
    #[display("ord")]
    Ord,
    #[display("chr")]
    Chr,
}

// See UnaryOperator explanation above
#[derive(Clone, Debug, PartialEq, Eq, Copy, Display)]
pub enum BinaryOp {
    #[display("*")]
    Mul,
    #[display("/")]
    Div,
    #[display("%")]
    Mod,
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display(">")]
    Gt,
    #[display(">=")]
    Gte,
    #[display("<")]
    Lt,
    #[display("<=")]
    Lte,
    #[display("==")]
    Eq,
    #[display("!=")]
    Neq,
    #[display("&&")]
    LAnd,
    #[display("||")]
    LOr,
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

    #[inline]
    #[must_use]
    pub fn has_read_instr<I: AsRef<[Self]>>(instrs: I) -> bool {
        for instr in instrs.as_ref() {
            if let Self::Read { .. } = *instr {
                return true;
            }
        }
        false
    }
}

impl WackPrintType {
    pub fn from_thir_type(thir_type: Type) -> Result<Self, Box<str>> {
        match thir_type.clone() {
            Type::BaseType(BaseType::Int) => Ok(Self::Int),
            Type::BaseType(BaseType::Bool) => Ok(Self::Bool),
            Type::BaseType(BaseType::Char) => Ok(Self::Char),
            Type::BaseType(BaseType::String) => Ok(Self::StringOrCharArray),
            Type::ArrayType(boxed) => match (*boxed).elem_type {
                Type::BaseType(BaseType::Char) => Ok(Self::StringOrCharArray),
                Type::Any => Err(format!("found any THIR type `{}`", thir_type).into()),
                _ => Ok(Self::OtherArray),
            },
            Type::PairType(_) => Ok(Self::Pair),
            Type::Any => Err(format!("found any THIR type `{}`", thir_type).into()),
        }
    }
}

impl WackReadType {
    pub fn from_thir_type(thir_type: Type) -> Result<Self, Box<str>> {
        match thir_type {
            Type::BaseType(BaseType::Int) => Ok(Self::Int),
            Type::BaseType(BaseType::Char) => Ok(Self::Char),
            _ => Err(format!("incorrect THIR type found `{}`", thir_type).into()),
        }
    }
}

impl From<hir::BinaryOper> for BinaryOp {
    #[inline]
    fn from(binop: hir::BinaryOper) -> Self {
        match binop {
            hir::BinaryOper::Mul => Self::Mul,
            hir::BinaryOper::Div => Self::Div,
            hir::BinaryOper::Mod => Self::Mod,
            hir::BinaryOper::Add => Self::Add,
            hir::BinaryOper::Sub => Self::Sub,
            hir::BinaryOper::Lte => Self::Lte,
            hir::BinaryOper::Lt => Self::Lt,
            hir::BinaryOper::Gte => Self::Gte,
            hir::BinaryOper::Gt => Self::Gt,
            hir::BinaryOper::Eq => Self::Eq,
            hir::BinaryOper::Neq => Self::Neq,
            hir::BinaryOper::LAnd => Self::LAnd,
            hir::BinaryOper::LOr => Self::LOr,
        }
    }
}

impl From<hir::UnaryOper> for UnaryOp {
    #[inline]
    fn from(unop: hir::UnaryOper) -> Self {
        match unop {
            hir::UnaryOper::LNot => Self::LNot,
            hir::UnaryOper::Minus => Self::Negate,
            hir::UnaryOper::Len => Self::Len,
            hir::UnaryOper::Ord => Self::Ord,
            hir::UnaryOper::Chr => Self::Chr,
        }
    }
}

// TODO: check that these give the right answers
impl From<hir::Liter> for WackLiteral {
    #[inline]
    fn from(liter: hir::Liter) -> Self {
        match liter {
            hir::Liter::IntLiter(i) => Self::Int(i),
            hir::Liter::BoolLiter(b) => Self::Bool(b),
            hir::Liter::CharLiter(c) => Self::Char(c),
            hir::Liter::StrLiter(s) => Self::StringLit(s),
            hir::Liter::PairLiter => Self::NullPair,
        }
    }
}

/// An identified used for global-scope items like functions, who's names
/// do not need to be generated using an increasing counter.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
#[repr(transparent)]
#[display("{_0}")]
pub struct WackGlobIdent(InternStr);

// impls relating to `WackGlobIdent`
mod wack_glob_ident {
    use crate::alias::InternStr;
    use crate::wackir::WackGlobIdent;
    use frontend::parsing::ast;

    impl WackGlobIdent {
        #[must_use]
        #[inline]
        pub const fn new(r#str: InternStr) -> Self {
            Self(r#str)
        }

        #[must_use]
        #[inline]
        pub const fn from_ref(r#str: &InternStr) -> Self {
            Self(*r#str)
        }

        #[must_use]
        #[inline]
        pub const fn into_inner(self) -> InternStr {
            self.0
        }
    }

    impl From<ast::Ident> for WackGlobIdent {
        #[inline]
        fn from(value: ast::Ident) -> Self {
            Self::new(value.into_inner())
        }
    }

    impl From<&ast::Ident> for WackGlobIdent {
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
/// NOTE: DO NOT CHANGE THE IMPLEMENTATION OF DISPLAY UNDER ANY CIRCUMSTANCE
/// We use this display for predefined functions and local static analysis
/// Which affects emission
#[derive(Clone, Display)]
#[display("{_0}.{_1}")]
pub struct WackTempIdent(ast::Ident, usize);

// impls relating to `WackTempIdent`
pub mod wack_temp_ident {
    use crate::alias::InternStr;
    use crate::thir_transform::thir_lowering_ctx::With;
    use crate::thir_transform::ThirLoweringCtx;
    use crate::wackir::WackTempIdent;
    use frontend::parsing::ast;
    use frontend::wacc_hir::hir;
    use std::fmt;
    use std::fmt::{Debug, Formatter};
    use std::hash::{Hash, Hasher};

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

    impl From<hir::Ident> for WackTempIdent {
        #[inline]
        fn from(value: hir::Ident) -> Self {
            Self(value.ident.clone(), value.uuid)
        }
    }

    impl From<hir::LoopLabel> for WackTempIdent {
        #[inline]
        fn from(value: hir::LoopLabel) -> Self {
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

    impl From<With<ast::Ident, &mut ThirLoweringCtx>> for WackTempIdent {
        #[inline]
        fn from(mut value: With<ast::Ident, &mut ThirLoweringCtx>) -> Self {
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

    impl From<WackTempIdent> for InternStr {
        #[inline]
        fn from(mid_ident: WackTempIdent) -> InternStr {
            mid_ident.0.into_inner()
        }
    }

    impl WackTempIdent {
        #[inline]
        pub fn create_new(ident: &str, counter: &mut usize) -> Self {
            Self(ast::Ident::from_str(ident), *counter)
        }

        // Used for generating WackTempIdent's that have a constant counter
        // Used for predefined functions currently as we don't care about their counter
        #[must_use]
        #[inline]
        pub fn new(ident: &str, counter: usize) -> Self {
            Self(ast::Ident::from_str(ident), counter)
        }

        #[inline]
        pub fn get_id(&self) -> usize {
            self.1
        }

        // Used for predefined functions currently as we don't care about their counter
        #[inline]
        pub fn get_str(&self) -> &str {
            &self.0
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
            Self::JumpToHandler(fun_name) => write!(f, "JumpToHandler({:?})", fun_name),
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

impl fmt::Display for WackLiteral {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Self::Char(c) => write!(f, "'{}'", *c as char),
            Self::StringLit(s) => write!(f, "\"{}\"", s),
            Self::NullPair => write!(f, "null"),
        }
    }
}

impl fmt::Display for WackInstr {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Return(val) => write!(f, "Return {val}"),

            Self::Unary { op, src, dst } => {
                write!(f, "{dst} = {op} {src}")
            }
            Self::Binary {
                op,
                src1,
                src2,
                dst,
            } => write!(f, "{dst} = {src1} {op} {src2}"),
            Self::Copy { src, dst } => {
                write!(f, "{dst} = {src}")
            }
            Self::Load { src_ptr, dst } => {
                write!(f, "{dst} = *{src_ptr}")
            }

            Self::AddPtr {
                src_ptr,
                index,
                scale,
                offset,
                dst_ptr,
            } => write!(f, "{dst_ptr} = {src_ptr} + {index} * {scale} + {offset}",),
            Self::CopyToOffset {
                src,
                dst_ptr,
                offset,
            } => write!(f, "memcpy({dst_ptr} + {offset}, {src}, sizeof(src))",),
            Self::Jump(target) => write!(f, "Jump({target})"),
            Self::JumpIfZero { condition, target } => {
                write!(f, "Jump {target} if not {condition}")
            }
            Self::JumpIfNotZero { condition, target } => write!(f, "Jump {target} if {condition}"),
            Self::JumpToHandler(fun_name) => write!(f, "JumpToFunc({fun_name})"),
            Self::Label(label) => write!(f, "{label}:"),
            Self::FunCall {
                fun_name,
                args,
                dst,
            } => {
                let args_str = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{dst} = {fun_name}({args_str})")
            }
            Self::Read { dst, ty } => {
                write!(f, "{dst} = read {ty}")
            }
            Self::FreeChecked(val) => write!(f, "free({val})"),
            Self::FreeUnchecked(val) => write!(f, "free({val})"),
            Self::Exit(val) => write!(f, "exit({val})"),
            Self::Print { src, ty } => {
                write!(f, "print_{ty} ({src})")
            }
            Self::Println { src, ty } => {
                write!(f, "println_{ty} ({src})")
            }
            Self::ArrayAccess {
                src_array_ptr,
                index,
                scale: _scale,
                dst_elem_ptr,
            } => {
                write!(f, "{dst_elem_ptr} = address $ {src_array_ptr}[{index}]")
            }
            Self::Alloc { size, dst_ptr } => {
                write!(f, "{dst_ptr} = malloc({size})")
            }
            Self::NullPtrGuard(ptr) => write!(f, "IsNullPtr({ptr})"),
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
        let int_liter = hir::Liter::IntLiter(42);
        assert_eq!(WackLiteral::from(int_liter), WackLiteral::Int(42));

        // Test boolean literal conversion
        let bool_liter_true = hir::Liter::BoolLiter(true);
        let bool_liter_false = hir::Liter::BoolLiter(false);
        assert_eq!(WackLiteral::from(bool_liter_true), WackLiteral::Bool(TRUE));
        assert_eq!(
            WackLiteral::from(bool_liter_false),
            WackLiteral::Bool(FALSE)
        );

        // Test char literal conversion
        let char_liter = hir::Liter::CharLiter('A' as u8);
        let char_liter2 = hir::Liter::CharLiter('a' as u8);
        assert_eq!(WackLiteral::from(char_liter), WackLiteral::Char(65)); // ASCII value of 'A'
        assert_eq!(WackLiteral::from(char_liter2), WackLiteral::Char(97)); // ASCII value of 'a'

        // Test pair literal conversion
        let pair_liter = hir::Liter::PairLiter;
        assert_eq!(WackLiteral::from(pair_liter), WackLiteral::NullPair);
    }
}
