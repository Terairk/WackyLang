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
use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};
use syntax::{ast, rename::RenamedName, types::SemanticType};

// Treat WackFunction's slightly differently from main
#[derive(Clone)]
pub struct WackProgram {
    pub functions: Vec<WackFunction>,
    pub main_body: Vec<WackInstruction>,
}

#[derive(Clone)]
pub struct WackFunction {
    pub name: WackIdent, // function names correspond to labels, which are `WackirIdent`s
    pub params: Vec<WackIdent>,
    // Not sure if we need types, should be fine
    // if we have Symbol Table
    pub body: Vec<WackInstruction>,
}

#[derive(Clone)]
pub enum WackInstruction {
    // TODO: at the end of this, remove redundant instructions
    Return(WackValue),
    SignExtend {
        src: WackValue,
        dst: WackValue,
    },
    Truncate {
        src: WackValue,
        dst: WackValue,
    },
    ZeroExtend {
        src: WackValue,
        dst: WackValue,
    },
    Unary {
        op: UnaryOperator,
        src: WackValue,
        dst: WackValue,
    },
    Binary {
        op: BinaryOperator,
        src1: WackValue,
        src2: WackValue,
        dst: WackValue,
    },
    Copy {
        src: WackValue,
        dst: WackValue,
    },
    GetAddress {
        src: WackValue,
        dst: WackValue,
    },
    Load {
        src_ptr: WackValue,
        dst: WackValue,
    },
    Store {
        src: WackValue,
        dst_ptr: WackValue,
    },
    AddPtr {
        ptr: WackValue,
        index: WackValue,
        scale: u32,
        dst: WackIdent,
    },
    CopyToOffset {
        src: WackValue,
        dst: WackIdent,
        offset: u32,
    },
    CopyFromOffset {
        src: WackIdent,
        dst: WackValue,
        offset: u32,
    },
    Jump(WackIdent),
    JumpIfZero {
        condition: WackValue,
        target: WackIdent,
    },
    JumpIfNotZero {
        condition: WackValue,
        target: WackIdent,
    },
    Label(WackIdent),
    FunCall {
        fun_name: WackIdent, // function names correspond to labels, which are `WackirIdent`s
        args: Vec<WackValue>,
        dst: WackValue,
    },
    Read {
        dst: WackValue,
        ty: SemanticType,
    },
    Free(WackValue),
    Exit(WackValue),
    Print {
        src: WackValue,
        ty: SemanticType,
    },
    Println {
        src: WackValue,
        ty: SemanticType,
    },
}

#[derive(Clone, Debug)]
pub enum WackValue {
    Literal(WackLiteral), // My only concern is the error type on SemanticType
    Var(WackIdent),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WackLiteral {
    Int(i32),
    Bool(WackBool), // smallest possible repr is 1 byte
    Char(u8),       // 7-bit ASCII fits within 1 byte
    StringLit(ArcIntern<str>),
    NullPair,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct WackBool(pub u8);

impl From<u8> for WackBool {
    #[inline]
    fn from(r#u8: u8) -> Self {
        WackBool(r#u8)
    }
}

impl From<bool> for WackBool {
    #[inline]
    fn from(b: bool) -> Self {
        (b as u8).into()
    }
}

// I know that these are the same as the ones in ast.rs but I'm not sure if I want to
// couple them together or not. For now I'll separate them just in case I need to move
// Len, Ord, Chr somewhere else
// TODO: just use the UnaryOper from ast.rs
#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Not,
    Negate,
    Len,
    Ord,
    Chr,
}

// See UnaryOperator explanation above
#[derive(Clone, Debug)]
pub enum BinaryOperator {
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
    And,
    Or,
}

impl From<ast::BinaryOper> for BinaryOperator {
    #[inline]
    fn from(binop: ast::BinaryOper) -> Self {
        match binop {
            ast::BinaryOper::Mul => BinaryOperator::Mul,
            ast::BinaryOper::Div => BinaryOperator::Div,
            ast::BinaryOper::Mod => BinaryOperator::Mod,
            ast::BinaryOper::Add => BinaryOperator::Add,
            ast::BinaryOper::Sub => BinaryOperator::Sub,
            ast::BinaryOper::Lte => BinaryOperator::Lte,
            ast::BinaryOper::Lt => BinaryOperator::Lt,
            ast::BinaryOper::Gte => BinaryOperator::Gte,
            ast::BinaryOper::Gt => BinaryOperator::Gt,
            ast::BinaryOper::Eq => BinaryOperator::Eq,
            ast::BinaryOper::Neq => BinaryOperator::Neq,
            ast::BinaryOper::And => BinaryOperator::And,
            ast::BinaryOper::Or => BinaryOperator::Or,
        }
    }
}

impl From<ast::UnaryOper> for UnaryOperator {
    #[inline]
    fn from(unop: ast::UnaryOper) -> Self {
        match unop {
            ast::UnaryOper::Not => UnaryOperator::Not,
            ast::UnaryOper::Minus => UnaryOperator::Negate,
            ast::UnaryOper::Len => UnaryOperator::Len,
            ast::UnaryOper::Ord => UnaryOperator::Ord,
            ast::UnaryOper::Chr => UnaryOperator::Chr,
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
            ast::Liter::CharLiter(c) => Self::Char(c as u8),
            ast::Liter::StrLiter(s) => Self::StringLit(s),
            ast::Liter::PairLiter => Self::NullPair,
        }
    }
}

/// Invariant: The usize's are unique so should reuse the global counter when possible
/// DO NOT UNDER ANY CIRCUMSTANCES USE THE SAME usize FOR TWO DIFFERENT IDENTIFIERS
#[derive(Clone)]
pub struct WackIdent(ast::Ident, usize);

impl Debug for WackIdent {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.0, self.1)
    }
}

impl PartialEq for WackIdent {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}

impl Eq for WackIdent {}

impl Hash for WackIdent {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

pub trait ConvertToWackIdent {
    fn to_wack_ident(&self, counter: &mut usize) -> WackIdent;
}

impl ConvertToWackIdent for RenamedName {
    #[inline]
    fn to_wack_ident(&self, _counter: &mut usize) -> WackIdent {
        WackIdent(self.ident.clone(), self.uuid)
    }
}

impl ConvertToWackIdent for ast::Ident {
    #[allow(clippy::arithmetic_side_effects)]
    #[inline]
    fn to_wack_ident(&self, counter: &mut usize) -> WackIdent {
        *counter += 1;
        WackIdent(self.clone(), *counter)
    }
}

impl From<WackIdent> for String {
    #[inline]
    fn from(mid_ident: WackIdent) -> Self {
        format!("{}.{}", mid_ident.0, mid_ident.1)
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

impl fmt::Debug for WackInstruction {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WackInstruction::Return(val) => write!(f, "Return({:?})", val),
            WackInstruction::ZeroExtend { src, dst } => {
                write!(f, "ZeroExtend {{ src: {:?}, dst: {:?} }}", src, dst)
            }
            WackInstruction::Unary { op, src, dst } => write!(
                f,
                "Unary {{ op: {:?}, src: {:?}, dst: {:?} }}",
                op, src, dst
            ),
            WackInstruction::Binary {
                op,
                src1,
                src2,
                dst,
            } => write!(
                f,
                "Binary {{ op: {:?}, src1: {:?}, src2: {:?}, dst: {:?} }}",
                op, src1, src2, dst
            ),
            WackInstruction::Copy { src, dst } => {
                write!(f, "Copy {{ src: {:?}, dst: {:?} }}", src, dst)
            }
            WackInstruction::Load { src_ptr, dst } => {
                write!(f, "Load {{ src_ptr: {:?}, dst: {:?} }}", src_ptr, dst)
            }
            WackInstruction::Store { src, dst_ptr } => {
                write!(f, "Store {{ src: {:?}, dst_ptr: {:?} }}", src, dst_ptr)
            }
            WackInstruction::AddPtr {
                ptr,
                index,
                scale,
                dst,
            } => write!(
                f,
                "AddPtr {{ ptr: {:?}, index: {:?}, scale: {:?}, dst: {:?} }}",
                ptr, index, scale, dst
            ),
            WackInstruction::CopyToOffset { src, dst, offset } => write!(
                f,
                "CopyToOffset {{ src: {:?}, dst: {:?}, offset: {:?} }}",
                src, dst, offset
            ),
            WackInstruction::CopyFromOffset { src, offset, dst } => write!(
                f,
                "CopyFromOffset {{ src: {:?}, offset: {:?}, dst: {:?} }}",
                src, offset, dst
            ),
            WackInstruction::Jump(target) => write!(f, "Jump({:?})", target),
            WackInstruction::JumpIfZero { condition, target } => write!(
                f,
                "JumpIfZero {{ condition: {:?}, target: {:?} }}",
                condition, target
            ),
            WackInstruction::JumpIfNotZero { condition, target } => write!(
                f,
                "JumpIfNotZero {{ condition: {:?}, target: {:?} }}",
                condition, target
            ),
            WackInstruction::Label(label) => write!(f, "Label({:?})", label),
            WackInstruction::FunCall {
                fun_name,
                args,
                dst,
            } => write!(
                f,
                "FunCall {{ fun_name: {:?}, args: {:?}, dst: {:?} }}",
                fun_name, args, dst
            ),
            WackInstruction::Read { dst, ty } => {
                write!(f, "Read {{ dst: {:?}, ty: {:?} }}", dst, ty)
            }
            WackInstruction::Free(val) => write!(f, "Free({:?})", val),
            WackInstruction::Exit(val) => write!(f, "Exit({:?})", val),
            WackInstruction::Print { src, ty } => {
                write!(f, "Print {{ src: {:?}, ty: {:?} }}", src, ty)
            }
            WackInstruction::Println { src, ty } => {
                write!(f, "Println {{ src: {:?}, ty: {:?} }}", src, ty)
            }
            _ => unimplemented!(),
        }
    }
}

/* ====================== TESTS * ====================== */
#[cfg(test)]
mod tests {
    use super::*;

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
            WackLiteral::Bool(1.into())
        );
        assert_eq!(
            WackLiteral::from(bool_liter_false),
            WackLiteral::Bool(0.into())
        );

        // Test char literal conversion
        let char_liter = ast::Liter::CharLiter('A');
        let char_liter2 = ast::Liter::CharLiter('a');
        assert_eq!(WackLiteral::from(char_liter), WackLiteral::Char(65)); // ASCII value of 'A'
        assert_eq!(WackLiteral::from(char_liter2), WackLiteral::Char(97)); // ASCII value of 'a'

        // Test pair literal conversion
        let pair_liter = ast::Liter::PairLiter;
        assert_eq!(WackLiteral::from(pair_liter), WackLiteral::NullPair);
    }
}
