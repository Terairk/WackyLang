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
use syntax::ast::{BinaryOper, Liter, UnaryOper};
use syntax::{ast::Ident, rename::RenamedName, types::SemanticType};

// Treat WackFunction's slightly differently from main
#[derive(Clone, Debug)]
pub struct WackProgram {
    pub top_level: Vec<WackFunction>,
    pub body: Vec<WackInstruction>,
}

#[derive(Clone, Debug)]
pub struct WackFunction {
    pub name: MidIdent,
    pub params: Vec<MidIdent>,
    // Not sure if we need types, should be fine
    // if we have Symbol Table
    pub body: Vec<WackInstruction>,
}

#[derive(Clone, Debug)]
pub enum WackInstruction {
    Return(WackValue),
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
        scale: i32,
        dst: MidIdent,
    },
    CopyToOffset {
        src: WackValue,
        dst: MidIdent,
        offset: i32,
    },
    CopyFromOffset {
        src: MidIdent,
        offset: i32,
        dst: WackValue,
    },
    Jump(MidIdent),
    JumpIfZero {
        condition: WackValue,
        target: MidIdent,
    },
    JumpIfNotZero {
        condition: WackValue,
        target: MidIdent,
    },
    Label(MidIdent),
    FunCall {
        fun_name: MidIdent,
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
    Constant(WackConst), // My only concern is the error type on SemanticType
    Var(MidIdent),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WackConst {
    // Represent all these as 32 bit integers
    Int(i32),
    Bool(i32),
    Char(i32),
    StringLit(ArcIntern<str>),
    NullPair,
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

impl From<BinaryOper> for BinaryOperator {
    #[inline]
    fn from(binop: BinaryOper) -> Self {
        match binop {
            BinaryOper::Mul => BinaryOperator::Mul,
            BinaryOper::Div => BinaryOperator::Div,
            BinaryOper::Mod => BinaryOperator::Mod,
            BinaryOper::Add => BinaryOperator::Add,
            BinaryOper::Sub => BinaryOperator::Sub,
            BinaryOper::Lte => BinaryOperator::Lte,
            BinaryOper::Lt => BinaryOperator::Lt,
            BinaryOper::Gte => BinaryOperator::Gte,
            BinaryOper::Gt => BinaryOperator::Gt,
            BinaryOper::Eq => BinaryOperator::Eq,
            BinaryOper::Neq => BinaryOperator::Neq,
            BinaryOper::And => BinaryOperator::And,
            BinaryOper::Or => BinaryOperator::Or,
        }
    }
}

impl From<UnaryOper> for UnaryOperator {
    #[inline]
    fn from(unop: UnaryOper) -> Self {
        match unop {
            UnaryOper::Not => UnaryOperator::Not,
            UnaryOper::Minus => UnaryOperator::Negate,
            UnaryOper::Len => UnaryOperator::Len,
            UnaryOper::Ord => UnaryOperator::Ord,
            UnaryOper::Chr => UnaryOperator::Chr,
        }
    }
}

// TODO: check that these give the right answers
impl From<Liter> for WackConst {
    #[inline]
    fn from(liter: Liter) -> Self {
        match liter {
            Liter::IntLiter(i) => Self::Int(i),
            Liter::BoolLiter(b) => Self::Bool(b as i32),
            Liter::CharLiter(c) => Self::Char(c as i32),
            Liter::StrLiter(s) => Self::StringLit(s),
            Liter::PairLiter => Self::NullPair,
        }
    }
}

#[derive(Clone)]
pub struct MidIdent(Ident, usize);

impl Debug for MidIdent {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.0, self.1)
    }
}

impl PartialEq for MidIdent {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}

// Invariant: The usize's are unique so should reuse the global counter when possible
// DO NOT UNDER ANY CIRCUMSTANCES USE THE SAME usize FOR TWO DIFFERENT IDENTIFIERS
impl Eq for MidIdent {}

impl Hash for MidIdent {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.1.hash(state);
    }
}

pub trait ConvertToMidIdent {
    fn to_mid_ident(&self, counter: &mut usize) -> MidIdent;
}

impl ConvertToMidIdent for RenamedName {
    #[inline]
    fn to_mid_ident(&self, _counter: &mut usize) -> MidIdent {
        MidIdent(self.ident.clone(), self.uuid)
    }
}

impl ConvertToMidIdent for Ident {
    #[allow(clippy::arithmetic_side_effects)]
    #[inline]
    fn to_mid_ident(&self, counter: &mut usize) -> MidIdent {
        *counter += 1;
        MidIdent(self.clone(), *counter)
    }
}

/* ====================== TESTS * ====================== */
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_liter_conversion() {
        // Test integer literal conversion
        let int_liter = Liter::IntLiter(42);
        assert_eq!(WackConst::from(int_liter), WackConst::Int(42));

        // Test boolean literal conversion
        let bool_liter_true = Liter::BoolLiter(true);
        let bool_liter_false = Liter::BoolLiter(false);
        assert_eq!(WackConst::from(bool_liter_true), WackConst::Bool(1));
        assert_eq!(WackConst::from(bool_liter_false), WackConst::Bool(0));

        // Test char literal conversion
        let char_liter = Liter::CharLiter('A');
        let char_liter2 = Liter::CharLiter('a');
        assert_eq!(WackConst::from(char_liter), WackConst::Char(65)); // ASCII value of 'A'
        assert_eq!(WackConst::from(char_liter2), WackConst::Char(97)); // ASCII value of 'a'

        // Test pair literal conversion
        let pair_liter = Liter::PairLiter;
        assert_eq!(WackConst::from(pair_liter), WackConst::NullPair);
    }
}
