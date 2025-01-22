use crate::shared::SharedString;
use std::slice;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Box<[Func]>,
    pub body: StatChain,
}

#[derive(Clone, Debug)] // additional constraint that every execution path through body of function must end with return or exit statement
pub struct Func {
    pub return_type: Type,
    pub name: Ident,
    pub params: Box<[FuncParam]>,
    pub body: StatChain,
}

#[derive(Clone, Debug)]
pub struct FuncParam {
    pub param_type: Type,
    pub name: Ident,
}

#[derive(Clone, Debug)]
pub struct StatChain(Box<[Stat]>);

#[derive(Error, Debug)]
#[error("Cannot convert to `StatChain` because the supplied `Vec<Stat> is empty")]
pub struct EmptyStatVecError;

impl StatChain {
    fn new(s: Stat) -> Self {
        Self(Box::new([s]))
    }

    pub(crate) fn try_new(stats: Vec<Stat>) -> Result<Self, EmptyStatVecError> {
        match stats.is_empty() {
            true => Err(EmptyStatVecError),
            false => Ok(Self(stats.into_boxed_slice())),
        }
    }

    pub fn first(&self) -> &Stat {
        &self.0[0]
    }

    pub fn last(&self) -> &Stat {
        &self.0[self.0.len() - 1]
    }

    pub fn iter(&self) -> slice::Iter<'_, Stat> {
        self.0.iter()
    }
}

impl From<Stat> for StatChain {
    fn from(s: Stat) -> Self {
        StatChain::new(s)
    }
}

impl TryFrom<Vec<Stat>> for StatChain {
    type Error = EmptyStatVecError;

    fn try_from(stats: Vec<Stat>) -> Result<Self, Self::Error> {
        StatChain::try_new(stats)
    }
}

#[derive(Clone, Debug)]
pub enum Stat {
    Skip,
    VarDefinition {
        var_type: Type,
        name: Ident,
        value: AssignRhs,
    },
    Assignment {
        lhs: AssignLhs,
        rhs: AssignRhs,
    },
    Read(AssignLhs),
    Free(Expr),
    Return(Expr),
    Exit(Expr),
    Print(Expr),
    Println(Expr),
    IfThenElse {
        if_cond: Expr,
        then_body: StatChain,
        else_body: StatChain,
    },
    WhileDo {
        while_cond: Expr,
        body: StatChain,
    },
    Scoped(StatChain),
}

#[derive(Clone, Debug)]
pub enum AssignLhs {
    Ident(Ident),
    ArrayElem(ArrayElem),
    PairElem(PairElem),
}

#[derive(Clone, Debug)]
pub enum AssignRhs {
    Expr(Expr),
    ArrayLiter(Box<[Expr]>),
    Newpair(Expr, Expr),
    PairElem(PairElem),
    Call { func_name: Ident, args: Box<[Expr]> },
}

#[derive(Clone, Debug)]
pub enum PairElem {
    Fst(Expr),
    Snd(Expr),
}

#[derive(Clone, Debug)]
pub enum Type {
    BaseType(BaseType),
    ArrayType(ArrayType),
    PairType(PairElemType, PairElemType),
}

#[derive(Clone, Debug)]
pub enum BaseType {
    Int,
    Bool,
    Char,
    String,
}

#[derive(Clone, Debug)]
pub struct ArrayType {
    pub elem_type: Box<Type>,
}

#[derive(Clone, Debug)]
pub enum PairElemType {
    BaseType(BaseType),
    ArrayType(ArrayType),
    Pair,
}

#[derive(Clone, Debug)]
pub enum Expr {
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(SharedString),
    PairLiter,
    Ident(Ident),
    ArrayElem(ArrayElem),
    Unary(UnaryOper, Box<Expr>),
    Binary(Box<Expr>, BinaryOper, Box<Expr>),
    Paren(Box<Expr>),

    // Generated only by parser errors.
    Error,
}

#[derive(Clone, Debug)]
pub enum UnaryOper {
    Not,
    Minus,
    Len,
    Ord,
    Chr,
}

#[derive(Clone, Debug)]
pub enum BinaryOper {
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

impl BinaryOper {
    /// The precedence of binary operators in WACC, where lower
    /// is higher. Source: WACC-language spec, Table 4.
    fn precedence(&self) -> u8 {
        match self {
            BinaryOper::Mul => 1,
            BinaryOper::Div => 1,
            BinaryOper::Mod => 1,
            BinaryOper::Add => 2,
            BinaryOper::Sub => 2,
            BinaryOper::Gt => 3,
            BinaryOper::Gte => 3,
            BinaryOper::Lt => 3,
            BinaryOper::Lte => 3,
            BinaryOper::Eq => 4,
            BinaryOper::Neq => 4,
            BinaryOper::And => 5,
            BinaryOper::Or => 6,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Ident(pub SharedString);

#[derive(Clone, Debug)]
pub struct ArrayElem {
    pub array_name: Ident,
    pub first_index: Box<Expr>,
    pub other_indices: Box<[Expr]>,
}

// TODO: comments are '#'-(any-character-except-EOL)* EOL
