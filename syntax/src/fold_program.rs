#![allow(clippy::arbitrary_source_item_ordering)]
use crate::ast::{ArrayElem, PairElem};
use crate::ast::{Expr, Func, FuncParam, Ident, LValue, Program, RValue, Stat, StatBlock};
use crate::source::SourcedNode;

use crate::nonempty::NonemptyArray;
use crate::types::Type;

type SN<T> = SourcedNode<T>;

// Folder trait is used to transform our AST
// fold and folder is a rust compiler term
pub trait Folder {
    // Generic type parameters for the folder
    type N: Clone;
    type T;
    type OutputN;
    type OutputT;

    // Functions ending in _sn are for SN wrapped nodes in the AST
    // It just makes it easier to fold the AST and imo looks cleaner as
    // the ugly map_inner with nested closures is hidden from the user

    /**************************************************************
     * SEE: bottom of this trait to see methods you must override *
     * TO USE: basically impl Folder and self.make_program        *
     * Put any state in the struct you use to implement Folder    *
     **************************************************************/

    // We need the into_vec here so into_iter returns T and not &T
    #[inline]
    fn fold_program(
        &mut self,
        program: Program<Self::N, Self::T>,
    ) -> Program<Self::OutputN, Self::OutputT> {
        let folded_funcs = program.funcs.fold_with(|func| self.fold_func(func));

        let folded_body = self.fold_stat_block_sn(program.body);

        // Return type depends on individual implementation
        // Just make sure make_program returns Self::Output
        self.make_program(folded_funcs, folded_body)
    }

    #[inline]
    fn fold_func(&mut self, func: Func<Self::N, Self::T>) -> Func<Self::OutputN, Self::OutputT> {
        Func {
            return_type: func.return_type, // Type remains unchanged
            name: func.name,
            params: func.params.fold_with(|param| self.fold_func_param(param)),
            body: self.fold_stat_block_sn(func.body),
        }
    }

    #[inline]
    fn fold_func_param(&mut self, param: FuncParam<Self::N>) -> FuncParam<Self::OutputN> {
        FuncParam {
            r#type: param.r#type,
            name: self.fold_name_sn(param.name),
        }
    }

    #[inline]
    fn fold_stat_block(
        &mut self,
        block: StatBlock<Self::N, Self::T>,
    ) -> StatBlock<Self::OutputN, Self::OutputT> {
        StatBlock(block.0.map_with(|stat| self.fold_stat_sn(stat)))
    }

    #[inline]
    fn fold_stat_block_sn(
        &mut self,
        block: SN<StatBlock<Self::N, Self::T>>,
    ) -> SN<StatBlock<Self::OutputN, Self::OutputT>> {
        block.map_inner(|inner| self.fold_stat_block(inner))
    }

    #[inline]
    fn fold_stat(&mut self, stat: Stat<Self::N, Self::T>) -> Stat<Self::OutputN, Self::OutputT> {
        match stat {
            Stat::Skip => Stat::Skip, // need this to avoid type inference error
            Stat::VarDefinition {
                r#type,
                name,
                rvalue,
            } => self.fold_var_definition(r#type, name, rvalue),
            Stat::Assignment { lvalue, rvalue } => Stat::Assignment {
                lvalue: self.fold_lvalue(lvalue),
                rvalue: self.fold_rvalue(rvalue),
            },
            Stat::Read(lvalue) => Stat::Read(self.fold_lvalue(lvalue)),
            Stat::Free(expr) => Stat::Free(self.fold_expr_sn(expr)),
            Stat::Return(expr) => self.fold_stat_return(expr),
            Stat::Exit(expr) => Stat::Exit(self.fold_expr_sn(expr)),
            Stat::Print(expr) => Stat::Print(self.fold_expr_sn(expr)),
            Stat::Println(expr) => Stat::Println(self.fold_expr_sn(expr)),
            Stat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => Stat::IfThenElse {
                if_cond: self.fold_expr_sn(if_cond),
                then_body: self.fold_stat_block_sn(then_body),
                else_body: self.fold_stat_block_sn(else_body),
            },
            Stat::WhileDo { while_cond, body } => Stat::WhileDo {
                while_cond: self.fold_expr_sn(while_cond),
                body: self.fold_stat_block_sn(body),
            },
            Stat::Scoped(body) => Stat::Scoped(self.fold_stat_block_sn(body)),
        }
    }

    // Helper method to fold a stat that is already wrapped in an SN
    #[inline]
    fn fold_stat_sn(
        &mut self,
        stat: SN<Stat<Self::N, Self::T>>,
    ) -> SN<Stat<Self::OutputN, Self::OutputT>> {
        stat.map_inner(|inner| self.fold_stat(inner))
    }

    // New method specifically for VarDefinition so we can do renaming
    // If other things need to be renamed, we may need to use a Decorator pattern
    #[inline]
    fn fold_var_definition(
        &mut self,
        r#type: SN<Type>,
        name: SN<Self::N>,
        rvalue: RValue<Self::N, Self::T>,
    ) -> Stat<Self::OutputN, Self::OutputT> {
        Stat::VarDefinition {
            r#type,
            name: self.fold_name_sn(name),
            rvalue: self.fold_rvalue(rvalue),
        }
    }

    #[inline]
    fn fold_stat_return(
        &mut self,
        expr: SN<Expr<Self::N, Self::T>>,
    ) -> Stat<Self::OutputN, Self::OutputT> {
        Stat::Return(self.fold_expr_sn(expr))
    }

    #[inline]
    fn fold_lvalue(
        &mut self,
        lvalue: LValue<Self::N, Self::T>,
    ) -> LValue<Self::OutputN, Self::OutputT> {
        match lvalue {
            LValue::Ident(name) => LValue::Ident(self.fold_name_sn(name)),
            LValue::ArrayElem(elem, ty) => {
                LValue::ArrayElem(self.fold_array_elem(elem), self.fold_type(ty))
            }
            LValue::PairElem(elem, ty) => {
                LValue::PairElem(self.fold_pair_elem_sn(elem), self.fold_type(ty))
            }
        }
    }

    #[inline]
    fn fold_lvalue_sn(
        &mut self,
        lvalue: SN<LValue<Self::N, Self::T>>,
    ) -> SN<LValue<Self::OutputN, Self::OutputT>> {
        lvalue.map_inner(|inner| self.fold_lvalue(inner))
    }

    #[inline]
    fn fold_rvalue(
        &mut self,
        rvalue: RValue<Self::N, Self::T>,
    ) -> RValue<Self::OutputN, Self::OutputT> {
        match rvalue {
            RValue::Expr(expr) => RValue::Expr(expr.map_inner(|inner| self.fold_expr(inner))),
            RValue::ArrayLiter(exprs, ty) => RValue::ArrayLiter(
                exprs.fold_with(|expr| self.fold_expr_sn(expr)),
                self.fold_type(ty),
            ),
            RValue::NewPair(fst, snd, ty) => RValue::NewPair(
                self.fold_expr_sn(fst),
                self.fold_expr_sn(snd),
                self.fold_type(ty),
            ),
            RValue::PairElem(pair_elem) => RValue::PairElem(self.fold_pair_elem(pair_elem)),
            RValue::Call {
                func_name,
                args,
                return_type,
            } => RValue::Call {
                func_name: self.fold_funcname_sn(func_name),
                args: args.fold_with(|arg| self.fold_expr_sn(arg)),
                return_type: self.fold_type(return_type),
            },
        }
    }

    #[inline]
    fn fold_value_sn(
        &mut self,
        value: SN<RValue<Self::N, Self::T>>,
    ) -> SN<RValue<Self::OutputN, Self::OutputT>> {
        value.map_inner(|inner| self.fold_rvalue(inner))
    }

    #[inline]
    fn fold_expr(&mut self, expr: Expr<Self::N, Self::T>) -> Expr<Self::OutputN, Self::OutputT> {
        match expr {
            Expr::Liter(lit, ty) => Expr::Liter(lit, self.fold_type(ty)),
            Expr::Ident(name, ty) => self.fold_expr_ident(name, ty),
            Expr::ArrayElem(array_elem, ty) => {
                Expr::ArrayElem(self.fold_array_elem(array_elem), self.fold_type(ty))
            }
            Expr::Unary(op, expr, ty) => {
                Expr::Unary(op, self.fold_expr_sn(expr), self.fold_type(ty))
            }
            Expr::Binary(lhs, op, rhs, ty) => Expr::Binary(
                self.fold_expr_sn(lhs),
                op,
                self.fold_expr_sn(rhs),
                self.fold_type(ty),
            ),
            Expr::Paren(expr, ty) => Expr::Paren(self.fold_expr_sn(expr), self.fold_type(ty)),
            Expr::Error(span) => Expr::Error(span),
        }
    }

    #[inline]
    fn fold_expr_ident(
        &mut self,
        ident: SN<Self::N>,
        r#type: Self::T,
    ) -> Expr<Self::OutputN, Self::OutputT> {
        Expr::Ident(self.fold_name_sn(ident), self.fold_type(r#type))
    }

    // Helper method to fold a expr that is already wrapped in an SN
    #[inline]
    fn fold_expr_sn(
        &mut self,
        expr: SN<Expr<Self::N, Self::T>>,
    ) -> SN<Expr<Self::OutputN, Self::OutputT>> {
        expr.map_inner(|inner| self.fold_expr(inner))
    }

    #[inline]
    fn fold_type_sn(&mut self, ty: SN<Self::T>) -> SN<Self::OutputT> {
        ty.map_inner(|inner| self.fold_type(inner))
    }

    #[inline]
    fn fold_array_elem(
        &mut self,
        elem: ArrayElem<Self::N, Self::T>,
    ) -> ArrayElem<Self::OutputN, Self::OutputT> {
        ArrayElem {
            array_name: self.fold_name_sn(elem.array_name),
            indices: { elem.indices.map_with(|index| self.fold_expr_sn(index)) },
        }
    }

    #[inline]
    fn fold_pair_elem(
        &mut self,
        elem: PairElem<Self::N, Self::T>,
    ) -> PairElem<Self::OutputN, Self::OutputT> {
        match elem {
            PairElem::Fst(expr) => PairElem::Fst(self.fold_lvalue_sn(expr)),
            PairElem::Snd(expr) => PairElem::Snd(self.fold_lvalue_sn(expr)),
        }
    }

    #[inline]
    fn fold_pair_elem_sn(
        &mut self,
        elem: SN<PairElem<Self::N, Self::T>>,
    ) -> SN<PairElem<Self::OutputN, Self::OutputT>> {
        elem.map_inner(|inner| self.fold_pair_elem(inner))
    }

    #[inline]
    fn make_program(
        &mut self,
        funcs: Box<[Func<Self::OutputN, Self::OutputT>]>,
        body: SN<StatBlock<Self::OutputN, Self::OutputT>>,
    ) -> Program<Self::OutputN, Self::OutputT> {
        Program { funcs, body }
    }

    /*****************************************
     * You must override these methods       *
     * Feel free to modify any of the above  *
     *****************************************/

    fn fold_name_sn(&mut self, name: SN<Self::N>) -> SN<Self::OutputN>;
    fn fold_funcname_sn(&mut self, name: SN<Ident>) -> SN<Ident>;

    fn fold_type(&mut self, ty: Self::T) -> Self::OutputT;
}

// Helper methods + traits for this file

// This trait is only used so we can call fold_with in a function call chain
pub trait BoxedSliceFold<T> {
    fn fold_with<F, U>(self, f: F) -> Box<[U]>
    where
        F: FnMut(T) -> U;
}

impl<T> BoxedSliceFold<T> for Box<[T]> {
    #[inline]
    fn fold_with<F, U>(self, f: F) -> Box<[U]>
    where
        F: FnMut(T) -> U,
    {
        self.into_vec()
            .into_iter()
            .map(f)
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}

pub trait NonEmptyFold<T> {
    fn map_with<F, U>(self, f: F) -> NonemptyArray<U>
    where
        F: FnMut(T) -> U;
}

impl<T> NonEmptyFold<T> for NonemptyArray<T> {
    #[inline]
    fn map_with<F, U>(self, mut f: F) -> NonemptyArray<U>
    where
        F: FnMut(T) -> U,
    {
        // we can maybe use the map_in_place crate
        let vec = Vec::from(self.into_boxed_slice());
        let mut result = Vec::with_capacity(vec.len());
        for item in vec {
            result.push(f(item));
        }

        NonemptyArray::try_from_boxed_slice(result.into_boxed_slice())
            .expect("Map operation should preserve non-emptiness")
    }
}
