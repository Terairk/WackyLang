#![allow(clippy::arbitrary_source_item_ordering)]
use crate::ast::{ArrayElem, PairElem};
use crate::ast::{Expr, Func, FuncParam, LValue, Program, RValue, Stat, StatBlock};
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
    type Output;

    // Functions ending in _sn are for SN wrapped nodes in the AST
    // It just makes it easier to fold the AST and imo looks cleaner as
    // the ugly map_inner with nested closures is hidden from the user

    // We need the into_vec here so into_iter returns T and not &T
    #[inline]
    fn fold_program(&mut self, program: Program<Self::N, Self::T>) -> Self::Output {
        let folded_funcs = program.funcs.fold_with(|func| self.fold_func(func));

        let folded_body = self.fold_stat_block_sn(program.body);

        // Return type depends on individual implementation
        // Just make sure make_program returns Self::Output
        self.make_program(folded_funcs, folded_body)
    }

    #[inline]
    fn fold_func(&mut self, func: Func<Self::N, Self::T>) -> Func<Self::N, Self::T> {
        Func {
            return_type: func.return_type, // Type remains unchanged
            name: self.fold_name(func.name),
            params: func.params.fold_with(|param| self.fold_func_param(param)),
            body: self.fold_stat_block_sn(func.body),
        }
    }

    #[inline]
    fn fold_func_param(&mut self, param: FuncParam<Self::N>) -> FuncParam<Self::N> {
        FuncParam {
            r#type: param.r#type,
            name: self.fold_name(param.name),
        }
    }
    #[inline]
    fn fold_stat_block(
        &mut self,
        block: StatBlock<Self::N, Self::T>,
    ) -> StatBlock<Self::N, Self::T> {
        StatBlock(block.0.map_with(|stat| self.fold_stat_sn(stat)))
    }

    #[inline]
    fn fold_stat_block_sn(
        &mut self,
        block: SN<StatBlock<Self::N, Self::T>>,
    ) -> SN<StatBlock<Self::N, Self::T>> {
        block.map_inner(|inner| self.fold_stat_block(inner))
    }

    #[inline]
    fn fold_stat(&mut self, stat: Stat<Self::N, Self::T>) -> Stat<Self::N, Self::T> {
        match stat {
            Stat::Skip => stat,
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
            Stat::Return(expr) => Stat::Return(self.fold_expr_sn(expr)),
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
    fn fold_stat_sn(&mut self, stat: SN<Stat<Self::N, Self::T>>) -> SN<Stat<Self::N, Self::T>> {
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
    ) -> Stat<Self::N, Self::T> {
        Stat::VarDefinition {
            r#type,
            name: self.fold_name(name),
            rvalue: self.fold_rvalue(rvalue),
        }
    }

    #[inline]
    fn fold_lvalue(&mut self, lvalue: LValue<Self::N, Self::T>) -> LValue<Self::N, Self::T> {
        match lvalue {
            LValue::Ident(name) => LValue::Ident(self.fold_name(name)),
            LValue::ArrayElem(elem, ty) => LValue::ArrayElem(self.fold_array_elem(elem), ty),
            LValue::PairElem(elem, ty) => LValue::PairElem(self.fold_pair_elem_sn(elem), ty),
        }
    }

    #[inline]
    fn fold_lvalue_sn(
        &mut self,
        lvalue: SN<LValue<Self::N, Self::T>>,
    ) -> SN<LValue<Self::N, Self::T>> {
        lvalue.map_inner(|inner| self.fold_lvalue(inner))
    }

    #[inline]
    fn fold_rvalue(&mut self, rvalue: RValue<Self::N, Self::T>) -> RValue<Self::N, Self::T> {
        match rvalue {
            RValue::Expr(expr) => RValue::Expr(expr.map_inner(|inner| self.fold_expr(inner))),
            RValue::ArrayLiter(exprs, ty) => {
                RValue::ArrayLiter(exprs.fold_with(|expr| self.fold_expr_sn(expr)), ty)
            }
            RValue::NewPair(fst, snd, ty) => {
                RValue::NewPair(self.fold_expr_sn(fst), self.fold_expr_sn(snd), ty)
            }
            RValue::PairElem(pair_elem) => RValue::PairElem(self.fold_pair_elem(pair_elem)),
            RValue::Call {
                func_name,
                args,
                return_type,
            } => RValue::Call {
                func_name: self.fold_name(func_name),
                args: args.fold_with(|arg| self.fold_expr_sn(arg)),
                return_type,
            },
        }
    }

    #[inline]
    fn fold_value_sn(
        &mut self,
        value: SN<RValue<Self::N, Self::T>>,
    ) -> SN<RValue<Self::N, Self::T>> {
        value.map_inner(|inner| self.fold_rvalue(inner))
    }

    #[inline]
    fn fold_expr(&mut self, expr: Expr<Self::N, Self::T>) -> Expr<Self::N, Self::T> {
        match expr {
            Expr::Liter(lit, ty) => Expr::Liter(lit, ty),
            Expr::Ident(name, ty) => Expr::Ident(self.fold_name_inner(name), ty),
            Expr::ArrayElem(array_elem, ty) => {
                Expr::ArrayElem(self.fold_array_elem(array_elem), ty)
            }
            Expr::Unary(op, expr, ty) => Expr::Unary(op, self.fold_expr_sn(expr), ty),
            Expr::Binary(lhs, op, rhs, ty) => {
                Expr::Binary(self.fold_expr_sn(lhs), op, self.fold_expr_sn(rhs), ty)
            }
            Expr::Paren(expr, ty) => Expr::Paren(self.fold_expr_sn(expr), ty),
            Expr::Error(span) => Expr::Error(span),
        }
    }

    // Helper method to fold a expr that is already wrapped in an SN
    #[inline]
    fn fold_expr_sn(&mut self, expr: SN<Expr<Self::N, Self::T>>) -> SN<Expr<Self::N, Self::T>> {
        expr.map_inner(|inner| self.fold_expr(inner))
    }

    // Helper methods that implementations may want to override
    #[inline]
    fn fold_name_inner(&mut self, name: Self::N) -> Self::N {
        name
    }

    #[inline]
    fn fold_name(&mut self, name: SN<Self::N>) -> SN<Self::N> {
        name.map_inner(|inner| self.fold_name_inner(inner))
    }

    #[inline]
    fn fold_array_elem(
        &mut self,
        elem: ArrayElem<Self::N, Self::T>,
    ) -> ArrayElem<Self::N, Self::T> {
        ArrayElem {
            array_name: self.fold_name(elem.array_name),
            indices: { elem.indices.map_with(|index| self.fold_expr_sn(index)) },
        }
    }

    #[inline]
    fn fold_pair_elem(&mut self, elem: PairElem<Self::N, Self::T>) -> PairElem<Self::N, Self::T> {
        match elem {
            PairElem::Fst(expr) => PairElem::Fst(self.fold_lvalue_sn(expr)),
            PairElem::Snd(expr) => PairElem::Snd(self.fold_lvalue_sn(expr)),
        }
    }

    #[inline]
    fn fold_pair_elem_sn(
        &mut self,
        elem: SN<PairElem<Self::N, Self::T>>,
    ) -> SN<PairElem<Self::N, Self::T>> {
        elem.map_inner(|inner| self.fold_pair_elem(inner))
    }

    fn make_program(
        &mut self,
        funcs: Box<[Func<Self::N, Self::T>]>,
        body: SN<StatBlock<Self::N, Self::T>>,
    ) -> Self::Output;
}

// Helper methods for this file

// This trait is only used so we can call fold_with in a function call chain
trait BoxedSliceFold<T> {
    fn fold_with<F>(self, f: F) -> Box<[T]>
    where
        F: FnMut(T) -> T;
}

impl<T> BoxedSliceFold<T> for Box<[T]> {
    fn fold_with<F>(self, f: F) -> Self
    where
        F: FnMut(T) -> T,
    {
        self.into_vec()
            .into_iter()
            .map(f)
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}

trait NonEmptyFold<T> {
    fn map_with<F>(self, f: F) -> Self
    where
        F: FnMut(T) -> T;
}

impl<T> NonEmptyFold<T> for NonemptyArray<T> {
    fn map_with<F>(self, mut f: F) -> Self
    where
        F: FnMut(T) -> T,
    {
        let mut vec = self.into_boxed_slice();
        for item in &mut vec {
            // SAFETY: This operation is sound because:
            // 1. `item` is a valid pointer to an initialized T, obtained from an owned [T]
            // 2. We immediately consume and replace the read value, preventing double drops
            // 3. The ownership of T is maintained as we move it through f() and reassign
            // 4. No references to the value can exist as we have exclusive access to the Vec
            // 5. Even if f() panics, Vec's drop handler will clean up any remaining elements
            let old_value = unsafe { std::ptr::read(item) };
            let new_value = f(old_value);
            *item = new_value;
        }
        Self::try_from_boxed_slice(vec).expect("Map operation should preserve non-emptiness")
    }
}
