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

    // We need the into_vec here so into_iter returns T and not &T
    #[inline]
    fn fold_program(&mut self, program: Program<Self::N, Self::T>) -> Self::Output {
        let folded_funcs = program
            .funcs
            .into_vec()
            .into_iter()
            .map(|func| self.fold_func(func))
            .collect::<Vec<_>>()
            .into_boxed_slice();

        let folded_body = self.fold_stat_block(program.body);

        // Return type depends on individual implementation
        // Just make sure make_program returns Self::Output
        self.make_program(folded_funcs, folded_body)
    }

    #[inline]
    fn fold_func(&mut self, func: Func<Self::N, Self::T>) -> Func<Self::N, Self::T> {
        Func {
            return_type: func.return_type, // Type remains unchanged
            name: self.fold_name(func.name),
            params: func
                .params
                .into_vec()
                .into_iter()
                .map(|p| self.fold_func_param(p))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            body: self.fold_stat_block(func.body),
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
        block: SN<StatBlock<Self::N, Self::T>>,
    ) -> SN<StatBlock<Self::N, Self::T>> {
        // Fold each statement in the block first
        let original_span = block.span();
        let folded_statements: Vec<_> = block
            .into_inner()
            .0
            .into_boxed_slice()
            .into_vec()
            .into_iter()
            .map(|stat| stat.map_inner(|inner| self.fold_stat(inner)))
            .collect();

        // Since we know the original block was a NonemptyArray,
        // this conversion should never fail
        SN::new(
            StatBlock(
                NonemptyArray::try_from_boxed_slice(folded_statements.into_boxed_slice())
                    .expect("Statement block cannot be empty"),
            ),
            original_span,
        )
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
            Stat::Free(expr) => Stat::Free(expr.map_inner(|inner| self.fold_expr(inner))),
            Stat::Return(expr) => Stat::Return(expr.map_inner(|inner| self.fold_expr(inner))),
            Stat::Exit(expr) => Stat::Exit(expr.map_inner(|inner| self.fold_expr(inner))),
            Stat::Print(expr) => Stat::Print(expr.map_inner(|inner| self.fold_expr(inner))),
            Stat::Println(expr) => Stat::Println(expr.map_inner(|inner| self.fold_expr(inner))),
            Stat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => Stat::IfThenElse {
                if_cond: if_cond.map_inner(|inner| self.fold_expr(inner)),
                then_body: self.fold_stat_block(then_body),
                else_body: self.fold_stat_block(else_body),
            },
            Stat::WhileDo { while_cond, body } => Stat::WhileDo {
                while_cond: while_cond.map_inner(|inner| self.fold_expr(inner)),
                body: self.fold_stat_block(body),
            },
            Stat::Scoped(body) => Stat::Scoped(self.fold_stat_block(body)),
        }
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
            LValue::ArrayElem(elem, t) => LValue::ArrayElem(self.fold_array_elem(elem), t),
            LValue::PairElem(elem, t) => {
                let new_elem = elem.map_inner(|inner| self.fold_pair_elem(inner));
                LValue::PairElem(new_elem, t)
            }
        }
    }

    #[inline]
    fn fold_rvalue(&mut self, rvalue: RValue<Self::N, Self::T>) -> RValue<Self::N, Self::T> {
        match rvalue {
            RValue::Expr(expr) => RValue::Expr(expr.map_inner(|inner| self.fold_expr(inner))),
            RValue::ArrayLiter(exprs, t) => RValue::ArrayLiter(
                exprs
                    .into_vec()
                    .into_iter()
                    .map(|e| e.map_inner(|inner| self.fold_expr(inner)))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                t,
            ),
            RValue::NewPair(fst, snd, t) => RValue::NewPair(
                fst.map_inner(|inner| self.fold_expr(inner)),
                snd.map_inner(|inner| self.fold_expr(inner)),
                t,
            ),
            RValue::PairElem(pair_elem) => RValue::PairElem(self.fold_pair_elem(pair_elem)),
            RValue::Call {
                func_name,
                args,
                return_type,
            } => RValue::Call {
                func_name: self.fold_name(func_name),
                args: args
                    .into_vec()
                    .into_iter()
                    .map(|e| e.map_inner(|inner| self.fold_expr(inner)))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                return_type,
            },
        }
    }

    #[inline]
    fn fold_expr(&mut self, expr: Expr<Self::N, Self::T>) -> Expr<Self::N, Self::T> {
        match expr {
            Expr::Liter(lit, t) => Expr::Liter(lit, t),
            Expr::Ident(name, t) => Expr::Ident(self.fold_name_inner(name), t),
            Expr::ArrayElem(array_elem, t) => Expr::ArrayElem(self.fold_array_elem(array_elem), t),
            Expr::Unary(op, inner, t) => {
                Expr::Unary(op, inner.map_inner(|inner| self.fold_expr(inner)), t)
            }
            Expr::Binary(lhs, op, rhs, t) => Expr::Binary(
                lhs.map_inner(|inner| self.fold_expr(inner)),
                op,
                rhs.map_inner(|inner| self.fold_expr(inner)),
                t,
            ),
            Expr::Paren(inner, t) => Expr::Paren(inner.map_inner(|inner| self.fold_expr(inner)), t),
            Expr::Error(span) => Expr::Error(span),
        }
    }

    // Helper methods that implementations may want to override
    #[inline]
    fn fold_name_inner(&mut self, name: Self::N) -> Self::N {
        name
    }

    #[inline]
    fn fold_name(&mut self, name: SN<Self::N>) -> SN<Self::N> {
        SN::new(self.fold_name_inner(name.inner().clone()), name.span())
    }

    #[inline]
    fn fold_array_elem(
        &mut self,
        elem: ArrayElem<Self::N, Self::T>,
    ) -> ArrayElem<Self::N, Self::T> {
        ArrayElem {
            array_name: self.fold_name(elem.array_name),
            indices: {
                let folded_indices: Vec<_> = elem
                    .indices
                    .into_boxed_slice()
                    .into_vec()
                    .into_iter()
                    .map(|e| e.map_inner(|inner| self.fold_expr(inner)))
                    .collect();

                // Since we know the original indices was a NonemptyArray,
                // this conversion should never fail
                NonemptyArray::try_from_boxed_slice(folded_indices.into_boxed_slice())
                    .expect("Folding should preserve non-emptiness")
            },
        }
    }

    #[inline]
    fn fold_pair_elem(&mut self, elem: PairElem<Self::N, Self::T>) -> PairElem<Self::N, Self::T> {
        match elem {
            PairElem::Fst(expr) => {
                let original_span = expr.span();
                let folded_lvalue = self.fold_lvalue(expr.into_inner());
                let sn_wrapped = SN::new(folded_lvalue, original_span);
                PairElem::Fst(sn_wrapped)
            }
            PairElem::Snd(expr) => {
                let original_span = expr.span();
                let folded_lvalue = self.fold_lvalue(expr.into_inner());
                let sn_wrapped = SN::new(folded_lvalue, original_span);
                PairElem::Snd(sn_wrapped)
            }
        }
    }

    fn make_program(
        &mut self,
        funcs: Box<[Func<Self::N, Self::T>]>,
        body: SN<StatBlock<Self::N, Self::T>>,
    ) -> Self::Output;
}
