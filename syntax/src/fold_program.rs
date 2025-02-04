#![allow(clippy::arbitrary_source_item_ordering)]
use crate::ast::{ArrayElem, PairElem};
use crate::ast::{Expr, Func, FuncParam, LValue, Program, RValue, Stat, StatBlock};
use crate::source::SourcedNode;

use crate::nonempty::NonemptyArray;

type SN<T> = SourcedNode<T>;

// Folder trait is used to transform our AST
// fold and folder is a rust compiler term
pub trait Folder {
    // Generic type parameters for the folder
    type N: Clone;
    type T;
    type Output;

    // We need the into_vec here so into_iter returns T and not &T
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

    fn fold_func_param(&mut self, param: FuncParam<Self::N>) -> FuncParam<Self::N> {
        FuncParam {
            r#type: param.r#type,
            name: self.fold_name(param.name),
        }
    }

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

    fn fold_stat(&mut self, stat: Stat<Self::N, Self::T>) -> Stat<Self::N, Self::T> {
        unimplemented!()
    }

    fn fold_lvalue(&mut self, lvalue: LValue<Self::N, Self::T>) -> LValue<Self::N, Self::T> {
        unimplemented!()
    }

    fn fold_rvalue(&mut self, rvalue: RValue<Self::N, Self::T>) -> RValue<Self::N, Self::T> {
        unimplemented!()
    }

    fn fold_expr(&mut self, expr: Expr<Self::N, Self::T>) -> Expr<Self::N, Self::T> {
        unimplemented!()
    }

    // Helper methods that implementations may want to override
    fn fold_name_inner(&mut self, name: Self::N) -> Self::N {
        name
    }

    fn fold_name(&mut self, name: SN<Self::N>) -> SN<Self::N> {
        SN::new(self.fold_name_inner(name.inner().clone()), name.span())
    }

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

    // fn fold_pair_elem(
    //     &mut self,
    //     elem: SN<PairElem<Self::N, Self::T>>,
    // ) -> SN<PairElem<Self::N, Self::T>> {
    //     elem.map_inner(|inner| match inner {
    //         PairElem::Fst(expr) => PairElem::Fst(self.fold_expr(expr)),
    //         PairElem::Snd(expr) => PairElem::Snd(self.fold_expr(expr)),
    //     })
    // }

    fn make_program(
        &mut self,
        funcs: Box<[Func<Self::N, Self::T>]>,
        body: SN<StatBlock<Self::N, Self::T>>,
    ) -> Self::Output;
}
