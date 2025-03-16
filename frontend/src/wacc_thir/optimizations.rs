use crate::parsing::ast;
use crate::wacc_hir::hir;
use crate::wacc_thir::thir::{
    Expr, Func, Ident, LValue, RValue, RenameIdent as _, Stat, StatBlock, Type,
};
use std::collections::HashMap;
use util::ext::VecExt as _;

const TAILREC_TEMP: &str = "tailrec_temp_variable";

/// Detects early "terminator" statements such as `return` or `exit` and
/// eliminates any subsequent unreachable code in the same block.
/// Performs basic unreachable code elimination that enliminates "unreachable blocks"
/// Needed to perform tail recursion optimization.
// More sophisticated unreachable code elimination is done in the WackyIR phase
pub fn unreachable_code_elimination(stat_block: StatBlock, should_optimise: bool) -> StatBlock {
    if !should_optimise {
        return stat_block;
    }
    // by default, the last statement should be a "terminator" statement
    let mut term_stat_ix = stat_block.0.len() - 1;

    // try to detect any premature "terminator" statements, and record the index
    for (i, s) in stat_block.0.iter().enumerate() {
        match *s {
            // all these can occur inside a statement-block, and all statements
            // in the same block after one of these is unreachable
            Stat::Return(_) | Stat::Exit(_) | Stat::Break(_) | Stat::NextLoop(_) => {
                term_stat_ix = i;
                break;
            }
            _ => {}
        }
    }

    // if a premature "terminator" statement was detected, then truncate the statement-body
    let stat_block = if term_stat_ix == stat_block.0.len() - 1 {
        stat_block
    } else {
        let mut stat_block = stat_block.0.to_vec();
        stat_block.truncate(term_stat_ix + 1);
        StatBlock::try_from(stat_block).expect("There should always be atleast one statement left")
    };

    // now perform the same transformation on the nested structures of body-statement
    StatBlock(stat_block.0.map(|s| match s {
        Stat::IfThenElse {
            if_cond,
            then_body,
            else_body,
        } => Stat::IfThenElse {
            if_cond,
            then_body: unreachable_code_elimination(then_body, should_optimise),
            else_body: unreachable_code_elimination(else_body, should_optimise),
        },
        Stat::LoopDo { label, body } => Stat::LoopDo {
            label,
            body: unreachable_code_elimination(body, should_optimise),
        },
        _ => s,
    }))
}

#[allow(clippy::too_many_lines)]
pub fn tail_recursion_optimization<I>(
    outer_tailrec_loop_label: hir::LoopLabel,
    mk_ident: &mut I,
    func: Func,
) -> Result<Func, Func>
where
    I: FnMut(&'static str, Type) -> Ident,
{
    // look for any call-statements that are self-recursive, if none found
    // then no tail-recursive optimization is applicable
    if !func.body.any(|s| s.has_call_to_func(&func.name)) {
        return Err(func);
    }

    // check that all recursive calls are tail-calls (i.e. the function is tail-recursive)
    if !func.body.all_calls_are_tail_calls(&func.name) {
        return Err(func);
    }

    // we now put the entire function body into a giant outer-loop label,
    // and replace all tail-recursive calls (and returns) with
    // 1) in-place function-parameter updates, and
    // 2) `nextloop` statements targeting this new outer-most loop
    let loop_label = outer_tailrec_loop_label;
    #[allow(clippy::items_after_statements, clippy::expect_used)]
    fn rewrite_tailrec_calls<I>(
        func_name: &ast::Ident,
        func_params: &[Ident],
        tailrec_loop_label: &hir::LoopLabel,
        mk_ident: &mut I,
        stat_block: StatBlock,
    ) -> StatBlock
    where
        I: FnMut(&'static str, Type) -> Ident,
    {
        // accumulate rewritten statements
        let mut accum = vec![];

        let mut stat_block_iter = stat_block.0.into_iter();
        while let Some(stat) = stat_block_iter.next() {
            // we are looking for variable-definition statements that call a function
            let Stat::VarDefinition {
                name,
                rvalue:
                    RValue::Call {
                        func_name: rvalue_func_name,
                        args,
                        ..
                    },
                ..
            } = stat.clone()
            else {
                accum.push(stat);
                continue;
            };

            // we are looking for that function name to match, so its tail-recursive
            if &rvalue_func_name != func_name {
                accum.push(stat);
                continue;
            }

            // the next item has to be there, and it has to be a return statement
            // (this was ensured by the previous checks performed)
            let Some(return_stat) = stat_block_iter.next() else {
                unreachable!()
            };
            let Stat::Return(return_val) = return_stat else {
                unreachable!()
            };
            // at this point, the return value should be precisely equal to the identifier just defined
            assert_eq!(return_val, Expr::Ident(name));

            // save old param values to temporaries, in order to avoid overriding
            let mut temp_map = HashMap::with_capacity(func_params.len());
            for param in func_params {
                let old_param = mk_ident(TAILREC_TEMP, param.r#type.clone());
                temp_map.insert(param.clone(), old_param.clone());
                accum.push(Stat::VarDefinition {
                    name: old_param,
                    rvalue: RValue::Expr(Expr::Ident(param.clone())),
                });
            }

            // now, push one in-place parameter-update assignment statement, per parameter of function
            for (param, new_value) in func_params.iter().zip(args.into_iter()) {
                let renamed_expr =
                    new_value.rename_ident(&mut |ident| temp_map.get(&ident).cloned());
                accum.push(Stat::Assignment {
                    lvalue: LValue::Ident(param.clone()),
                    rvalue: RValue::Expr(renamed_expr),
                });
            }

            // finally, push a "nextloop" statement which starts the next iteration of the
            // giant "tail-recursion" outer-loop, which simulates the return statement
            accum.push(Stat::NextLoop(tailrec_loop_label.clone()));
        }

        // now apply the same transformation recursively to any nested stat-blocks
        let mut recursive_transformation = |body| {
            rewrite_tailrec_calls(func_name, func_params, tailrec_loop_label, mk_ident, body)
        };
        let accum = accum.map(|s| match s {
            Stat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => Stat::IfThenElse {
                if_cond,
                then_body: recursive_transformation(then_body),
                else_body: recursive_transformation(else_body),
            },
            Stat::LoopDo { label, body } => Stat::LoopDo {
                label,
                body: recursive_transformation(body),
            },
            _ => s,
        });
        StatBlock::try_from(accum).expect("There should always be at least one statement left")
    }

    // finally, stick the resulting transformed block into an outer-loop and return the result
    let transformed =
        rewrite_tailrec_calls(&func.name, &func.params, &loop_label, mk_ident, func.body);
    let body = StatBlock::singleton(Stat::LoopDo {
        label: loop_label,
        body: transformed,
    });
    Ok(Func {
        return_type: func.return_type,
        name: func.name,
        params: func.params,
        body,
    })
}

#[cfg(test)]
pub mod tests {
    use crate::parsing::ast;
    use crate::wacc_hir::hir;
    use crate::wacc_thir::optimizations::unreachable_code_elimination;
    use crate::wacc_thir::thir::{Expr, Liter, Stat, StatBlock, Type};

    const TEST_LABEL: &str = "test_label";
    const TEST_LITER: Liter = Liter {
        liter: hir::Liter::PairLiter,
        r#type: Type::Any,
    };
    const TEST_EXPR: Expr = Expr::Liter(TEST_LITER);
    const TEST_STAT: Stat = Stat::Free(TEST_EXPR);

    #[inline]
    fn test_label() -> hir::LoopLabel {
        hir::LoopLabel::new_rogue_zero(ast::Ident::from_str(TEST_LABEL))
    }

    #[test]
    #[allow(clippy::unwrap_used)]
    pub fn unreachable_code_after_return_and_exit() {
        let before_stat_block = StatBlock::try_from(vec![
            TEST_STAT,
            TEST_STAT,
            Stat::IfThenElse {
                if_cond: TEST_EXPR,
                then_body: StatBlock::try_from(vec![
                    TEST_STAT,
                    TEST_STAT,
                    Stat::Exit(TEST_EXPR),
                    TEST_STAT,
                ])
                .unwrap(),
                else_body: StatBlock::try_from(vec![
                    TEST_STAT,
                    Stat::Return(TEST_EXPR),
                    TEST_STAT,
                    TEST_STAT,
                ])
                .unwrap(),
            },
            TEST_STAT,
            Stat::LoopDo {
                label: test_label(),
                body: StatBlock::try_from(vec![
                    TEST_STAT,
                    TEST_STAT,
                    TEST_STAT,
                    Stat::Exit(TEST_EXPR),
                    TEST_STAT,
                    TEST_STAT,
                    TEST_STAT,
                ])
                .unwrap(),
            },
            TEST_STAT,
            Stat::Return(TEST_EXPR),
            TEST_STAT,
            TEST_STAT,
        ])
        .unwrap();
        let after_stat_block = StatBlock::try_from(vec![
            TEST_STAT,
            TEST_STAT,
            Stat::IfThenElse {
                if_cond: TEST_EXPR,
                then_body: StatBlock::try_from(vec![TEST_STAT, TEST_STAT, Stat::Exit(TEST_EXPR)])
                    .unwrap(),
                else_body: StatBlock::try_from(vec![TEST_STAT, Stat::Return(TEST_EXPR)]).unwrap(),
            },
            TEST_STAT,
            Stat::LoopDo {
                label: hir::LoopLabel::new_rogue_zero(ast::Ident::from_str(TEST_LABEL)),
                body: StatBlock::try_from(vec![
                    TEST_STAT,
                    TEST_STAT,
                    TEST_STAT,
                    Stat::Exit(TEST_EXPR),
                ])
                .unwrap(),
            },
            TEST_STAT,
            Stat::Return(TEST_EXPR),
        ])
        .unwrap();
        assert_eq!(
            unreachable_code_elimination(before_stat_block, true),
            after_stat_block
        );
    }

    #[test]
    #[allow(clippy::unwrap_used)]
    pub fn unreachable_code_after_break_and_nextloop() {
        let before_stat_block = StatBlock::try_from(vec![
            TEST_STAT,
            TEST_STAT,
            Stat::IfThenElse {
                if_cond: TEST_EXPR,
                then_body: StatBlock::try_from(vec![
                    TEST_STAT,
                    TEST_STAT,
                    Stat::Break(test_label()),
                    TEST_STAT,
                ])
                .unwrap(),
                else_body: StatBlock::try_from(vec![
                    TEST_STAT,
                    Stat::NextLoop(test_label()),
                    TEST_STAT,
                    TEST_STAT,
                ])
                .unwrap(),
            },
            TEST_STAT,
            Stat::LoopDo {
                label: hir::LoopLabel::new_rogue_zero(ast::Ident::from_str(TEST_LABEL)),
                body: StatBlock::try_from(vec![
                    TEST_STAT,
                    TEST_STAT,
                    TEST_STAT,
                    Stat::Break(test_label()),
                    TEST_STAT,
                    TEST_STAT,
                    TEST_STAT,
                ])
                .unwrap(),
            },
            TEST_STAT,
            Stat::NextLoop(test_label()),
            TEST_STAT,
            TEST_STAT,
        ])
        .unwrap();
        let after_stat_block = StatBlock::try_from(vec![
            TEST_STAT,
            TEST_STAT,
            Stat::IfThenElse {
                if_cond: TEST_EXPR,
                then_body: StatBlock::try_from(vec![
                    TEST_STAT,
                    TEST_STAT,
                    Stat::Break(test_label()),
                ])
                .unwrap(),
                else_body: StatBlock::try_from(vec![TEST_STAT, Stat::NextLoop(test_label())])
                    .unwrap(),
            },
            TEST_STAT,
            Stat::LoopDo {
                label: hir::LoopLabel::new_rogue_zero(ast::Ident::from_str(TEST_LABEL)),
                body: StatBlock::try_from(vec![
                    TEST_STAT,
                    TEST_STAT,
                    TEST_STAT,
                    Stat::Break(test_label()),
                ])
                .unwrap(),
            },
            TEST_STAT,
            Stat::NextLoop(test_label()),
        ])
        .unwrap();
        assert_eq!(
            unreachable_code_elimination(before_stat_block, true),
            after_stat_block
        );
    }
}
