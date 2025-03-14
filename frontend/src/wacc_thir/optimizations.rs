use crate::wacc_thir::thir::{Stat, StatBlock};

/// Detects early "terminator" statements such as `return` or `exit` and
/// eliminates any subsequent unreachable code in the same block.
pub fn unreachable_code_elimination(stat_block: StatBlock) -> StatBlock {
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
            then_body: unreachable_code_elimination(then_body),
            else_body: unreachable_code_elimination(else_body),
        },
        Stat::LoopDo { label, body } => Stat::LoopDo {
            label,
            body: unreachable_code_elimination(body),
        },
        _ => s,
    }))
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
            unreachable_code_elimination(before_stat_block),
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
            unreachable_code_elimination(before_stat_block),
            after_stat_block
        );
    }
}
