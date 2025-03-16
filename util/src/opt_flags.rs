use bon::Builder;

#[derive(Debug, Clone, Default, Copy, Builder)]
pub struct OptimizationConfig {
    /// Enable constant folding
    fold_constants: bool,

    /// Enable copy propagation
    copy_propagation: bool,

    /// Eliminate unreachable/dead code
    eliminate_unreachable_code: bool,

    /// Eliminate dead stores
    eliminate_dead_stores: bool,

    /// Perform register allocation
    reg_alloc: bool,

    /// Coalesce registers
    reg_coalesce: bool,

    /// Optimize tail recursion
    tailrec: bool,

    /// Print the CFG
    print_cfg: bool,
}

impl OptimizationConfig {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub const fn has_fold_constants(&self) -> bool {
        self.fold_constants
    }

    #[inline]
    pub const fn has_copy_propagation(&self) -> bool {
        self.copy_propagation
    }

    #[inline]
    pub const fn has_eliminate_unreachable_code(&self) -> bool {
        self.eliminate_unreachable_code
    }

    #[inline]
    pub const fn has_eliminate_dead_stores(&self) -> bool {
        self.eliminate_dead_stores
    }

    #[inline]
    pub const fn has_reg_alloc(&self) -> bool {
        self.reg_alloc
    }

    #[inline]
    pub const fn should_print_cfg(&self) -> bool {
        self.print_cfg
    }

    #[inline]
    pub const fn has_reg_coalesce(&self) -> bool {
        self.reg_coalesce
    }

    #[inline]
    pub const fn should_tailrec_optimize(&self) -> bool {
        self.tailrec
    }
}
