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
}

impl OptimizationConfig {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn has_fold_constants(&self) -> bool {
        self.fold_constants
    }

    pub fn has_copy_propagation(&self) -> bool {
        self.copy_propagation
    }

    pub fn has_eliminate_unreachable_code(&self) -> bool {
        self.eliminate_unreachable_code
    }

    pub fn has_eliminate_dead_stores(&self) -> bool {
        self.eliminate_dead_stores
    }
}
