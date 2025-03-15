use crate::types::{WackFuncType, WackType};
use crate::wackir::{WackGlobIdent, WackProgram, WackTempIdent};
use frontend::parsing::ast;
use frontend::wacc_thir::HirLoweringPhaseOutput;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
/* ================== PUBLIC API ================== */

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct WackIdentSymbolTable(pub HashMap<WackTempIdent, WackType>);

impl Deref for WackIdentSymbolTable {
    type Target = HashMap<WackTempIdent, WackType>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for WackIdentSymbolTable {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// Only take in type_resolver so i can discard the heck out of it
// I can probably take it in by reference but I'd prefer not to clone
#[must_use]
#[inline]
pub fn lower_program(
    hir_lowering: HirLoweringPhaseOutput,
) -> (WackProgram, usize, WackIdentSymbolTable) {
    // make context
    let program = hir_lowering.thir_program;
    let mut ctx = ThirLoweringCtx::new_from(
        hir_lowering.func_symbol_table,
        hir_lowering.hir_ident_symbol_table,
        hir_lowering.ident_counter,
    );

    // lower main body
    let mut main_body = Vec::new();
    ctx.lower_stat_block(program.body, &mut main_body);

    // lower functions
    let wack_functions = program
        .funcs
        .into_iter()
        .map(|func| ctx.lower_func(func))
        .collect();
    let wack_program = WackProgram {
        functions: wack_functions,
        main_body,
    };

    // return output
    (wack_program, ctx.ident_counter(), ctx.symbol_table)
}

/* ================== INTERNAL API ================== */

/// Used to keep track of loop-start and loop-end labels
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LoopRegion {
    pub start_label: WackTempIdent,
    pub end_label: WackTempIdent,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ThirLoweringCtx {
    ident_counter: usize,
    func_table: HashMap<WackGlobIdent, WackFuncType>,
    symbol_table: WackIdentSymbolTable,
    /// used to rename function to wacc_function
    func_rename_map: HashMap<ast::Ident, WackGlobIdent>,

    /// used to keep track of loop-start and loop-end labels
    current_loop_nesting_stack: Vec<LoopRegion>,
}

// impls relating to `ThirLoweringCtx`
pub(crate) mod thir_lowering_ctx {
    use crate::thir_transform::{LoopRegion, ThirLoweringCtx, WackIdentSymbolTable};
    use crate::types::{WackFuncType, WackPointerType, WackType};
    use crate::wackir::{
        BinaryOp, FALSE, TRUE, WackFunction, WackGlobIdent, WackInstr, WackLiteral, WackPrintType,
        WackReadType, WackTempIdent, WackValue,
    };
    use extend::ext;
    use frontend::parsing::ast;
    use frontend::wacc_hir::hir;
    use frontend::wacc_thir::lower_hir::{IdentSymbolTable, ThirFuncSymbolTable};
    use frontend::wacc_thir::thir;
    use std::collections::HashMap;

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct With<T, C> {
        ctx: C,
        inner: T,
    }

    impl<T, C> With<T, C> {
        #[inline]
        #[must_use]
        pub fn into_components(self) -> (T, C) {
            (self.inner, self.ctx)
        }

        #[inline]
        #[must_use]
        pub fn into_inner(self) -> T {
            self.inner
        }

        #[inline]
        #[must_use]
        pub fn into_ctx(self) -> C {
            self.ctx
        }
    }

    impl<T, C> With<T, &C> {
        #[inline]
        #[must_use]
        pub const fn ctx_ref(&self) -> &C {
            self.ctx
        }
    }

    impl<T, C> With<T, &mut C> {
        #[inline]
        #[must_use]
        pub fn ctx_mut(&mut self) -> &mut C {
            self.ctx
        }
    }

    #[ext(pub, name = WithAstLoweringCtxExt)]
    impl<T, C> T {
        #[inline]
        fn with_ctx(self, ctx: C) -> With<T, C> {
            With { ctx, inner: self }
        }

        #[inline]
        fn with_ctx_ref(self, ctx: &C) -> With<T, &C> {
            With { ctx, inner: self }
        }

        #[inline]
        fn with_ctx_mut(self, ctx: &mut C) -> With<T, &mut C> {
            With { ctx, inner: self }
        }
    }

    impl ThirLoweringCtx {
        pub(crate) fn new_from(
            func_symbol_table: ThirFuncSymbolTable,
            hir_ident_symbol_table: IdentSymbolTable,
            ident_counter: usize,
        ) -> Self {
            // create mapping to new function names, and new function table
            let mut func_rename_map: HashMap<ast::Ident, WackGlobIdent> = HashMap::new();
            let func_table = func_symbol_table
                .functions
                .into_iter()
                .map(|(id, (func, params))| {
                    // rename functions
                    let new_id: WackGlobIdent = ast::Ident::from_str(&format!("wacc_{id}")).into();
                    func_rename_map.insert(id, new_id.clone());

                    // crate new function table entry
                    (new_id, WackFuncType::from_thir_type(params, func))
                })
                .collect();

            // create new symbol table
            let symbol_table = WackIdentSymbolTable(
                hir_ident_symbol_table
                    .0
                    .into_iter()
                    .map(|(id, sym_ty)| (id.into(), WackType::from_thir_type(sym_ty)))
                    .collect(),
            );

            Self {
                ident_counter,
                func_table,
                symbol_table,
                func_rename_map,
                current_loop_nesting_stack: Vec::new(),
            }
        }

        pub(crate) const fn ident_counter(&self) -> usize {
            self.ident_counter
        }

        #[allow(clippy::arithmetic_side_effects)]
        pub(crate) const fn inc_ident_counter(&mut self) -> usize {
            self.ident_counter += 1;
            self.ident_counter
        }

        // Makes a temporary wacky variable, and adds it to the symbol table
        fn make_temporary(&mut self, wack_ty: WackType) -> WackTempIdent {
            // Eventually we may want to replace temp with a function name
            // for better debugging
            let ident: WackTempIdent = ast::Ident::from_str("temp").with_ctx_mut(self).into();

            // Add entry to symbol table
            self.symbol_table.insert(ident.clone(), wack_ty);

            // return
            ident
        }

        // Makes a label for jump's for now
        #[inline]
        fn make_label(&mut self, name: &str) -> WackTempIdent {
            let ident: WackTempIdent = ast::Ident::from_str(name).with_ctx_mut(self).into();
            ident
        }

        /// Create a new [`LoopRegion`] from a [`hir::LoopLabel`] for tracking start/end labels.
        #[inline]
        fn make_loop_region(&mut self, loop_label: hir::LoopLabel) -> LoopRegion {
            let start_label = loop_label.into();
            let end_label = self.make_label("loop_end");
            LoopRegion {
                start_label,
                end_label,
            }
        }

        /// Assuming a loop-label is already associated with a [`LoopRegion`] in stack,
        /// we can look up that [`LoopRegion`] using the loop-label.
        #[inline]
        fn lookup_loop_region_from_stack(&self, loop_label: hir::LoopLabel) -> Option<LoopRegion> {
            // convert loop label to wacky-ident
            let start_label: WackTempIdent = loop_label.into();

            // look for the loop-region corresponding to the loop-label in reverse (since its a stack)
            for region in self.current_loop_nesting_stack.iter().rev() {
                if start_label == region.start_label {
                    return Some(region.clone());
                }
            }
            None
        }

        pub(crate) fn lower_func(&mut self, func: thir::Func) -> WackFunction {
            let mut instructions: Vec<WackInstr> = Vec::new();
            self.lower_stat_block(func.body, &mut instructions);
            let params = func
                .params
                .into_iter()
                .map(|param| self.lower_func_param(&param))
                .collect();

            // Fetch the renamed name from function map
            let name = self
                .func_rename_map
                .get(&func.name)
                .expect("Function should be in map")
                .clone();

            WackFunction {
                name,
                params,
                body: instructions,
            }
        }

        /* TODO: find where we care about the types of params */
        #[inline]
        fn lower_func_param(&mut self, param: &thir::Ident) -> WackTempIdent {
            param.ident.clone().into()
        }

        pub(crate) fn lower_stat_block(
            &mut self,
            stat_block: thir::StatBlock,
            instructions: &mut Vec<WackInstr>,
        ) {
            for stat in stat_block.0 {
                self.lower_stat(stat, instructions);
            }
        }

        #[allow(clippy::too_many_lines, clippy::expect_used)]
        fn lower_stat(&mut self, stat: thir::Stat, instructions: &mut Vec<WackInstr>) {
            match stat {
                // TODO: add more type-checking code to lowerer SemTy vs. WackTy
                thir::Stat::Skip => (),
                thir::Stat::VarDefinition { name, rvalue, .. } => {
                    // TODO: add more type-checking code to lowerer SemTy vs. WackTy
                    // extract LHS
                    let lhs = name.ident.into();

                    // evaluate RHS, store into temp `rhs` variable
                    let (rhs, _rhs_ty) = self.lower_rvalue(rvalue, instructions);

                    // copy value at `rhs` to `lhs` identifier
                    let instr = WackInstr::Copy { src: rhs, dst: lhs };
                    instructions.push(instr);
                }
                thir::Stat::Assignment { lvalue, rvalue } => {
                    // TODO: add more type-checking code to lowerer SemTy vs. WackTy
                    let (lhs, _lhs_ty, derefed) = self.lower_lvalue(lvalue, instructions);
                    let (rhs, _rhs_ty) = self.lower_rvalue(rvalue, instructions);
                    let instr;
                    if derefed {
                        instr = WackInstr::CopyToOffset {
                            src: rhs,
                            dst_ptr: lhs,
                            offset: 0,
                        };
                    } else {
                        instr = WackInstr::Copy { src: rhs, dst: lhs };
                    }
                    instructions.push(instr);
                }
                thir::Stat::Read(lvalue) => {
                    // grab the semantic type, and use it to obtain read type
                    let sem_ty = lvalue.r#type();
                    let read_type = WackReadType::from_thir_type(sem_ty.clone())
                        .expect("This failing indicates frontend bug");

                    // get destination, and make sure types match up
                    let (value, _wack_ty, _) = self.lower_lvalue(lvalue, instructions);
                    // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                    //       so it is not strictly checking for equality -- as otherwise this will give false positives
                    // assert_eq!(WackType::from_semantic_type(sem_ty), wack_ty);

                    // perform read instruction
                    let instr = WackInstr::Read {
                        dst: value,
                        ty: read_type,
                    };
                    instructions.push(instr);
                }
                thir::Stat::Free(expr) => {
                    // get value and typecheck
                    let _sem_ty = expr.r#type();
                    let (value, wack_ptr_ty) = self.lower_expr(expr, instructions);
                    // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                    //       so it is not strictly checking for equality -- as otherwise this will give false positives
                    // assert_eq!(WackType::from_semantic_type(sem_ty), wack_ptr_ty);

                    // perform either checked or unchecked free based on type
                    let wack_derefed_ty = WackPointerType::try_from_wack_type(wack_ptr_ty.clone())
                        .expect("Only pointer-types can be freed")
                        .deref_type()
                        .expect("We can only free typed pointers");
                    let instr = match wack_derefed_ty {
                        WackType::Pair(_, _) => WackInstr::FreeChecked(value),
                        WackType::Array(_) => {
                            let tmp = self.make_temporary(wack_ptr_ty);
                            instructions.push(WackInstr::AddPtr {
                                src_ptr: value,
                                index: WackValue::Literal(WackLiteral::Int(0)),
                                scale: 1,
                                offset: -4,
                                dst_ptr: tmp.clone(),
                            });
                            WackInstr::FreeUnchecked(WackValue::Var(tmp))
                        }
                        _ => unreachable!(
                            "free value should be a pointer to pair or array, but found {:#?}",
                            wack_ptr_ty
                        ),
                    };
                    instructions.push(instr);
                }
                thir::Stat::Return(expr) => {
                    // get value and typecheck
                    let _sem_ty = expr.r#type();
                    let (value, _wack_ty) = self.lower_expr(expr, instructions);
                    // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                    //       so it is not strictly checking for equality -- as otherwise this will give false positives
                    // assert_eq!(WackType::from_semantic_type(sem_ty), wack_ty);

                    // push return instruction
                    let instr = WackInstr::Return(value);
                    instructions.push(instr);
                }
                thir::Stat::Exit(expr) => {
                    // get value and typecheck
                    let _sem_ty = expr.r#type();
                    let (value, _wack_ty) = self.lower_expr(expr, instructions);

                    // perform exit instr
                    let instr = WackInstr::Exit(value);
                    instructions.push(instr);
                }
                thir::Stat::Print(expr) => {
                    // grab the semantic type, and use it to obtain read type
                    let sem_ty = expr.r#type();
                    let print_type = WackPrintType::from_thir_type(sem_ty.clone())
                        .expect("This failing indicates frontend bug");

                    // get destination, and make sure types match up
                    let (value, _wack_ty) = self.lower_expr(expr, instructions);
                    // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                    //       so it is not strictly checking for equality -- as otherwise this will give false positives
                    // assert_eq!(WackType::from_semantic_type(sem_ty), wack_ty);

                    // get print instruction
                    let instr = WackInstr::Print {
                        src: value,
                        ty: print_type,
                    };
                    instructions.push(instr);
                }
                thir::Stat::Println(expr) => {
                    let sem_ty = expr.r#type();
                    let print_type = WackPrintType::from_thir_type(sem_ty.clone())
                        .expect("This failing indicates frontend bug");

                    // get destination, and make sure types match up
                    let (value, _wack_ty) = self.lower_expr(expr, instructions);
                    // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                    //       so it is not strictly checking for equality -- as otherwise this will give false positives
                    // assert_eq!(WackType::from_semantic_type(sem_ty), wack_ty);

                    // get print instruction
                    let instr = WackInstr::Println {
                        src: value,
                        ty: print_type,
                    };
                    instructions.push(instr);
                }
                thir::Stat::IfThenElse {
                    if_cond,
                    then_body,
                    else_body,
                } => {
                    // TODO: add more type-checking code to lowerer SemTy vs. WackTy

                    // Makes my life easier
                    use WackInstr as Instr;

                    let else_label = self.make_label("if_else");
                    let end_label = self.make_label("if_end");

                    // Evaluate the condition
                    let (condition, _cond_ty) = self.lower_expr(if_cond, instructions);

                    // Jump to true branch if condition is true
                    instructions.push(Instr::JumpIfZero {
                        condition,
                        target: else_label.clone(),
                    });

                    // Evaluate then branch
                    self.lower_stat_block(then_body, instructions);

                    // Jump to end of if
                    instructions.push(Instr::Jump(end_label.clone()));

                    // else branch
                    instructions.push(Instr::Label(else_label));
                    self.lower_stat_block(else_body, instructions);

                    // End of if
                    instructions.push(Instr::Label(end_label));
                }
                thir::Stat::LoopDo { label, body } => {
                    // create new loop-region to keep track of start/end labels,
                    // and create instruction corresponding to the start of the loop
                    let loop_region = self.make_loop_region(label);
                    instructions.push(WackInstr::Label(loop_region.start_label.clone()));

                    // push loop-region onto the loop-stack to update the
                    // context within which the loop-body will be lowered
                    self.current_loop_nesting_stack.push(loop_region.clone());
                    self.lower_stat_block(body, instructions);
                    // pop the loop-region from the stack and sanity-check that we got back what we pushed
                    assert_eq!(
                        &self
                            .current_loop_nesting_stack
                            .pop()
                            .expect("The stack should always pop the loop-region we just pushed"),
                        &loop_region,
                        "The stack should always pop the loop-region we just pushed"
                    );

                    // push jump-to-start instruction, and end-label, to complete the loop
                    instructions.push(WackInstr::Jump(loop_region.start_label));
                    instructions.push(WackInstr::Label(loop_region.end_label));
                }
                thir::Stat::Break(label) => {
                    // find loop-label, it should always be there
                    let loop_region = self.lookup_loop_region_from_stack(label).expect("Break-statement encountered outside a loop. This is a bug in the frontend!!!");

                    // jump to the end of the loop
                    instructions.push(WackInstr::Jump(loop_region.end_label));
                }
                thir::Stat::NextLoop(label) => {
                    // find loop-label, it should always be there
                    let loop_region = self.lookup_loop_region_from_stack(label).expect("Nextloop-statement encountered outside a loop. This is a bug in the frontend!!!");

                    // jump to the start of the loop
                    instructions.push(WackInstr::Jump(loop_region.start_label));
                }
            }
        }

        // TODO: check this return type later
        // TODO: i ignore types now but i doubt it'll be for long
        fn lower_expr(
            &mut self,
            expr: thir::Expr,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackValue, WackType) {
            match expr {
                thir::Expr::Liter(liter) => (
                    Self::lower_literal(liter.liter),
                    WackType::from_thir_type(liter.r#type),
                ),
                thir::Expr::Ident(ident) => {
                    let (ident, wack_ty) = self.lower_ident(ident);
                    (WackValue::Var(ident), wack_ty)
                }
                thir::Expr::ArrayElem(boxed) => {
                    // obtain pointer to the correct array element
                    let array_elem = *boxed;
                    let (array_elem_src_ptr, elem_ptr_ty) =
                        self.lower_array_elem_to_ptr(array_elem, instructions);
                    let elem_ty = elem_ptr_ty
                        .deref_type()
                        .expect("The pointer to array elements must be typed");

                    // dereference pointer to array-element, to extract its value to underlying
                    // runtime representation.
                    //
                    // NOTE: the value will always be a type representable by an integer:
                    //         - integers, booleans, characters, etc. can fit within integer registers
                    //         - arrays, strings, pairs, etc., are actually pointers hence fit within integer registers
                    //       therefore dereferencing is sufficient to obtain the underlying value
                    let dst_value = self.make_temporary(elem_ty.clone());
                    instructions.push(WackInstr::Load {
                        src_ptr: WackValue::Var(array_elem_src_ptr),
                        dst: dst_value.clone(),
                    });

                    // returned the obtained value
                    (WackValue::Var(dst_value), elem_ty)
                }
                thir::Expr::Unary(boxed) => {
                    let unary_expr = *boxed;
                    let (dst, dst_ty) = self.lower_unary(
                        unary_expr.expr.0,
                        unary_expr.expr.1,
                        unary_expr.r#type,
                        instructions,
                    );

                    (WackValue::Var(dst), dst_ty)
                }
                thir::Expr::Binary(boxed) => {
                    let binary_expr = *boxed;
                    let (dst, dst_ty) = self.lower_binary(
                        binary_expr.r#type,
                        binary_expr.expr.0,
                        binary_expr.expr.1,
                        binary_expr.expr.2,
                        instructions,
                    );
                    (WackValue::Var(dst), dst_ty)
                }
                thir::Expr::IfThenElse {
                    if_cond,
                    then_val,
                    else_val,
                    r#type,
                } => {
                    // TODO: add more type-checking code to lowerer SemTy vs. WackTy

                    // Makes my life easier
                    use WackInstr as Instr;

                    let else_label = self.make_label("if_else");
                    let end_label = self.make_label("if_end");

                    // Create destination target, and evaluate the condition
                    let dst_type = WackType::from_thir_type(r#type);
                    let dst_target = self.make_temporary(dst_type.clone());
                    let (condition, _cond_ty) = self.lower_expr(*if_cond, instructions);

                    // Jump to true branch if condition is true
                    instructions.push(Instr::JumpIfZero {
                        condition,
                        target: else_label.clone(),
                    });

                    // Evaluate then branch, and copy to target
                    let (then_val, _then_ty) = self.lower_expr(*then_val, instructions);
                    instructions.push(Instr::Copy {
                        src: then_val,
                        dst: dst_target.clone(),
                    });

                    // Jump to end of if
                    instructions.push(Instr::Jump(end_label.clone()));

                    // Evaluate else branch, and copy to target
                    instructions.push(Instr::Label(else_label));
                    let (else_val, _else_ty) = self.lower_expr(*else_val, instructions);
                    instructions.push(Instr::Copy {
                        src: else_val,
                        dst: dst_target.clone(),
                    });

                    // End of if, return destination target + it's type
                    instructions.push(Instr::Label(end_label));
                    (WackValue::Var(dst_target), dst_type)
                }
            }
        }

        fn lower_rvalue(
            &mut self,
            rvalue: thir::RValue,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackValue, WackType) {
            match rvalue {
                thir::RValue::Expr(expr) => self.lower_expr(expr, instructions),
                thir::RValue::ArrayLiter(elems) => {
                    // convert to Wack type, which should be a pointer type to raw array
                    let ptr_wack_ty =
                        WackPointerType::try_from_wack_type(WackType::from_thir_type(elems.r#type))
                            .expect("Array types should be represented by pointers to the raw array-values in memory");

                    let (array_liter_ptr, ptr_ty) = self.lower_array_liter_to_ptr(
                        elems.liter_values,
                        ptr_wack_ty,
                        instructions,
                    );
                    (WackValue::Var(array_liter_ptr), WackType::Pointer(ptr_ty))
                }
                thir::RValue::NewPair(newpair) => {
                    let (newpair_ptr, ptr_ty) = self.lower_newpair_to_ptr(
                        (newpair.fst, newpair.snd),
                        WackPointerType::try_from_wack_type(
                            WackType::from_thir_type(newpair.r#type))
                            .expect("Pair types should always be lowered to pointers to raw-pair values in memory"),
                        instructions,
                    );
                    (WackValue::Var(newpair_ptr), WackType::Pointer(ptr_ty))
                }
                thir::RValue::PairElem(pair_elem) => {
                    // obtain pointer to the correct pair-element
                    let (pair_elem_src_ptr, elem_ptr_ty) =
                        self.lower_pair_elem_to_ptr(pair_elem, instructions);
                    let elem_ty = elem_ptr_ty
                        .deref_type()
                        .expect("The pointer to pair elements must be typed");

                    // extract the first/second element of pair to underlying runtime representation
                    // by performing pointer dereferencing.
                    //
                    // NOTE: the value will always be a type representable by an integer:
                    //         - integers, booleans, characters, etc. can fit within integer registers
                    //         - arrays, strings, pairs, etc., are actually pointers hence fit within integer registers
                    //       therefore dereferencing is sufficient to obtain the underlying value
                    let dst_value = self.make_temporary(elem_ty.clone());
                    instructions.push(WackInstr::Load {
                        src_ptr: WackValue::Var(pair_elem_src_ptr),
                        dst: dst_value.clone(),
                    });

                    // returned the obtained value
                    (WackValue::Var(dst_value), elem_ty)
                }
                thir::RValue::Call {
                    func_name,
                    args,
                    return_type,
                } => {
                    // lower the arguments
                    let (wacky_args, _wacky_args_ty): (Vec<WackValue>, Vec<WackType>) = args
                        .into_iter()
                        .map(|arg| self.lower_expr(arg, instructions))
                        .unzip();

                    // Covert return type, and fetch the function identifier
                    let wack_ret_ty = WackType::from_thir_type(return_type);
                    let dst = self.make_temporary(wack_ret_ty.clone());
                    let wacky_func_name = self
                        .func_rename_map
                        .get(&func_name)
                        .expect("Function should be in map")
                        .clone();

                    // Push the call instruction
                    let instr = WackInstr::FunCall {
                        fun_name: wacky_func_name,
                        args: wacky_args,
                        dst: dst.clone(),
                    };
                    instructions.push(instr);
                    (WackValue::Var(dst), wack_ret_ty)
                }
            }
        }

        // the third parameter means whether this pointer should be automatically dereferenced when written
        fn lower_lvalue(
            &mut self,
            lvalue: thir::LValue,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType, bool) {
            match lvalue {
                thir::LValue::Ident(ident) => {
                    let (a, b) = self.lower_ident(ident);
                    (a, b, false)
                }
                thir::LValue::ArrayElem(array_elem) => {
                    // the lvalue evaluation only needs to return the pointer to the array element
                    let (ptr, ptr_ty) = self.lower_array_elem_to_ptr(array_elem, instructions);
                    (ptr, WackType::Pointer(ptr_ty), true)
                }
                thir::LValue::PairElem(pair_elem) => {
                    // the rvalue evaluation only needs to return the pointer to the pair element
                    let (ptr, ptr_ty) = self.lower_pair_elem_to_ptr(pair_elem, instructions);
                    (ptr, WackType::Pointer(ptr_ty), true)
                }
            }
        }

        #[allow(clippy::arithmetic_side_effects)]
        fn lower_array_liter_to_ptr(
            &mut self,
            elems: Box<[thir::Expr]>,
            array_ptr_ty: WackPointerType,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackPointerType) {
            // dereference the pointer type, and extract array_element type from it
            let raw_array_ty = array_ptr_ty
                .deref_type()
                .expect("The pointer should be typed, adn of raw-array-value type");
            // SAFETY: if there are no bugs in the frontend typechecking, this should never fail
            let elem_ty = unsafe { raw_array_ty.into_array_elem_type() };

            // compute the amount of memory this array literal needs:
            //   `alloc_size_bytes = array_len_bytes + (array_len * array_elem_bytes)`
            // the array is stored on the heap prefixed with its length, so it needs to allocate
            // memory to store that length-value as well.
            let array_len_bytes = WackType::WACC_ARRAY_LEN_WIDTH.byte_width() as usize;
            let array_len = elems.len();
            let array_elem_bytes = elem_ty.try_size_of().unwrap();
            let alloc_size_bytes = array_len_bytes + array_len * array_elem_bytes;

            //  allocate enough memory on the heap to store all elements and the size of the array
            let array_dst_ptr = self.make_temporary(WackType::Pointer(array_ptr_ty.clone()));
            instructions.push(WackInstr::Alloc {
                size: alloc_size_bytes,
                dst_ptr: array_dst_ptr.clone(),
            });
            let new_array_dst_ptr = self.make_temporary(WackType::Pointer(array_ptr_ty.clone()));
            instructions.push(WackInstr::add_ptr_offset(
                WackValue::Var(array_dst_ptr.clone()),
                4,
                new_array_dst_ptr.clone(),
            ));
            instructions.push(WackInstr::CopyToOffset {
                src: WackValue::Literal(WackLiteral::Int(array_len as i32)),
                dst_ptr: new_array_dst_ptr.clone(),
                offset: -4,
            });
            // one-by-one, evaluate each element of the array and then
            // store it to the corresponding slot in the array
            for (i, elem) in elems.into_iter().enumerate() {
                // it should never be the case that these types disagree
                // TODO: it may be the case that element types can be coerced safely to the
                //       overall array type, so this assert may trigger false-positives
                // assert_eq!(WackType::from_semantic_type(elem.get_type()), elem_ty);
                let (elem_value, _elem_value_ty) = self.lower_expr(elem, instructions);
                // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                //       so it is not strictly checking for equality -- as otherwise this will give false positives
                // assert_eq!(elem_ty, elem_value_ty);

                // compute offset into array, and copy element to that location
                let offset = (i * array_elem_bytes) as i32;
                instructions.push(WackInstr::CopyToOffset {
                    src: elem_value,
                    dst_ptr: new_array_dst_ptr.clone(),
                    offset,
                });
            }

            // return variable holding pointer to allocated array
            (new_array_dst_ptr, array_ptr_ty)
        }

        #[allow(dead_code)]
        fn assert_eq_with_pairtype_erasure(unerased: WackType, possibly_erased: WackType) {
            match (unerased.clone(), possibly_erased.clone()) {
                (
                    WackType::Pointer(WackPointerType::Any),
                    WackType::Pointer(WackPointerType::Any),
                ) => {}
                // a pointer is allowed to "cast" to untyped pointer if the target type is erased pair
                (
                    WackType::Pointer(WackPointerType::Of(inner)),
                    WackType::Pointer(WackPointerType::Any),
                ) => match *inner {
                    WackType::Pair(_, _) => {}
                    _ => assert_eq!(unerased, possibly_erased),
                },
                (_, _) => assert_eq!(unerased, possibly_erased),
            }
        }

        #[allow(clippy::arithmetic_side_effects)]
        fn lower_newpair_to_ptr(
            &mut self,
            elems: (thir::Expr, thir::Expr),
            newpair_ptr_ty: WackPointerType,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackPointerType) {
            // extract pair types
            let raw_newpair_ty = newpair_ptr_ty
                .deref_type()
                .expect("The pointer should be typed, adn of raw-pair-value type");
            // SAFETY: if there are no bugs in the frontend typechecking, this should never fail
            let (fst_elem_type, snd_elem_type) = unsafe { raw_newpair_ty.into_pair_elem_types() };

            // compute the amount of memory this pair literal needs:
            //   `alloc_size_bytes = fst_bytes + snd_bytes`
            // the pair is stored on the heap as two directly-adjacent regions of memory
            // which represent either the first or second elements, respectively.
            let fst_bytes = fst_elem_type.try_size_of().unwrap();
            let snd_bytes = snd_elem_type.try_size_of().unwrap();
            let alloc_size_bytes = fst_bytes + snd_bytes;

            //  allocate enough memory on the heap to store both elements and of the pair
            let pair_dst_ptr = self.make_temporary(WackType::Pointer(newpair_ptr_ty.clone()));
            instructions.push(WackInstr::Alloc {
                size: alloc_size_bytes,
                dst_ptr: pair_dst_ptr.clone(),
            });

            // it should never be the case that these types disagree
            // TODO: it may be the case that element types can be coerced safely to the
            //       overall pair type, so this assert may trigger false-positives
            let _fst_target_ty = WackType::from_thir_type(elems.0.r#type());
            let _snd_target_ty = WackType::from_thir_type(elems.1.r#type());
            // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
            //       so it is not strictly checking for equality -- as otherwise this will give false positives
            // Self::assert_eq_with_pairtype_erasure(fst_target_ty.clone(), fst_elem_type);
            // Self::assert_eq_with_pairtype_erasure(snd_target_ty.clone(), snd_elem_type);

            // evaluate expressions of both elements, ensure matching types
            let ((fst_value, _fst_ty), (snd_value, _snd_ty)) = (
                self.lower_expr(elems.0, instructions),
                self.lower_expr(elems.1, instructions),
            );
            // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
            //       so it is not strictly checking for equality -- as otherwise this will give false positives
            // assert_eq!(fst_target_ty, fst_ty);
            // assert_eq!(snd_target_ty, snd_ty);

            // insert each element to their corresponding slots in the allocated pair
            let mut offset = 0; // the first element has zero-offset from start of pair
            instructions.push(WackInstr::CopyToOffset {
                src: fst_value,
                dst_ptr: pair_dst_ptr.clone(),
                offset,
            });
            offset += fst_bytes as i32; // the second element follows directly after the first
            instructions.push(WackInstr::CopyToOffset {
                src: snd_value,
                dst_ptr: pair_dst_ptr.clone(),
                offset,
            });

            // return variable holding pointer to allocated pair
            (pair_dst_ptr, newpair_ptr_ty)
        }

        fn lower_pair_elem_to_ptr(
            &mut self,
            elem: thir::PairElem,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackPointerType) {
            // grab the inner value, and whether its Fst or Snd
            let (lvalue, is_fst) = (
                *elem.pair_elem.1,
                match elem.pair_elem.0 {
                    hir::PairElemSelector::Fst => true,
                    hir::PairElemSelector::Snd => false,
                },
            );
            // grab lvalue type, which contains the _full_ backwards-propagated type
            let lvalue_ty = lvalue.r#type();
            let pair_ptr_ty = WackType::from_thir_type(lvalue_ty);

            // the lvalue should evaluate to pointer of type pair
            let (mut pair_src_ptr, _, derefed) = self.lower_lvalue(lvalue, instructions);
            if derefed {
                let tmp_ident = self.make_temporary(pair_ptr_ty.clone());
                instructions.push(WackInstr::Load {
                    src_ptr: WackValue::Var(pair_src_ptr),
                    dst: tmp_ident.clone(),
                });
                pair_src_ptr = tmp_ident;
            }
            let pair_src_ptr = WackValue::Var(pair_src_ptr);
            let raw_pair_ty = WackPointerType::try_from_wack_type(pair_ptr_ty)
                .expect("Lowered value should be a pointer to raw pair-value")
                .deref_type()
                .expect("Lowered value should be a pointer to raw pair-value");
            // SAFETY: if there are no bugs in the frontend typechecking, this should never fail
            let wack_elems_ty = unsafe { raw_pair_ty.into_pair_elem_types() };
            let (elem_ty, offset) = match (is_fst, wack_elems_ty) {
                (true, (fst, _)) => (fst, 0), // the first element has zero-offset from start of pair
                (false, (fst, snd)) => (snd.clone(), fst.try_size_of().unwrap()), // the second element follows directly after the first
            }; // TODO: think about padding and alignment: this may not be the definitive layout

            // it should never be the case that these types disagree
            // TODO: it may be the case that element types can be coerced safely to the
            //       overall array type, so this assert may trigger false-positives
            // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
            //       so it is not strictly checking for equality -- as otherwise this will give false positives
            // Self::assert_eq_with_pairtype_erasure(
            //     WackType::from_semantic_type(elem_sem_type),
            //     elem_ty.clone(),
            // );

            // check that the obtained pointer isn't null pair literal,
            // and if not, obtain the pointer to the element value
            let pair_elem_ptr_ty = WackPointerType::of(elem_ty);
            let pair_elem_dst_ptr =
                self.make_temporary(WackType::Pointer(pair_elem_ptr_ty.clone()));
            instructions.push(WackInstr::NullPtrGuard(pair_src_ptr.clone()));
            instructions.push(WackInstr::add_ptr_offset(
                pair_src_ptr,
                offset as i32,
                pair_elem_dst_ptr.clone(),
            ));

            // return pointer to element, and type
            (pair_elem_dst_ptr, pair_elem_ptr_ty)
        }

        #[allow(clippy::expect_used, clippy::unwrap_used)]
        fn lower_array_elem_to_ptr(
            &mut self,
            array_elem: thir::ArrayElem,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackPointerType) {
            // array-access is potentially nested, and hence the first step is recursive
            let (src_array_ptr, index, elem_type) = match array_elem {
                // base case
                thir::ArrayElem::FirstAccess {
                    array_name,
                    index,
                    r#type,
                } => {
                    // the source array pointer is _initially_ the array-identifier
                    let (src_array_ptr, _array_ptr_ty) = self.lower_ident(array_name);
                    (src_array_ptr, index, r#type)
                }
                // recursive/nested case
                thir::ArrayElem::NestedAccess {
                    array_elem,
                    index,
                    r#type,
                } => {
                    // this is a nested access, so recursively access the array; what we get back is
                    // a POINTER to a WACC array, which is _itself_ a POINTER to the _actual_ raw array
                    let (elem_dst_ptr, elem_ptr_ty) =
                        self.lower_array_elem_to_ptr(*array_elem, instructions);

                    // so we need to dereference the DOUBLE-pointer ONCE to get access to the _actual_ pointer to array
                    let array_ptr_ty = elem_ptr_ty.deref_type().unwrap();
                    let src_array_ptr = self.make_temporary(array_ptr_ty);
                    instructions.push(WackInstr::Load {
                        src_ptr: WackValue::Var(elem_dst_ptr),
                        dst: src_array_ptr.clone(),
                    });

                    (src_array_ptr, index, r#type)
                }
            };

            // obtain index value, and the corresponding element type (for the scale)
            let (index_value, _index_ty) = self.lower_expr(index.clone(), instructions);
            // TODO: assert that type = int ??

            // convert array-element type, and obtain scale from it
            let array_elem_ty = WackType::from_thir_type(r#elem_type);
            let scale = array_elem_ty.try_size_of().unwrap();

            // create destination pointer-identifier, and its type - which is
            // a pointer of the array-element type
            let elem_ptr_ty = WackPointerType::of(array_elem_ty);
            let elem_dst_ptr = self.make_temporary(WackType::Pointer(elem_ptr_ty.clone()));

            // push array-access instruction
            instructions.push(WackInstr::ArrayAccess {
                src_array_ptr: WackValue::Var(src_array_ptr),
                index: index_value,
                scale,
                dst_elem_ptr: elem_dst_ptr.clone(),
            });

            // return identifier holding the pointer, and pointer-type
            (elem_dst_ptr, elem_ptr_ty)
        }

        // Very confusing but converts a syntax literal to Wacky Value
        // For now their definitions are basically the same
        fn lower_literal(liter: hir::Liter) -> WackValue {
            WackValue::Literal(liter.into())
        }

        fn lower_ident(&mut self, ident: thir::Ident) -> (WackTempIdent, WackType) {
            // fetch associated type
            let ident: WackTempIdent = ident.ident.into();
            let wack_ty = self
                .symbol_table
                .get(&ident)
                .expect(format!("identifier '{}' missing from symbol table", ident).as_str())
                .clone();

            // assert types match and return
            // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
            //       so it is not strictly checking for equality -- as otherwise this will give false positives
            // Self::assert_eq_with_pairtype_erasure(WackType::from_semantic_type(sem_type), wack_ty.clone());
            (ident, wack_ty)
        }

        // Very confusing but converts a syntax unary operand to Wacky Operator
        // For now their definitions are the same, but they may diverge in the future
        // TODO: check this later
        fn lower_unary(
            &mut self,
            unary_op: hir::UnaryOper,
            expr: thir::Expr,
            sem_type: thir::Type,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            // lower the inner expression

            if let hir::UnaryOper::Minus = unary_op {
                let (src, _src_ty) = self.lower_expr(expr, instr);
                let dst_ty = WackType::from_thir_type(sem_type);
                let dst_name = self.make_temporary(dst_ty.clone());
                instr.push(WackInstr::Binary {
                    op: BinaryOp::Sub,
                    src1: WackValue::Literal(WackLiteral::Int(0)),
                    src2: src,
                    dst: dst_name.clone(),
                });
                return (dst_name, dst_ty);
            }

            if let hir::UnaryOper::Len = unary_op {
                let (src, src_ty) = self.lower_expr(expr, instr);
                let dst_ty = WackType::from_thir_type(sem_type);
                let dst_name = self.make_temporary(dst_ty.clone());
                let new_src = self.make_temporary(src_ty.clone());
                instr.push(WackInstr::add_ptr_offset(src, -4, new_src.clone()));
                instr.push(WackInstr::Load {
                    src_ptr: WackValue::Var(new_src),
                    dst: dst_name.clone(),
                });
                // instr.push(WackInstr::Copy {
                //     src: WackValue::Var(src),
                //     dst: dst_name.clone(),
                // });
                return (dst_name, dst_ty);
            }
            let (src, _src_ty) = self.lower_expr(expr, instr);
            // TODO: do something with this type

            // make new identifier of target type
            let dst_ty = WackType::from_thir_type(sem_type);
            let dst_name = self.make_temporary(dst_ty.clone());

            // push instruction + return result
            instr.push(WackInstr::Unary {
                op: unary_op.into(),
                src,
                dst: dst_name.clone(),
            });

            (dst_name, dst_ty)
        }

        fn lower_binary(
            &mut self,
            sem_ty: thir::Type,
            expr1: thir::Expr,
            binop: hir::BinaryOper,
            expr2: thir::Expr,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            match binop {
                hir::BinaryOper::LAnd => self.lower_and_expr(sem_ty, expr1, expr2, instr),
                hir::BinaryOper::LOr => self.lower_or_expr(sem_ty, expr1, expr2, instr),
                _ => self.lower_normal_binary(sem_ty, expr1, binop, expr2, instr),
            }
        }

        fn lower_normal_binary(
            &mut self,
            sem_ty: thir::Type,
            expr1: thir::Expr,
            binop: hir::BinaryOper,
            expr2: thir::Expr,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            // We handle And/Or differently since we'll make them short circuit here
            let (src1, _src1_ty) = self.lower_expr(expr1, instr);
            let (src2, _src2_ty) = self.lower_expr(expr2, instr);
            // TODO: do something with these types??

            // make new identifier of target type
            let dst_ty = WackType::from_thir_type(sem_ty);
            let dst_name = self.make_temporary(dst_ty.clone());

            // push instruction + return result
            let wacky_op: BinaryOp = binop.into();
            instr.push(WackInstr::Binary {
                op: wacky_op,
                src1,
                src2,
                dst: dst_name.clone(),
            });
            (dst_name, dst_ty)
        }

        fn lower_or_expr(
            &mut self,
            sem_ty: thir::Type,
            expr1: thir::Expr,
            expr2: thir::Expr,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            // Makes my life easier
            use WackInstr as Instr;

            // Create labels for false branch and end of expr
            let true_label = self.make_label("or_true");
            let end_label = self.make_label("or_end");

            // Create a temporary variable to store the result of expression
            let dst_ty = WackType::from_thir_type(sem_ty);
            let dst = self.make_temporary(dst_ty.clone());

            // evaluate left, and short circuit conditionally
            let (left_v, _left_v_ty) = self.lower_expr(expr1, instr); // TODO: do something with these types??
            instr.push(Instr::JumpIfNotZero {
                condition: left_v,
                target: true_label.clone(),
            });

            // evaluate right, and short circuit conditionally
            let (right_v, _right_v_ty) = self.lower_expr(expr2, instr); // TODO: do something with these types??
            instr.push(Instr::JumpIfNotZero {
                condition: right_v,
                target: true_label.clone(),
            });

            // Both expressions evaluate to False so dst to False
            instr.push(Instr::Copy {
                src: WackValue::Literal(WackLiteral::Bool(FALSE)),
                dst: dst.clone(),
            });
            // Jump over the true branch
            instr.push(Instr::Jump(end_label.clone()));

            // True branch
            instr.push(Instr::Label(true_label));
            instr.push(Instr::Copy {
                src: WackValue::Literal(WackLiteral::Bool(TRUE)),
                dst: dst.clone(),
            });

            instr.push(Instr::Label(end_label));

            (dst, dst_ty)
        }

        fn lower_and_expr(
            &mut self,
            sem_ty: thir::Type,
            expr1: thir::Expr,
            expr2: thir::Expr,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            // Makes my life easier
            use WackInstr as Instr;

            // Create labels for false branch and end of expr
            let false_label = self.make_label("and_false");
            let end_label = self.make_label("and_end");

            // Create a temporary variable to store the result of expression
            let dst_ty = WackType::from_thir_type(sem_ty);
            let dst = self.make_temporary(dst_ty.clone());

            // evaluate left, and short circuit conditionally
            let (left_v, _left_v_ty) = self.lower_expr(expr1, instr); // TODO: do something with these types??
            instr.push(Instr::JumpIfZero {
                condition: left_v,
                target: false_label.clone(),
            });

            // evaluate right, and short circuit conditionally
            let (right_v, _right_v_ty) = self.lower_expr(expr2, instr); // TODO: do something with these types??
            instr.push(Instr::JumpIfZero {
                condition: right_v,
                target: false_label.clone(),
            });

            // Both expressions evaluate to True so dst to True
            instr.push(Instr::Copy {
                src: WackValue::Literal(WackLiteral::Bool(TRUE)),
                dst: dst.clone(),
            });

            // Jump over the false branch
            instr.push(Instr::Jump(end_label.clone()));

            instr.push(Instr::Label(false_label));
            instr.push(Instr::Copy {
                src: WackValue::Literal(WackLiteral::Bool(FALSE)),
                dst: dst.clone(),
            });
            instr.push(Instr::Label(end_label));

            (dst, dst_ty)
        }
    }
}
