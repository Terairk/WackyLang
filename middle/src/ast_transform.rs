use std::collections::HashMap;

use crate::types::{WackFuncType, WackType};
use crate::wackir::{WackGlobIdent, WackProgram, WackTempIdent};
use syntax::ast::{
    ArrayElem, Expr, Func, FuncParam, Ident, LValue, PairElem, Program, RValue, Stat, StatBlock,
};
use syntax::rename::RenamedName;
use syntax::typecheck::TypeResolver;
use syntax::types::SemanticType;
/* ================== PUBLIC API ================== */

type TypedAST = Program<RenamedName, SemanticType>;
type TypedFunc = Func<RenamedName, SemanticType>;
type TypedFuncParam = FuncParam<RenamedName>;
type TypedStatBlock = StatBlock<RenamedName, SemanticType>;
type TypedStat = Stat<RenamedName, SemanticType>;
type TypedLValue = LValue<RenamedName, SemanticType>;
type TypedRValue = RValue<RenamedName, SemanticType>;
type TypedExpr = Expr<RenamedName, SemanticType>;
type TypedArrayElem = ArrayElem<RenamedName, SemanticType>;
type TypedPairElem = PairElem<RenamedName, SemanticType>;

// Only take in type_resolver so i can discard the heck out of it
// I can probably take it in by reference but I'd prefer not to clone
#[must_use]
#[inline]
pub fn lower_program(
    program: TypedAST,
    type_resolver: TypeResolver,
) -> (WackProgram, usize, HashMap<WackTempIdent, WackType>) {
    let mut ctx = AstLoweringCtx::new_from(type_resolver);
    let mut main_body = Vec::new();
    ctx.lower_stat_block(program.body, &mut main_body);

    let wack_functions = program
        .funcs
        .into_iter()
        .map(|func| ctx.lower_func(func))
        .collect();

    let wack_program = WackProgram {
        functions: wack_functions,
        main_body,
    };

    (wack_program, ctx.ident_counter(), ctx.symbol_table)
}

/* ================== INTERNAL API ================== */
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct AstLoweringCtx {
    ident_counter: usize,
    func_table: HashMap<WackGlobIdent, WackFuncType>,
    symbol_table: HashMap<WackTempIdent, WackType>,
    /// used to rename function to wacc_function
    func_rename_map: HashMap<Ident, WackGlobIdent>,
}

// impls relating to `AstLoweringCtx`
pub(crate) mod ast_lowering_ctx {
    use crate::ast_transform::{
        AstLoweringCtx, TypedArrayElem, TypedExpr, TypedFunc, TypedFuncParam, TypedLValue,
        TypedPairElem, TypedRValue, TypedStat, TypedStatBlock,
    };
    use crate::types::{WackFuncType, WackPointerType, WackType};
    use crate::wackir::{
        BinaryOp, WackBool, WackFunction, WackGlobIdent, WackInstr, WackLiteral, WackPrintType,
        WackReadType, WackTempIdent, WackValue,
    };
    use extend::ext;
    use std::collections::HashMap;
    use syntax::ast;
    use syntax::ast::{BinaryOper, Ident, LValue, UnaryOper};
    use syntax::node::Node;
    use syntax::rename::RenamedName;
    use syntax::typecheck::TypeResolver;
    use syntax::types::{BaseType, SemanticType};

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

    impl AstLoweringCtx {
        pub(crate) fn new_from(type_resolver: TypeResolver) -> Self {
            // extract renamer + counter
            let renamer = type_resolver.renamer;
            let ident_counter = renamer.counter();

            // create mapping to new function names, and new function table
            let mut func_map: HashMap<Ident, WackGlobIdent> = HashMap::new();
            let func_table = renamer
                .id_func_table
                .functions
                .into_iter()
                .map(|(id, (func, params))| {
                    // rename functions
                    let new_id: WackGlobIdent = Ident::from_str(&format!("wacc_{id}")).into();
                    func_map.insert(id, new_id.clone());

                    // crate new function table entry
                    (new_id, WackFuncType::from_semantic_type(params, func))
                })
                .collect();

            // create new symbol table
            let symbol_table: HashMap<WackTempIdent, WackType> = type_resolver
                .symid_table
                .into_iter()
                .map(|(id, sym_ty)| (id.into(), WackType::from_semantic_type(sym_ty)))
                .collect();
            Self {
                ident_counter,
                func_table,
                symbol_table,
                func_rename_map: func_map,
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
            let ident: WackTempIdent = Ident::from_str("temp").with_ctx_mut(self).into();

            // Add entry to symbol table
            self.symbol_table.insert(ident.clone(), wack_ty);

            // return
            ident
        }

        // Makes a label for jump's for now
        fn make_label(&mut self, name: &str) -> WackTempIdent {
            let ident: WackTempIdent = Ident::from_str(name).with_ctx_mut(self).into();
            ident
        }

        pub(crate) fn lower_func(&mut self, func: TypedFunc) -> WackFunction {
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
        fn lower_func_param(&mut self, param: &TypedFuncParam) -> WackTempIdent {
            param.name.inner().clone().into()
        }

        pub(crate) fn lower_stat_block(
            &mut self,
            stat_block: TypedStatBlock,
            instructions: &mut Vec<WackInstr>,
        ) {
            for stat in stat_block.0 {
                self.lower_stat(stat.into_inner(), instructions);
            }
        }

        #[allow(clippy::too_many_lines)]
        fn lower_stat(&mut self, stat: TypedStat, instructions: &mut Vec<WackInstr>) {
            match stat {
                // TODO: add more type-checking code to lowerer SemTy vs. WackTy
                TypedStat::Skip => (),
                TypedStat::VarDefinition { name, rvalue, .. } => {
                    // TODO: add more type-checking code to lowerer SemTy vs. WackTy
                    // extract LHS
                    let lhs = name.into_inner().into();

                    // evaluate RHS, store into temp `rhs` variable
                    let (rhs, rhs_ty) = self.lower_rvalue(rvalue.into_inner(), instructions);

                    // copy value at `rhs` to `lhs` identifier
                    let instr = WackInstr::Copy { src: rhs, dst: lhs };
                    instructions.push(instr);
                }
                TypedStat::Assignment { lvalue, rvalue } => {
                    // TODO: add more type-checking code to lowerer SemTy vs. WackTy
                    let (lhs, lhs_ty, derefed) = self.lower_lvalue(lvalue.into_inner(), instructions);
                    let (rhs, rhs_ty) = self.lower_rvalue(rvalue.into_inner(), instructions);
                    let instr;
                    if derefed {
                        instr = WackInstr::CopyToOffset { src: rhs, dst_ptr: WackValue::Var(lhs), offset: 0 };
                    } else {
                        instr = WackInstr::Copy { src: rhs, dst: lhs };
                    }
                    instructions.push(instr);
                }
                TypedStat::Read(lvalue) => {
                    // grab the semantic type, and use it to obtain read type
                    let sem_ty = lvalue.inner().get_type();
                    let read_type = WackReadType::from_semantic_type(sem_ty.clone())
                        .expect("This failing indicates frontend bug");

                    // get destination, and make sure types match up
                    let (value, wack_ty, _) = self.lower_lvalue(lvalue.into_inner(), instructions);
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
                TypedStat::Free(expr) => {
                    // get value and typecheck
                    let sem_ty = expr.inner().get_type();
                    let (value, wack_ptr_ty) = self.lower_expr(expr.into_inner(), instructions);
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
                        WackType::Array(_) => WackInstr::FreeUnchecked(value),
                        _ => unreachable!(
                            "free value should be a pointer to pair or array, but found {:#?}",
                            wack_ptr_ty
                        ),
                    };
                    instructions.push(instr);
                }
                TypedStat::Return(expr) => {
                    // get value and typecheck
                    let sem_ty = expr.inner().get_type();
                    let (value, wack_ty) = self.lower_expr(expr.into_inner(), instructions);
                    // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                    //       so it is not strictly checking for equality -- as otherwise this will give false positives
                    // assert_eq!(WackType::from_semantic_type(sem_ty), wack_ty);

                    // push return instruction
                    let instr = WackInstr::Return(value);
                    instructions.push(instr);
                }
                TypedStat::Exit(expr) => {
                    // get value and typecheck
                    let sem_ty = expr.inner().get_type();
                    let (value, wack_ty) = self.lower_expr(expr.into_inner(), instructions);

                    // perform exit instr
                    let instr = WackInstr::Exit(value);
                    instructions.push(instr);
                }
                TypedStat::Print(expr) => {
                    // grab the semantic type, and use it to obtain read type
                    let sem_ty = expr.inner().get_type();
                    let print_type = WackPrintType::from_semantic_type(sem_ty.clone())
                        .expect("This failing indicates frontend bug");

                    // get destination, and make sure types match up
                    let (value, wack_ty) = self.lower_expr(expr.into_inner(), instructions);
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
                TypedStat::Println(expr) => {
                    let sem_ty = expr.inner().get_type();
                    let print_type = WackPrintType::from_semantic_type(sem_ty.clone())
                        .expect("This failing indicates frontend bug");

                    // get destination, and make sure types match up
                    let (value, wack_ty) = self.lower_expr(expr.into_inner(), instructions);
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
                TypedStat::IfThenElse {
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
                    let (condition, cond_ty) = self.lower_expr(if_cond.into_inner(), instructions);

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
                TypedStat::WhileDo { while_cond, body } => {
                    // TODO: add more type-checking code to lowerer SemTy vs. WackTy

                    // Makes my life easier
                    use WackInstr as Instr;
                    let start_label = self.make_label("while_start");
                    let end_label = self.make_label("while_end");

                    instructions.push(Instr::Label(start_label.clone()));
                    let (val, val_ty) = self.lower_expr(while_cond.into_inner(), instructions);
                    instructions.push(Instr::JumpIfZero {
                        condition: val,
                        target: end_label.clone(),
                    });
                    self.lower_stat_block(body, instructions);
                    instructions.push(Instr::Jump(start_label));
                    instructions.push(Instr::Label(end_label));
                }
                TypedStat::Scoped(stat_block) => self.lower_stat_block(stat_block, instructions),
            }
        }

        // TODO: check this return type later
        // TODO: i ignore types now but i doubt it'll be for long
        fn lower_expr(
            &mut self,
            expr: TypedExpr,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackValue, WackType) {
            match expr {
                TypedExpr::Liter(liter, sem_ty) => (
                    Self::lower_literal(liter),
                    WackType::from_semantic_type(sem_ty),
                ),
                TypedExpr::Ident(sn_ident, sem_ty) => {
                    let (ident, wack_ty) = self.lower_ident(sn_ident.into_inner(), sem_ty);
                    (WackValue::Var(ident), wack_ty)
                }
                TypedExpr::ArrayElem(array_elem, sem_ty) => {
                    // obtain pointer to the correct array element
                    let (array_elem_src_ptr, elem_ptr_ty) =
                        self.lower_array_elem_to_ptr(array_elem.into_inner(), sem_ty, instructions);
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
                TypedExpr::Unary(sn_unary, sn_expr, sem_ty) => {
                    let (dst, dst_ty) = self.lower_unary(
                        sn_unary.into_inner(),
                        sn_expr.into_inner(),
                        sem_ty,
                        instructions,
                    );

                    (WackValue::Var(dst), dst_ty)
                }
                TypedExpr::Binary(sn_expr1, sn_binop, sn_expr2, sem_ty) => {
                    let (dst, dst_ty) = self.lower_binary(
                        sem_ty,
                        sn_expr1.into_inner(),
                        sn_binop.into_inner(),
                        sn_expr2.into_inner(),
                        instructions,
                    );
                    (WackValue::Var(dst), dst_ty)
                }
                TypedExpr::Paren(sn_expr, _t) => {
                    self.lower_expr(sn_expr.into_inner(), instructions)
                }
                TypedExpr::Error(_) => unreachable!(
                    "Unless there is a bug somewhere in frontend, this should not be reachable."
                ),
            }
        }

        fn lower_rvalue(
            &mut self,
            rvalue: TypedRValue,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackValue, WackType) {
            match rvalue {
                TypedRValue::Expr(expr, _) => self.lower_expr(expr.into_inner(), instructions),
                TypedRValue::ArrayLiter(elems, sym_type) => {
                    // convert to Wack type, which should be a pointer type to raw array
                    let ptr_wack_ty =
                        WackPointerType::try_from_wack_type(WackType::from_semantic_type(sym_type))
                            .expect("Array types should be represented by pointers to the raw array-values in memory");

                    let (array_liter_ptr, ptr_ty) = self.lower_array_liter_to_ptr(
                        elems.into_iter().map(Node::into_inner).collect(),
                        ptr_wack_ty,
                        instructions,
                    );
                    (WackValue::Var(array_liter_ptr), WackType::Pointer(ptr_ty))
                }
                TypedRValue::NewPair(fst, snd, sym_type) => {
                    let (newpair_ptr, ptr_ty) = self.lower_newpair_to_ptr(
                        (fst.into_inner(), snd.into_inner()),
                        WackPointerType::try_from_wack_type(
                            WackType::from_semantic_type(sym_type))
                            .expect("Pair types should always be lowered to pointers to raw-pair values in memory"),
                        instructions,
                    );
                    (WackValue::Var(newpair_ptr), WackType::Pointer(ptr_ty))
                }
                TypedRValue::PairElem(elem, sem_type) => {
                    // obtain pointer to the correct pair-element
                    let (pair_elem_src_ptr, elem_ptr_ty) =
                        self.lower_pair_elem_to_ptr(elem.into_inner(), sem_type, instructions);
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
                TypedRValue::Call {
                    func_name,
                    args,
                    return_type,
                } => {
                    // lower the arguments
                    let (wacky_args, wacky_args_ty): (Vec<WackValue>, Vec<WackType>) = args
                        .into_iter()
                        .map(|arg| self.lower_expr(arg.into_inner(), instructions))
                        .unzip();

                    // Covert return type, and fetch the function identifier
                    let wack_ret_ty = WackType::from_semantic_type(return_type);
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
            lvalue: TypedLValue,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType, bool) {
            match lvalue {
                TypedLValue::Ident(sn_ident, sem_ty) => {
                    let (a, b) = self.lower_ident(sn_ident.into_inner(), sem_ty);

                    (a, b, false)
                }
                TypedLValue::ArrayElem(array_elem, t) => {
                    // the lvalue evaluation only needs to return the pointer to the array element
                    let (ptr, ptr_ty) =
                        self.lower_array_elem_to_ptr(array_elem.into_inner(), t, instructions);
                    (ptr, WackType::Pointer(ptr_ty), true)
                }
                TypedLValue::PairElem(elem, sem_type) => {
                    // the rvalue evaluation only needs to return the pointer to the pair element
                    let (ptr, ptr_ty) =
                        self.lower_pair_elem_to_ptr(elem.into_inner(), sem_type, instructions);
                    (ptr, WackType::Pointer(ptr_ty), true)
                }
            }
        }

        #[allow(clippy::arithmetic_side_effects)]
        fn lower_array_liter_to_ptr(
            &mut self,
            elems: Box<[TypedExpr]>,
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
            let array_len_bytes = BaseType::ARRAY_LEN_BYTES;
            let array_len = elems.len();
            let array_elem_bytes = elem_ty.try_size_of().unwrap();
            let alloc_size_bytes = array_len_bytes + array_len * array_elem_bytes;

            //  allocate enough memory on the heap to store all elements and the size of the array
            let array_dst_ptr = self.make_temporary(WackType::Pointer(array_ptr_ty.clone()));
            instructions.push(WackInstr::Alloc {
                size: alloc_size_bytes,
                dst_ptr: array_dst_ptr.clone(),
            });

            instructions.push(WackInstr::CopyToOffset {
                src: WackValue::Literal(WackLiteral::Int(array_len as i32)),
                dst_ptr: WackValue::Var(array_dst_ptr.clone()),
                offset: 0,
            });
            // one-by-one, evaluate each element of the array and then
            // store it to the corresponding slot in the array
            for (i, elem) in elems.into_iter().enumerate() {
                // it should never be the case that these types disagree
                // TODO: it may be the case that element types can be coerced safely to the
                //       overall array type, so this assert may trigger false-positives
                // assert_eq!(WackType::from_semantic_type(elem.get_type()), elem_ty);
                let (elem_value, elem_value_ty) = self.lower_expr(elem, instructions);
                // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                //       so it is not strictly checking for equality -- as otherwise this will give false positives
                // assert_eq!(elem_ty, elem_value_ty);

                // compute offset into array, and copy element to that location
                let offset = array_len_bytes + i * array_elem_bytes;
                instructions.push(WackInstr::CopyToOffset {
                    src: elem_value,
                    dst_ptr: WackValue::Var(array_dst_ptr.clone()),
                    offset,
                });
            }

            // return variable holding pointer to allocated array
            (array_dst_ptr, array_ptr_ty)
        }

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
            elems: (TypedExpr, TypedExpr),
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
            let fst_target_ty = WackType::from_semantic_type(elems.0.get_type());
            let snd_target_ty = WackType::from_semantic_type(elems.1.get_type());
            // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
            //       so it is not strictly checking for equality -- as otherwise this will give false positives
            // Self::assert_eq_with_pairtype_erasure(fst_target_ty.clone(), fst_elem_type);
            // Self::assert_eq_with_pairtype_erasure(snd_target_ty.clone(), snd_elem_type);

            // evaluate expressions of both elements, ensure matching types
            let ((fst_value, fst_ty), (snd_value, snd_ty)) = (
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
                dst_ptr: WackValue::Var(pair_dst_ptr.clone()),
                offset,
            });
            offset += fst_bytes; // the second element follows directly after the first
            instructions.push(WackInstr::CopyToOffset {
                src: snd_value,
                dst_ptr: WackValue::Var(pair_dst_ptr.clone()),
                offset,
            });

            // return variable holding pointer to allocated pair
            (pair_dst_ptr, newpair_ptr_ty)
        }

        fn lower_pair_elem_to_ptr(
            &mut self,
            elem: TypedPairElem,
            elem_sem_type: SemanticType,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackPointerType) {
            // grab the inner value, and whether its Fst or Snd
            let (lvalue, is_fst) = match elem {
                TypedPairElem::Fst(lvalue) => (lvalue, true),
                TypedPairElem::Snd(lvalue) => (lvalue, false),
            };
            // grab lvalue type, which contains the _full_ backwards-propagated type
            let lvalue_ty = match lvalue.clone().into_inner() {
                LValue::Ident(_, t) | LValue::ArrayElem(_, t) | LValue::PairElem(_, t) => t,
            };
            let pair_ptr_ty = WackType::from_semantic_type(lvalue_ty);

            // the lvalue should evaluate to pointer of type pair
            let (pair_src_ptr, _, _) = self.lower_lvalue(lvalue.into_inner(), instructions);
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
                offset,
                pair_elem_dst_ptr.clone(),
            ));

            // return pointer to element, and type
            (pair_elem_dst_ptr, pair_elem_ptr_ty)
        }

        #[allow(clippy::expect_used, clippy::unwrap_used)]
        fn lower_array_elem_to_ptr(
            &mut self,
            array_elem: TypedArrayElem,
            sem_type: SemanticType,
            instructions: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackPointerType) {
            // we are returning a POINTER to the target type, rather than the target type itself
            let target_type = WackType::pointer_of(WackType::from_semantic_type(sem_type));

            // get array name: the pointer to the beginning, and the element type
            let mut src_array_ptr: WackTempIdent = array_elem.array_name.into_inner().into();
            let mut array_ptr_ty = self
                .symbol_table
                .get(&src_array_ptr)
                .expect("This symbol should always be found, unless a previous stage has bugs.")
                .clone();

            // first element is guaranteed to be there
            let mut elem_ix_iter = array_elem.indices.into_iter();
            let mut index = elem_ix_iter.next().unwrap().into_inner();

            // track output element pointer
            let mut elem_ptr_ty: WackPointerType;
            let mut elem_dst_ptr: WackTempIdent;
            loop {
                // obtain index value, and the corresponding element type (for the scale)
                let (index_value, index_ty) = self.lower_expr(index.clone(), instructions);
                // TODO: assert that type = int ??

                // dereference pointer type to raw-array type, and extract element type from it
                let raw_array_ty = WackPointerType::try_from_wack_type(array_ptr_ty.clone())
                    .expect("Lowered value should be a pointer to raw-array value")
                    .deref_type()
                    .expect("Lowered value should be a pointer to raw-array value");
                // SAFETY: this is guaranteed to be an array-type, if not, previous stage has bugs.
                let array_elem_ty = unsafe { raw_array_ty.into_array_elem_type() };

                // obtain scale from the inner element type
                let scale = array_elem_ty.try_size_of().unwrap();

                // obtain pointer to element
                elem_ptr_ty = WackPointerType::of(array_elem_ty.clone());
                let elem_ptr_ty_wrapped = WackType::Pointer(elem_ptr_ty.clone());
                elem_dst_ptr = self.make_temporary(elem_ptr_ty_wrapped.clone());
                instructions.push(WackInstr::ArrayAccess {
                    src_array_ptr: WackValue::Var(src_array_ptr.clone()),
                    index: index_value,
                    scale,
                    dst_elem_ptr: elem_dst_ptr.clone(),
                });

                // if there is more indices, it means the element pointer points to an array;
                // dereference it to obtain another array, and set up for another loop;
                if let Some(next_index) = elem_ix_iter.next() {
                    // the element-type is a WACC array, i.e. in reality a pointer to an array,
                    // so update the "base" pointer-to array, to be that of the element type
                    array_ptr_ty = array_elem_ty;

                    // update `src_array_pointer` by dereferencing the `elem_dst_ptr`, and load
                    // the next index for iteration
                    src_array_ptr = self.make_temporary(array_ptr_ty.clone());
                    instructions.push(WackInstr::Load {
                        src_ptr: WackValue::Var(elem_dst_ptr.clone()),
                        dst: src_array_ptr.clone(),
                    });
                    index = next_index.into_inner();
                } else {
                    // if this is the last index, check that the end-type doesn't disagree with
                    // semantic type; if it does, there is a bug in the frontend
                    // TODO: it may be the case that element types can be coerced safely to the
                    //       overall array type, so this assert may trigger false-positives
                    // TODO: figure out a way to implement "weakening" check for lhs-rhs WACC types
                    //       so it is not strictly checking for equality -- as otherwise this will give false positives
                    // assert_eq!(elem_ptr_ty_wrapped, target_type);
                    break;
                }
            }

            // return variable holding pointer to element
            (elem_dst_ptr, elem_ptr_ty)
        }

        // Very confusing but converts a syntax literal to Wacky Value
        // For now their definitions are basically the same
        fn lower_literal(liter: ast::Liter) -> WackValue {
            WackValue::Literal(liter.into())
        }

        fn lower_ident(
            &mut self,
            ident: RenamedName,
            sem_type: SemanticType,
        ) -> (WackTempIdent, WackType) {
            // fetch associated type
            let ident: WackTempIdent = ident.into();
            let wack_ty = self
                .symbol_table
                .get(&ident)
                .expect("identifier missing from symbol table")
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
            unary_op: UnaryOper,
            expr: TypedExpr,
            sem_type: SemanticType,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            // lower the inner expression

            if let UnaryOper::Minus = unary_op {
                let (src, src_ty) = self.lower_expr(expr, instr);
                let dst_ty = WackType::from_semantic_type(sem_type);
                let dst_name = self.make_temporary(dst_ty.clone());
                instr.push(WackInstr::Binary {
                    op: BinaryOp::Sub,
                    src1: WackValue::Literal(WackLiteral::Int(0)),
                    src2: src,
                    dst: dst_name.clone(),
                });
                return (dst_name, dst_ty);
            }

            if let UnaryOper::Len = unary_op {
                let (src, src_ty) = self.lower_expr(expr, instr);
                let dst_ty = WackType::from_semantic_type(sem_type);
                let dst_name = self.make_temporary(dst_ty.clone());
                instr.push(WackInstr::Load {
                    src_ptr: src,
                    dst: dst_name.clone(),
                });
                // instr.push(WackInstr::Copy {
                //     src: WackValue::Var(src),
                //     dst: dst_name.clone(),
                // });
                return (dst_name, dst_ty);
            }
            let (src, src_ty) = self.lower_expr(expr, instr);
            // TODO: do something with this type

            // make new identifier of target type
            let dst_ty = WackType::from_semantic_type(sem_type);
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
            sem_ty: SemanticType,
            expr1: TypedExpr,
            binop: BinaryOper,
            expr2: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            match binop {
                BinaryOper::And => self.lower_and_expr(sem_ty, expr1, expr2, instr),
                BinaryOper::Or => self.lower_or_expr(sem_ty, expr1, expr2, instr),
                _ => self.lower_normal_binary(sem_ty, expr1, binop, expr2, instr),
            }
        }

        fn lower_normal_binary(
            &mut self,
            sem_ty: SemanticType,
            expr1: TypedExpr,
            binop: BinaryOper,
            expr2: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            // We handle And/Or differently since we'll make them short circuit here
            let (src1, src1_ty) = self.lower_expr(expr1, instr);
            let (src2, src2_ty) = self.lower_expr(expr2, instr);
            // TODO: do something with these types??

            // make new identifier of target type
            let dst_ty = WackType::from_semantic_type(sem_ty);
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
            sem_ty: SemanticType,
            expr1: TypedExpr,
            expr2: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            // Makes my life easier
            use WackInstr as Instr;

            // Create labels for false branch and end of expr
            let true_label = self.make_label("or_true");
            let end_label = self.make_label("or_end");

            // Create a temporary variable to store the result of expression
            let dst_ty = WackType::from_semantic_type(sem_ty);
            let dst = self.make_temporary(dst_ty.clone());

            // evaluate left, and short circuit conditionally
            let (left_v, left_v_ty) = self.lower_expr(expr1, instr); // TODO: do something with these types??
            instr.push(Instr::JumpIfNotZero {
                condition: left_v,
                target: true_label.clone(),
            });

            // evaluate right, and short circuit conditionally
            let (right_v, right_v_ty) = self.lower_expr(expr2, instr); // TODO: do something with these types??
            instr.push(Instr::JumpIfNotZero {
                condition: right_v,
                target: true_label.clone(),
            });

            // Both expressions evaluate to False so dst to False
            instr.push(Instr::Copy {
                src: WackValue::Literal(WackLiteral::Bool(WackBool::FALSE)),
                dst: dst.clone(),
            });
            // Jump over the true branch
            instr.push(Instr::Jump(end_label.clone()));

            // True branch
            instr.push(Instr::Label(true_label));
            instr.push(Instr::Copy {
                src: WackValue::Literal(WackLiteral::Bool(WackBool::TRUE)),
                dst: dst.clone(),
            });

            instr.push(Instr::Label(end_label));

            (dst, dst_ty)
        }

        fn lower_and_expr(
            &mut self,
            sem_ty: SemanticType,
            expr1: TypedExpr,
            expr2: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> (WackTempIdent, WackType) {
            // Makes my life easier
            use WackInstr as Instr;

            // Create labels for false branch and end of expr
            let false_label = self.make_label("and_false");
            let end_label = self.make_label("and_end");

            // Create a temporary variable to store the result of expression
            let dst_ty = WackType::from_semantic_type(sem_ty);
            let dst = self.make_temporary(dst_ty.clone());

            // evaluate left, and short circuit conditionally
            let (left_v, left_v_ty) = self.lower_expr(expr1, instr); // TODO: do something with these types??
            instr.push(Instr::JumpIfZero {
                condition: left_v,
                target: false_label.clone(),
            });

            // evaluate right, and short circuit conditionally
            let (right_v, right_v_ty) = self.lower_expr(expr2, instr); // TODO: do something with these types??
            instr.push(Instr::JumpIfZero {
                condition: right_v,
                target: false_label.clone(),
            });

            // Both expressions evaluate to True so dst to True
            instr.push(Instr::Copy {
                src: WackValue::Literal(WackLiteral::Bool(WackBool::TRUE)),
                dst: dst.clone(),
            });

            // Jump over the false branch
            instr.push(Instr::Jump(end_label.clone()));

            instr.push(Instr::Label(false_label));
            instr.push(Instr::Copy {
                src: WackValue::Literal(WackLiteral::Bool(WackBool::FALSE)),
                dst: dst.clone(),
            });
            instr.push(Instr::Label(end_label));

            (dst, dst_ty)
        }
    }
}
