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
pub fn lower_program(program: TypedAST, type_resolver: TypeResolver) -> (WackProgram, usize) {
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

    (wack_program, ctx.ident_counter())
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
    use crate::types::{WackFuncType, WackType};
    use crate::wackir::{
        BinaryOp, UnaryOp, WackBool, WackFunction, WackGlobIdent, WackInstr, WackLiteral,
        WackTempIdent, WackValue,
    };
    use extend::ext;
    use std::collections::HashMap;
    use syntax::ast;
    use syntax::ast::{BinaryOper, Ident, UnaryOper};
    use syntax::node::Node;
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

    fn rename_function_ident(ident: &Ident) -> WackGlobIdent {
        Ident::from_str(&format!("wacc_{ident}")).into()
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
                    let new_id = rename_function_ident(&id);
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

        // Makes a temporary wacky variable
        fn make_temporary(&mut self) -> WackTempIdent {
            // Eventually we may want to replace temp with a function name
            // for better debugging
            let ident = Ident::from_str("temp");
            ident.with_ctx_mut(self).into()
        }

        // Makes a label for jump's for now
        fn make_label(&mut self, name: &str) -> WackTempIdent {
            let ident = Ident::from_str(name);
            ident.with_ctx_mut(self).into()
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

        fn lower_stat(&mut self, stat: TypedStat, instructions: &mut Vec<WackInstr>) {
            match stat {
                TypedStat::Skip => (),
                TypedStat::VarDefinition {
                    r#type,
                    name,
                    rvalue,
                } => {
                    // extract LHS
                    let lhs_type = r#type.into_inner();
                    let lhs = WackValue::Var(name.into_inner().into());

                    // evaluate RHS, store into temp `rhs` variable
                    let rhs = self.lower_rvalue(rvalue.into_inner(), instructions);

                    // copy value at `rhs` to `lhs` identifier
                    let instr = WackInstr::Copy { src: rhs, dst: lhs };
                    instructions.push(instr);
                }
                TypedStat::Assignment { lvalue, rvalue } => {
                    let lhs = self.lower_lvalue(lvalue.into_inner(), instructions);
                    let rhs = self.lower_rvalue(rvalue.into_inner(), instructions);
                    let instr = WackInstr::Copy { src: rhs, dst: lhs };
                    instructions.push(instr);
                }
                TypedStat::Read(lvalue) => {
                    let sem_type = lvalue.inner().get_type();
                    let value = self.lower_lvalue(lvalue.into_inner(), instructions);
                    let instr = WackInstr::Read {
                        dst: value,
                        ty: sem_type,
                    };
                    instructions.push(instr);
                }
                TypedStat::Free(expr) => {
                    let sem_type = expr.inner().get_type();
                    let value = self.lower_expr(expr.into_inner(), instructions);
                    let instr = match sem_type {
                        SemanticType::Pair(_, _) => WackInstr::FreeChecked(value),
                        SemanticType::Array(_) => WackInstr::FreeUnchecked(value),
                        _ => unreachable!("free value should be a pair or array"),
                    };
                    instructions.push(instr);
                }
                TypedStat::Return(expr) => {
                    let sem_type = expr.inner().get_type();
                    let value = self.lower_expr(expr.into_inner(), instructions);
                    let instr = WackInstr::Return(value);
                    instructions.push(instr);
                }
                TypedStat::Exit(expr) => {
                    let sem_type = expr.inner().get_type();
                    let value = self.lower_expr(expr.into_inner(), instructions);
                    let instr = WackInstr::Exit(value);
                    instructions.push(instr);
                }
                TypedStat::Print(expr) => {
                    let sem_type = expr.inner().get_type();
                    let value = self.lower_expr(expr.into_inner(), instructions);
                    let instr = WackInstr::Print {
                        src: value,
                        ty: sem_type,
                    };
                    instructions.push(instr);
                }
                TypedStat::Println(expr) => {
                    let sem_type = expr.inner().get_type();
                    let value = self.lower_expr(expr.into_inner(), instructions);
                    let instr = WackInstr::Println {
                        src: value,
                        ty: sem_type,
                    };
                    instructions.push(instr);
                }
                TypedStat::IfThenElse {
                    if_cond,
                    then_body,
                    else_body,
                } => {
                    // Makes my life easier
                    use WackInstr as Instr;

                    let else_label = self.make_label("if_else");
                    let end_label = self.make_label("if_end");

                    // Evaluate the condition
                    let condition = self.lower_expr(if_cond.into_inner(), instructions);

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
                    // Makes my life easier
                    use WackInstr as Instr;
                    let start_label = self.make_label("while_start");
                    let end_label = self.make_label("while_end");

                    instructions.push(Instr::Label(start_label.clone()));
                    let val = self.lower_expr(while_cond.into_inner(), instructions);
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
        fn lower_expr(&mut self, expr: TypedExpr, instructions: &mut Vec<WackInstr>) -> WackValue {
            match expr {
                TypedExpr::Liter(liter, _t) => Self::lower_literal(liter),
                TypedExpr::Ident(sn_ident, _t) => WackValue::Var(sn_ident.into_inner().into()),
                TypedExpr::ArrayElem(array_elem, t) => {
                    // obtain pointer to the correct array element
                    let array_elem_src_ptr =
                        self.lower_array_elem_to_ptr(array_elem.into_inner(), t, instructions);

                    // dereference pointer to array-element, to extract its value to underlying
                    // runtime representation.
                    //
                    // NOTE: the value will always be a type representable by an integer:
                    //         - integers, booleans, characters, etc. can fit within integer registers
                    //         - arrays, strings, pairs, etc., are actually pointers hence fit within integer registers
                    //       therefore dereferencing is sufficient to obtain the underlying value
                    let dst_value = WackValue::Var(self.make_temporary());
                    instructions.push(WackInstr::Load {
                        src_ptr: array_elem_src_ptr,
                        dst: dst_value.clone(),
                    });

                    // returned the obtained value
                    dst_value
                }
                TypedExpr::Unary(sn_unary, sn_expr, _t) => {
                    self.lower_unary(sn_unary.into_inner(), sn_expr.into_inner(), instructions)
                }
                TypedExpr::Binary(sn_expr1, sn_binop, sn_expr2, t) => self.lower_binary(
                    sn_expr1.into_inner(),
                    sn_binop.into_inner(),
                    sn_expr2.into_inner(),
                    instructions,
                ),
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
        ) -> WackValue {
            match rvalue {
                TypedRValue::Expr(expr, _) => self.lower_expr(expr.into_inner(), instructions),
                TypedRValue::ArrayLiter(elems, sym_type) => self.lower_array_liter_to_ptr(
                    elems.into_iter().map(Node::into_inner).collect(),
                    // SAFETY: by this point, type checking has concluded and the semantic type
                    //         HAS to be an array type, so this operation is safe.
                    unsafe { sym_type.into_array_elem_type() },
                    instructions,
                ),
                TypedRValue::NewPair(fst, snd, sym_type) => self.lower_newpair_to_ptr(
                    (fst.into_inner(), snd.into_inner()),
                    // SAFETY: by this point, type checking has concluded and the semantic type
                    //         HAS to be a pair-type, so this operation is safe.
                    unsafe { sym_type.into_pair_elem_types() },
                    instructions,
                ),
                TypedRValue::PairElem(elem, sem_type) => {
                    // obtain pointer to the correct pair element
                    let pair_elem_src_ptr =
                        self.lower_pair_elem_to_ptr(elem.into_inner(), sem_type, instructions);

                    // extract the first/second element of pair to underlying runtime representation
                    // by performing pointer dereferencing.
                    //
                    // NOTE: the value will always be a type representable by an integer:
                    //         - integers, booleans, characters, etc. can fit within integer registers
                    //         - arrays, strings, pairs, etc., are actually pointers hence fit within integer registers
                    //       therefore dereferencing is sufficient to obtain the underlying value
                    let dst_value = WackValue::Var(self.make_temporary());
                    instructions.push(WackInstr::Load {
                        src_ptr: pair_elem_src_ptr,
                        dst: dst_value.clone(),
                    });

                    // returned the obtained value
                    dst_value
                }
                // TODO: please add types to this
                TypedRValue::Call {
                    func_name,
                    args,
                    return_type,
                } => {
                    // lower the arguments
                    let wacky_args = args
                        .into_iter()
                        .map(|arg| self.lower_expr(arg.into_inner(), instructions))
                        .collect();

                    // Fetch the function identifier
                    let dst = WackValue::Var(self.make_temporary());
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
                    dst
                }
            }
        }

        fn lower_lvalue(
            &mut self,
            lvalue: TypedLValue,
            instructions: &mut Vec<WackInstr>,
        ) -> WackValue {
            match lvalue {
                TypedLValue::Ident(ident, _) => WackValue::Var(ident.into_inner().into()),
                TypedLValue::ArrayElem(array_elem, t) => {
                    // the lvalue evaluation only needs to return the pointer to the array element
                    self.lower_array_elem_to_ptr(array_elem.into_inner(), t, instructions)
                }
                TypedLValue::PairElem(elem, sem_type) => {
                    // the rvalue evaluation only needs to return the pointer to the pair element
                    self.lower_pair_elem_to_ptr(elem.into_inner(), sem_type, instructions)
                }
            }
        }

        #[allow(clippy::arithmetic_side_effects)]
        fn lower_array_liter_to_ptr(
            &mut self,
            elems: Box<[TypedExpr]>,
            elem_type: SemanticType,
            instructions: &mut Vec<WackInstr>,
        ) -> WackValue {
            // compute the amount of memory this array literal needs:
            //   `alloc_size_bytes = array_len_bytes + (array_len * array_elem_bytes)`
            // the array is stored on the heap prefixed with its length, so it needs to allocate
            // memory to store that length-value as well.
            let array_len_bytes = BaseType::ARRAY_LEN_BYTES;
            let array_len = elems.len();
            let array_elem_bytes = elem_type.size_of();
            let alloc_size_bytes = array_len_bytes + array_len * array_elem_bytes;

            //  allocate enough memory on the heap to store all elements and the size of the array
            let array_dst_ptr = self.make_temporary();
            let wrapped_array_value = WackValue::Var(array_dst_ptr.clone());
            instructions.push(WackInstr::Alloc {
                size: alloc_size_bytes,
                dst_ptr: wrapped_array_value.clone(),
            });

            // one-by-one, evaluate each element of the array and then
            // store it to the corresponding slot in the array
            for (i, elem) in elems.into_iter().enumerate() {
                // it should never be the case that these types disagree
                // TODO: it may be the case that element types can be coerced safely to the
                //       overall array type, so this assert may trigger false-positives
                assert_eq!(elem.get_type(), elem_type);
                let elem_value = self.lower_expr(elem, instructions);

                // compute offset into array, and copy element to that location
                let offset = array_len_bytes + i * array_elem_bytes;
                instructions.push(WackInstr::CopyToOffset {
                    src: elem_value,
                    dst: array_dst_ptr.clone(),
                    offset,
                });
            }

            // return variable holding pointer to allocated array
            wrapped_array_value
        }

        #[allow(clippy::arithmetic_side_effects)]
        fn lower_newpair_to_ptr(
            &mut self,
            elems: (TypedExpr, TypedExpr),
            elem_types: (SemanticType, SemanticType),
            instructions: &mut Vec<WackInstr>,
        ) -> WackValue {
            // compute the amount of memory this pair literal needs:
            //   `alloc_size_bytes = fst_bytes + snd_bytes`
            // the pair is stored on the heap as two directly-adjacent regions of memory
            // which represent either the first or second elements, respectively.
            let fst_bytes = elem_types.0.size_of();
            let snd_bytes = elem_types.1.size_of();
            let alloc_size_bytes = fst_bytes + snd_bytes;

            //  allocate enough memory on the heap to store both elements and of the pair
            let pair_dst_ptr = self.make_temporary();
            let wrapped_pair_value = WackValue::Var(pair_dst_ptr.clone());
            instructions.push(WackInstr::Alloc {
                size: alloc_size_bytes,
                dst_ptr: wrapped_pair_value.clone(),
            });

            // it should never be the case that these types disagree
            // TODO: it may be the case that element types can be coerced safely to the
            //       overall pair type, so this assert may trigger false-positives
            assert_eq!(elems.0.get_type(), elem_types.0);
            assert_eq!(elems.1.get_type(), elem_types.1);

            // evaluate expressions of both elements
            let elem_values = (
                self.lower_expr(elems.0, instructions),
                self.lower_expr(elems.1, instructions),
            );

            // insert each element to their corresponding slots in the allocated pair
            let mut offset = 0; // the first element has zero-offset from start of pair
            instructions.push(WackInstr::CopyToOffset {
                src: elem_values.0,
                dst: pair_dst_ptr.clone(),
                offset,
            });
            offset += fst_bytes; // the second element follows directly after the first
            instructions.push(WackInstr::CopyToOffset {
                src: elem_values.1,
                dst: pair_dst_ptr,
                offset,
            });

            // return variable holding pointer to allocated pair
            wrapped_pair_value
        }

        fn lower_pair_elem_to_ptr(
            &mut self,
            elem: TypedPairElem,
            elem_sem_type: SemanticType,
            instructions: &mut Vec<WackInstr>,
        ) -> WackValue {
            // match on elem to obtain:
            //   1: the value which reduces to a pair-type pointer,
            //   2: the type that the pair-element represents
            //   3: the offset into the pair at which the pair-element value can be found
            // TODO: make sure I am using `SemanticType`s correctly, and not misunderstanding anything
            let (operand_value, operand_element_type, offset) = match elem {
                TypedPairElem::Fst(lvalue) => {
                    let lvalue = lvalue.into_inner();

                    // SAFETY: by this point, type checking has concluded and the semantic type
                    //         of the operand to pair extractions HAS to be a pair-type,
                    //         so this operation is safe.
                    let (fst_type, _) = unsafe { lvalue.get_type().into_pair_elem_types() };
                    let offset = 0; // the first element has zero-offset from start of pair
                    (lvalue, fst_type, offset)
                }
                TypedPairElem::Snd(lvalue) => {
                    let lvalue = lvalue.into_inner();

                    // SAFETY: by this point, type checking has concluded and the semantic type
                    //         of the operand to pair extractions HAS to be a pair-type,
                    //         so this operation is safe.
                    let (fst_type, snd_type) = unsafe { lvalue.get_type().into_pair_elem_types() };
                    let offset = fst_type.size_of(); // the second element follows directly after the first
                    (lvalue, snd_type, offset)
                }
            };

            // it should never be the case that these types disagree
            // TODO: it may be the case that element types can be coerced safely to the
            //       overall pair type, so this assert may trigger false-positives
            assert_eq!(operand_element_type, elem_sem_type);
            let pair_src_ptr = self.lower_lvalue(operand_value, instructions);

            // check that the obtained pointer isn't null pair literal,
            // and if not, obtain the pointer to the element value
            let pair_elem_dst_ptr = self.make_temporary();
            instructions.push(WackInstr::NullPtrGuard(pair_src_ptr.clone()));
            instructions.push(WackInstr::add_ptr_offset(
                pair_src_ptr,
                offset,
                pair_elem_dst_ptr.clone(),
            ));

            WackValue::Var(pair_elem_dst_ptr)
        }

        #[allow(clippy::expect_used, clippy::unwrap_used)]
        fn lower_array_elem_to_ptr(
            &mut self,
            array_elem: TypedArrayElem,
            sem_type: SemanticType,
            instructions: &mut Vec<WackInstr>,
        ) -> WackValue {
            // get array name: the pointer to the beginning, and the element type
            let mut src_array_ptr: WackTempIdent = array_elem.array_name.into_inner().into();
            let mut array_type = self
                .symbol_table
                .get(&src_array_ptr)
                .expect("This symbol should always be found, unless a previous stage has bugs.")
                .clone();

            // first element is guaranteed to be there
            let mut elem_ix_iter = array_elem.indices.into_iter();
            let mut index = elem_ix_iter.next().unwrap().into_inner();

            // track output element pointer
            let mut elem_dst_ptr: WackTempIdent;
            loop {
                // obtain index value, and the corresponding element type (for the scale)
                let index_value = self.lower_expr(index.clone(), instructions);
                // SAFETY: this is guaranteed to be an array-type, if not, previous stage has bugs.
                let array_elem_type = unsafe { array_type.clone().into_array_elem_type() };
                let scale = array_elem_type.size_of();

                // obtain pointer to element
                elem_dst_ptr = self.make_temporary();
                instructions.push(WackInstr::ArrayAccess {
                    src_array_ptr: WackValue::Var(src_array_ptr.clone()),
                    index: index_value,
                    scale,
                    dst_elem_ptr: elem_dst_ptr.clone(),
                });

                // if there is more indices, it means the element pointer points to an array;
                // dereference it to obtain another array, and set up for another loop;
                if let Some(next_index) = elem_ix_iter.next() {
                    // update `src_array_pointer` by dereferencing the `elem_dst_ptr`
                    src_array_ptr = self.make_temporary();
                    instructions.push(WackInstr::Load {
                        src_ptr: WackValue::Var(elem_dst_ptr.clone()),
                        dst: WackValue::Var(src_array_ptr.clone()),
                    });

                    // load up the next index, and update base array's type
                    index = next_index.into_inner();
                    array_type = array_elem_type;
                } else {
                    // if this is the last index, check that the end-type doesn't disagree with
                    // semantic type; if it does, there is a bug in the frontend
                    // TODO: it may be the case that element types can be coerced safely to the
                    //       overall array type, so this assert may trigger false-positives
                    assert_eq!(array_elem_type, sem_type);
                    break;
                }
            }

            // return variable holding pointer to element
            WackValue::Var(elem_dst_ptr)
        }

        // Very confusing but converts a syntax literal to Wacky Value
        // For now their definitions are basically the same
        fn lower_literal(liter: ast::Liter) -> WackValue {
            WackValue::Literal(liter.into())
        }

        // Very confusing but converts a syntax unary operand to Wacky Operator
        // For now their definitions are the same, but they may diverge in the future
        // TODO: check this later

        fn lower_unary(
            &mut self,
            unary_op: UnaryOper,
            expr: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> WackValue {
            let src = self.lower_expr(expr, instr);
            let dst_name = self.make_temporary();
            let dst = WackValue::Var(dst_name);
            let wacky_op: UnaryOp = unary_op.into();
            let new_instr = WackInstr::Unary {
                op: wacky_op,
                src,
                dst: dst.clone(),
            };
            instr.push(new_instr);
            dst
        }

        fn lower_binary(
            &mut self,
            expr1: TypedExpr,
            binop: BinaryOper,
            expr2: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> WackValue {
            match binop {
                BinaryOper::And => self.lower_and_expr(expr1, expr2, instr),
                BinaryOper::Or => self.lower_or_expr(expr1, expr2, instr),
                _ => self.lower_normal_binary(expr1, binop, expr2, instr),
            }
        }

        fn lower_normal_binary(
            &mut self,
            expr1: TypedExpr,
            binop: BinaryOper,
            expr2: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> WackValue {
            // We handle And/Or differently since we'll make them short circuit here
            let src1 = self.lower_expr(expr1, instr);
            let src2 = self.lower_expr(expr2, instr);
            let dst_name = self.make_temporary();
            let dst = WackValue::Var(dst_name);
            let wacky_op: BinaryOp = binop.into();
            let new_instr = WackInstr::Binary {
                op: wacky_op,
                src1,
                src2,
                dst: dst.clone(),
            };
            instr.push(new_instr);
            dst
        }

        fn lower_or_expr(
            &mut self,
            expr1: TypedExpr,
            expr2: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> WackValue {
            // Makes my life easier
            use WackInstr as Instr;

            // Create labels for false branch and end of expr
            let true_label = self.make_label("or_true");
            let end_label = self.make_label("or_end");

            // Create a temporary variable to store the result of expression
            let dst = WackValue::Var(self.make_temporary());

            let left_v = self.lower_expr(expr1, instr);

            instr.push(Instr::JumpIfNotZero {
                condition: left_v,
                target: true_label.clone(),
            });

            let right_v = self.lower_expr(expr2, instr);

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

            dst
        }

        fn lower_and_expr(
            &mut self,
            expr1: TypedExpr,
            expr2: TypedExpr,
            instr: &mut Vec<WackInstr>,
        ) -> WackValue {
            // Makes my life easier
            use WackInstr as Instr;

            // Create labels for false branch and end of expr
            let false_label = self.make_label("and_false");
            let end_label = self.make_label("and_end");

            // Create a temporary variable to store the result of expression
            let dst = WackValue::Var(self.make_temporary());

            let left_v = self.lower_expr(expr1, instr);

            instr.push(Instr::JumpIfZero {
                condition: left_v,
                target: false_label.clone(),
            });

            let right_v = self.lower_expr(expr2, instr);

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

            dst
        }
    }
}
