// use crate::source::{SourcedBoxedNode, SourcedNode, SourcedSpan};
// use crate::wacc_hir::hir;
// use crate::wacc_hir::lower_ast::FuncSymbolTable;
// use crate::wacc_thir::thir::{ArrayElem, Expr, Ident, Liter};
// use crate::wacc_thir::types::{BaseType, Type};
// use chumsky::span::Span;
// use std::collections::HashMap;
//
// // A file-local type alias for better readability of type definitions
// type SN<T> = SourcedNode<T>;
// type SBN<T> = SourcedBoxedNode<T>;
//
// #[derive(Debug, Clone)]
// pub enum HirLoweringError {
//     // ArityMismatch(SN<Ident>, usize, usize),
//     // DuplicateIdent(SN<Ident>),
//     TypeMismatch {
//         span: SourcedSpan,
//         expected: Type,
//         actual: Type,
//     },
//     ExpectedArrayType {
//         span: SourcedSpan,
//         actual: Type,
//     },
//     UntypedIdentEncountered(SN<hir::Ident>), // AssignmentWithBothSidesUnknown(SourcedSpan),
//     // MismatchedArgCount(SourcedSpan, usize, usize),
//     InvalidIndexType {
//         span: SourcedSpan,
//         actual: Type,
//     },
//     // InvalidFreeType(SourcedSpan, SemanticType),
//     InvalidNumberOfIndexes {
//         span: SourcedSpan,
//         max_possible_indices: usize,
//         actual_indices: usize,
//     },
//     // UndefinedIdent(SN<Ident>),
//     // ReturnInMain(SourcedSpan),
// }
//
// struct HirLoweringCtx {
//     errors: Vec<HirLoweringError>,
//     func_symbol_table: FuncSymbolTable,
//     current_func_hir_return_type: Option<hir::Type>,
//     hir_ident_symbol_table: HashMap<hir::Ident, Type>,
// }
//
// impl HirLoweringCtx {
//     fn new(func_symbol_table: FuncSymbolTable) -> Self {
//         Self {
//             errors: Vec::new(),
//             func_symbol_table,
//             current_func_hir_return_type: None,
//             hir_ident_symbol_table: HashMap::new(),
//         }
//     }
//
//     fn add_error(&mut self, error: HirLoweringError) {
//         self.errors.push(error);
//     }
//
//     // fn unary_expect(
//     //     &mut self,
//     //     expr: &SN<Expr<RenamedName, SemanticType>>,
//     //     expected_type: SemanticType,
//     //     result_type: SemanticType,
//     // ) -> SemanticType {
//     //     if expr.get_type(self).can_coerce_into(&expected_type) {
//     //         result_type
//     //     } else {
//     //         self.add_error(TypeMismatch(
//     //             expr.span(),
//     //             expr.get_type(self),
//     //             expected_type,
//     //         ));
//     //         SemanticType::Error(expr.span())
//     //     }
//     // }
//     //
//     // fn binary_expect(
//     //     &mut self,
//     //     lhs: SN<Expr<RenamedName, SemanticType>>,
//     //     rhs: SN<Expr<RenamedName, SemanticType>>,
//     //     expected_type: SemanticType,
//     //     result_type: SemanticType,
//     // ) -> SemanticType {
//     //     if !lhs.get_type(&self).can_coerce_into(&expected_type.clone()) {
//     //         self.add_error(TypeMismatch(
//     //             lhs.span(),
//     //             lhs.get_type(&self),
//     //             expected_type.clone(),
//     //         ));
//     //         SemanticType::Error(lhs.span());
//     //     }
//     //     if !rhs.get_type(&self).can_coerce_into(&expected_type) {
//     //         self.add_error(TypeMismatch(rhs.span(), rhs.get_type(&self), expected_type));
//     //         SemanticType::Error(rhs.span())
//     //     } else {
//     //         result_type
//     //     }
//     // }
//     //
//     // #[inline]
//     // pub fn lookup_symbol_table(&self, renamed_name: &SN<RenamedName>) -> SemanticType {
//     //     if let Some(semantic_type) = self.symid_table.get(renamed_name.inner()) {
//     //         semantic_type.clone()
//     //     } else {
//     //         SemanticType::Error(renamed_name.span())
//     //     }
//     // }
//
//     // ----
//
//     //     pub fn lower_program(&mut self, program: ast::Program) -> Program {
//     //     // Build function table
//     //     if let Err(err) = Self::build_func_table(&mut self.func_symbol_table, &program) {
//     //         self.add_error(err);
//     //     }
//     //
//     //     // cannot curry mutable-references and still have FnMut, RIP :(
//     //     let lowered_funcs = program.funcs.map(|f| self.lower_func(f));
//     //     let lowered_body = self.lower_stat_block(program.body);
//     //
//     //     // return lowered program
//     //     Program {
//     //         funcs: lowered_funcs,
//     //         body: lowered_body,
//     //     }
//     // }
//     //
//     // /// When you enter a new function, you create a new scope just for the args,
//     // /// Then you create another scope for the body of the function
//     // /// We also keep track if we're in the main function
//     // #[inline]
//     // pub fn lower_func(&mut self, func: ast::Func) -> Func {
//     //     let ast::Func {
//     //         return_type,
//     //         name,
//     //         params,
//     //         body,
//     //     } = func;
//     //
//     //     // perform lowering within temporary-map context
//     //     self.in_main = false;
//     //     let new_map = self.copy_id_map_with_false();
//     //     let old_id_map = mem::replace(&mut self.identifier_map, new_map);
//     //     let return_type = Self::lower_type_sn(&return_type); // Type remains unchanged
//     //     // cannot curry mutable-references and still have FnMut, RIP :(
//     //     let params = params.map(|p| self.lower_func_param(p));
//     //     let body = self.with_temporary_map(|slf| slf.lower_stat_block(body));
//     //     self.identifier_map = old_id_map;
//     //     self.in_main = true;
//     //
//     //     // construct output function
//     //     Func {
//     //         return_type,
//     //         name,
//     //         params,
//     //         body,
//     //     }
//     // }
//     //
//     // #[allow(clippy::indexing_slicing)]
//     // #[inline]
//     // pub fn lower_func_param(&mut self, func_param: ast::FuncParam) -> FuncParam {
//     //     let ast::FuncParam { r#type, name } = func_param;
//     //
//     //     // shouldn't panic since we check the key exists
//     //     let lowered_type = Self::lower_type_sn(&r#type);
//     //     if self.identifier_map.contains_key(name.inner())
//     //         && self.identifier_map[name.inner()].from_current_block
//     //     {
//     //         self.add_error(AstLoweringError::DuplicateIdent(name.clone()));
//     //         // return dummy Stat
//     //         return FuncParam {
//     //             r#type: lowered_type,
//     //             name: Ident::new_rouge_zero_sn(name),
//     //         };
//     //     }
//     //
//     //     // insert new identifier corresponding to function parameter
//     //     let unique_name = Ident::new_sn(&mut self.counter, name.clone());
//     //     let map_entry = IDMapEntry::new(unique_name.clone(), true);
//     //     self.identifier_map.insert(name.inner().clone(), map_entry);
//     //
//     //     // return the renamed function parameter
//     //     FuncParam {
//     //         r#type: lowered_type,
//     //         name: unique_name,
//     //     }
//     // }
//     //
//     // #[allow(clippy::expect_used)]
//     // pub fn lower_stat_block(&mut self, stat_block: ast::StatBlock) -> StatBlock {
//     //     // lower everything within a temporarily-renamed context
//     //     self.with_temporary_map(|slf| {
//     //         // accumulate lowered statements into output vector
//     //         let mut lowered_stats: MultiItemVec<SN<Stat>> = MultiItemVec::new();
//     //         for stat_sn in stat_block.0 {
//     //             lowered_stats.push_multi_item(slf.lower_stat_sn(stat_sn));
//     //         }
//     //
//     //         // there should always be >=1 lowered statements by this point
//     //         StatBlock(NonemptyArray::try_from_boxed_slice(lowered_stats).expect(
//     //             "Lowering a `StatBlock` should always leave produce at least one lowered `Stat`",
//     //         ))
//     //     })
//     // }
//     //
//     // #[allow(clippy::shadow_unrelated)]
//     // #[allow(clippy::indexing_slicing)]
//     // pub fn lower_stat_sn(&mut self, stat_sn: SN<ast::Stat>) -> MultiItem<SN<Stat>> {
//     //     // decompose statement, and create curried constructor
//     //     let (stat, span) = stat_sn.into_tuple();
//     //     let sn_new_flipped = (|c, i| SN::new(i, c)).curry();
//     //     let stat_sn = sn_new_flipped(span).chain(MultiItem::Item);
//     //
//     //     // match on statement, and produce either one spanned-statement or many of them
//     //     match stat {
//     //         ast::Stat::Skip => stat_sn(Stat::Skip),
//     //         ast::Stat::VarDefinition {
//     //             r#type,
//     //             name,
//     //             rvalue,
//     //         } => {
//     //             let lowered_type = Self::lower_type_sn(&r#type);
//     //
//     //             // Sorry about the lots of clones here
//     //             // Evaluate the rhs before creating unique name to not allow int x = x
//     //             // where x is not defined yet
//     //             // resolved_expr returns a new copy of the initializer with any variables renamed
//     //             let lowered_rvalue = self.lower_rvalue_sn(rvalue);
//     //
//     //             // Check for duplicate id after resolving rvalue
//     //             // shouldn't panic since we check the key exists
//     //             if self.identifier_map.contains_key(name.inner())
//     //                 && self.identifier_map[name.inner()].from_current_block
//     //             {
//     //                 self.add_error(AstLoweringError::DuplicateIdent(name.clone()));
//     //                 // return dummy Stat
//     //                 return stat_sn(Stat::VarDefinition {
//     //                     r#type: lowered_type,
//     //                     name: Ident::new_rouge_zero_sn(name),
//     //                     rvalue: lowered_rvalue,
//     //                 });
//     //             }
//     //
//     //             let unique_name = Ident::new_sn(&mut self.counter, name.clone());
//     //             let map_entry = IDMapEntry::new(unique_name.clone(), true);
//     //             self.identifier_map.insert(name.inner().clone(), map_entry);
//     //
//     //             stat_sn(Stat::VarDefinition {
//     //                 r#type: lowered_type,
//     //                 name: unique_name,
//     //                 rvalue: lowered_rvalue,
//     //             })
//     //         }
//     //         ast::Stat::Assignment { lvalue, rvalue } => stat_sn(Stat::Assignment {
//     //             lvalue: self.lower_lvalue_sn(lvalue),
//     //             rvalue: self.lower_rvalue_sn(rvalue),
//     //         }),
//     //         ast::Stat::Read(lv) => stat_sn(Stat::Read(self.lower_lvalue_sn(lv))),
//     //         ast::Stat::Free(e) => stat_sn(Stat::Free(self.lower_expr_sn(e))),
//     //         ast::Stat::Return(e) => stat_sn({
//     //             // If we do a return in main, we add an error
//     //             if self.in_main {
//     //                 self.add_error(AstLoweringError::ReturnInMain(e.span()));
//     //             }
//     //             Stat::Return(self.lower_expr_sn(e))
//     //         }),
//     //         ast::Stat::Exit(e) => stat_sn(Stat::Exit(self.lower_expr_sn(e))),
//     //         ast::Stat::Print(e) => stat_sn(Stat::Print(self.lower_expr_sn(e))),
//     //         ast::Stat::Println(e) => stat_sn(Stat::Println(self.lower_expr_sn(e))),
//     //         ast::Stat::IfThenElse {
//     //             if_cond,
//     //             then_body,
//     //             else_body,
//     //         } => stat_sn(Stat::IfThenElse {
//     //             if_cond: self.lower_expr_sn(if_cond),
//     //             then_body: self.lower_stat_block(then_body),
//     //             else_body: self.lower_stat_block(else_body),
//     //         }),
//     //         // TODO: refactor this branch once loop-break implemented
//     //         ast::Stat::WhileDo { while_cond, body } => stat_sn(Stat::WhileDo {
//     //             while_cond: self.lower_expr_sn(while_cond),
//     //             body: self.lower_stat_block(body),
//     //         }),
//     //         ast::Stat::Scoped(block) => MultiItem::multi(self.lower_stat_block(block).0),
//     //     }
//     // }
//     //
//     // pub fn lower_lvalue(&mut self, lvalue: ast::LValue) -> LValue {
//     //     match lvalue {
//     //         ast::LValue::Ident(i) => LValue::Ident(self.lower_ident_sn(i)),
//     //         ast::LValue::ArrayElem(a) => LValue::ArrayElem(self.lower_array_elem_sn(a)),
//     //         ast::LValue::PairElem(p) => LValue::PairElem(self.lower_pair_elem_sn(p)),
//     //     }
//     // }
//     //
//     // #[inline]
//     // pub fn lower_lvalue_sn(&mut self, lvalue_sn: SN<ast::LValue>) -> SN<LValue> {
//     //     lvalue_sn.map_inner(Self::lower_lvalue.curry()(self))
//     // }
//     //
//     // #[inline]
//     // pub fn lower_lvalue_sbn(&mut self, lvalue_sbn: SBN<ast::LValue>) -> SBN<LValue> {
//     //     lvalue_sbn.map_inner_unboxed(Self::lower_lvalue.curry()(self))
//     // }
//     //
//     // #[inline]
//     // pub fn lower_rvalue(&mut self, rvalue: ast::RValue) -> RValue {
//     //     match rvalue {
//     //         ast::RValue::Expr(e) => RValue::Expr(self.lower_expr_sn(e)),
//     //         ast::RValue::ArrayLiter(a) => RValue::ArrayLiter(a.map(|e| self.lower_expr_sn(e))),
//     //         ast::RValue::NewPair(le, re) => {
//     //             RValue::NewPair(self.lower_expr_sn(le), self.lower_expr_sn(re))
//     //         }
//     //         ast::RValue::PairElem(p) => RValue::PairElem(self.lower_pair_elem_sn(p)),
//     //         ast::RValue::Call { func_name, args } => RValue::Call {
//     //             func_name,
//     //             // cannot curry mutable-references and still have FnMut, RIP :(
//     //             args: args.map(|e| self.lower_expr_sn(e)),
//     //         },
//     //     }
//     // }
//     //
//     // #[inline]
//     // pub fn lower_rvalue_sn(&mut self, rvalue_sbn: SN<ast::RValue>) -> SN<RValue> {
//     //     rvalue_sbn.map_inner(Self::lower_rvalue.curry()(self))
//     // }
//     //
//     // #[inline]
//     // pub const fn lower_pair_elem_selector(
//     //     pair_elem_selector: &ast::PairElemSelector,
//     // ) -> PairElemSelector {
//     //     match *pair_elem_selector {
//     //         ast::PairElemSelector::Fst => PairElemSelector::Fst,
//     //         ast::PairElemSelector::Snd => PairElemSelector::Snd,
//     //     }
//     // }
//     //
//     // #[inline]
//     // pub fn lower_pair_elem(&mut self, pair_elem: ast::PairElem) -> PairElem {
//     //     let ast::PairElem(selector, lvalue) = pair_elem;
//     //     PairElem(
//     //         Self::lower_pair_elem_selector(&selector),
//     //         self.lower_lvalue_sbn(lvalue),
//     //     )
//     // }
//     //
//     // #[inline]
//     // pub fn lower_pair_elem_sn(&mut self, pair_elem_sn: SN<ast::PairElem>) -> SN<PairElem> {
//     //     pair_elem_sn.map_inner(Self::lower_pair_elem.curry()(self))
//     // }
//
//     #[inline]
//     pub fn lower_array_elem(&mut self, array_elem: hir::ArrayElem) -> ArrayElem {
//         let hir::ArrayElem {
//             array_name: array_name_sn,
//             indices: indices_sn,
//         } = array_elem;
//
//         // look up identifier and its type
//         let array_name = self.lookup_ident_sn(array_name_sn.clone());
//
//         // resolve indices, ensuring they are of the `int` type
//         let indices = indices_sn.clone().map(|i| {
//             // get resolved expression
//             let resolved_i = self.lower_expr_sn(i.clone());
//             let resolved_i_type = resolved_i.r#type();
//
//             // ensure that type is `int`, or error
//             // TODO: check if this needs type casting or not
//             if resolved_i_type != Type::BaseType(BaseType::Int) {
//                 self.add_error(HirLoweringError::InvalidIndexType {
//                     span: i.span(),
//                     actual: resolved_i_type,
//                 });
//             }
//
//             resolved_i
//         });
//
//         // unwind the indices of array to ensure that the array has the right number of indices
//         let actual_indices = indices.len();
//         let mut indices_iter = indices.into_iter().enumerate();
//         let (_, first_index) = indices_iter
//             .next()
//             .expect("The first index value should always be there");
//         let mut curr_type = match array_name.clone().r#type {
//             Type::ArrayType(inner) => *inner,
//             Type::Any => Type::Any,
//             other => {
//                 self.add_error(HirLoweringError::ExpectedArrayType {
//                     span: array_name_sn.span(),
//                     actual: other.clone(),
//                 });
//                 other
//             }
//         };
//         let mut curr_array_elem = ArrayElem::FirstAccess {
//             array_name,
//             index: first_index,
//             r#type: curr_type.clone(),
//         };
//         let mut max_possible_indices = 1;
//
//         for (i, index) in indices_iter {
//             match curr_type {
//                 Type::ArrayType(inner) => {
//                     // update type, extend number of indices
//                     curr_type = *inner;
//                     max_possible_indices += 1;
//
//                     // update array elem value
//                     curr_array_elem = ArrayElem::NestedAccess {
//                         array_elem: Box::new(curr_array_elem),
//                         index,
//                         r#type: curr_type.clone(),
//                     }
//                 }
//                 Type::Any => {
//                     // update array elem value
//                     curr_array_elem = ArrayElem::NestedAccess {
//                         array_elem: Box::new(curr_array_elem),
//                         index,
//                         r#type: Type::Any,
//                     }
//                 }
//                 _ => {
//                     self.add_error(HirLoweringError::InvalidNumberOfIndexes {
//                         span: indices_sn[i].span(),
//                         max_possible_indices,
//                         actual_indices,
//                     });
//                     // update array elem value
//                     curr_array_elem = ArrayElem::NestedAccess {
//                         array_elem: Box::new(curr_array_elem),
//                         index,
//                         r#type: Type::Any,
//                     }
//                 }
//             }
//         }
//
//         // return resolved array_elem
//         curr_array_elem
//     }
//
//     #[inline]
//     pub fn lower_array_elem_sn(&mut self, array_elem_sn: SN<hir::ArrayElem>) -> ArrayElem {
//         self.lower_array_elem(array_elem_sn.into_inner())
//     }
//
//     #[inline]
//     pub fn lower_array_elem_sbn(&mut self, array_elem_sbn: SBN<hir::ArrayElem>) -> ArrayElem {
//         self.lower_array_elem(array_elem_sbn.into_inner_unboxed())
//     }
//
//     pub fn lower_expr(&mut self, expr: hir::Expr) -> Expr {
//         // match expr {
//         //     hir::Expr::Liter(l) => Expr::Liter(Self::lower_liter_sn(l)),
//         //     hir::Expr::Ident(i) => Expr::Ident(self.lookup_ident_sn(i)),
//         //     hir::Expr::ArrayElem(a) => Expr::ArrayElem(Box::new(self.lower_array_elem_sbn(a))),
//         //     hir::Expr::Unary(op, e) => {
//         //         Expr::Unary(Self::lower_unary_oper_sn(&op), self.lower_expr_sbn(e))
//         //     }
//         //     hir::Expr::Binary(le, op, re) => Expr::Binary(
//         //         self.lower_expr_sbn(le),
//         //         Self::lower_binary_oper_sn(&op),
//         //         self.lower_expr_sbn(re),
//         //     ),
//         //     hir::Expr::Paren(e) => self.lower_expr(e.into_inner_unboxed()),
//         //     hir::Expr::IfThenElse {
//         //         if_cond,
//         //         then_val,
//         //         else_val,
//         //     } => Expr::IfThenElse {
//         //         if_cond: self.lower_expr_sbn(if_cond),
//         //         then_val: self.lower_expr_sbn(then_val),
//         //         else_val: self.lower_expr_sbn(else_val),
//         //     },
//         // }
//         todo!()
//     }
//
//     #[inline]
//     pub fn lower_expr_sn(&mut self, expr_sn: SN<hir::Expr>) -> Expr {
//         self.lower_expr(expr_sn.into_inner())
//     }
//
//     // #[inline]
//     // pub fn lower_expr_sbn(&mut self, expr_sbn: SBN<ast::Expr>) -> SBN<Expr> {
//     //     expr_sbn.map_inner_unboxed(Self::lower_expr.curry()(self))
//     // }
//
//     #[allow(clippy::as_conversions)]
//     #[inline]
//     pub fn lower_liter(liter: hir::Liter) -> Liter {
//         // get type by pattern matching
//         let r#type = match liter {
//             hir::Liter::IntLiter(_) => Type::BaseType(BaseType::Int),
//             hir::Liter::BoolLiter(_) => Type::BaseType(BaseType::Bool),
//             hir::Liter::CharLiter(_) => Type::BaseType(BaseType::Char),
//             hir::Liter::StrLiter(_) => Type::BaseType(BaseType::String),
//             hir::Liter::PairLiter => Type::erased_pair_type(),
//         };
//
//         Liter { liter, r#type }
//     }
//
//     #[inline]
//     pub fn lower_liter_sn(liter: SN<hir::Liter>) -> Liter {
//         Self::lower_liter(liter.into_inner())
//     }
//
//     // #[inline]
//     // pub const fn lower_unary_oper(unary_oper: &ast::UnaryOper) -> UnaryOper {
//     //     match *unary_oper {
//     //         ast::UnaryOper::BNot => UnaryOper::BNot,
//     //         ast::UnaryOper::LNot => UnaryOper::LNot,
//     //         ast::UnaryOper::Minus => UnaryOper::Minus,
//     //         ast::UnaryOper::Len => UnaryOper::Len,
//     //         ast::UnaryOper::Ord => UnaryOper::Ord,
//     //         ast::UnaryOper::Chr => UnaryOper::Chr,
//     //     }
//     // }
//
//     // #[inline]
//     // pub fn lower_unary_oper_sn(unary_oper_sn: &SN<ast::UnaryOper>) -> SN<UnaryOper> {
//     //     unary_oper_sn
//     //         .transpose_ref()
//     //         .map_inner(Self::lower_unary_oper)
//     // }
//     //
//     // #[inline]
//     // pub const fn lower_binary_oper(binary_oper: &ast::BinaryOper) -> BinaryOper {
//     //     match *binary_oper {
//     //         ast::BinaryOper::Mul => BinaryOper::Mul,
//     //         ast::BinaryOper::Div => BinaryOper::Div,
//     //         ast::BinaryOper::Mod => BinaryOper::Mod,
//     //         ast::BinaryOper::Add => BinaryOper::Add,
//     //         ast::BinaryOper::Sub => BinaryOper::Sub,
//     //         ast::BinaryOper::Lte => BinaryOper::Lte,
//     //         ast::BinaryOper::Lt => BinaryOper::Lt,
//     //         ast::BinaryOper::Gte => BinaryOper::Gte,
//     //         ast::BinaryOper::Gt => BinaryOper::Gt,
//     //         ast::BinaryOper::Eq => BinaryOper::Eq,
//     //         ast::BinaryOper::Neq => BinaryOper::Neq,
//     //         ast::BinaryOper::BAnd => BinaryOper::BAnd,
//     //         ast::BinaryOper::BXor => BinaryOper::BXor,
//     //         ast::BinaryOper::BOr => BinaryOper::BOr,
//     //         ast::BinaryOper::LAnd => BinaryOper::LAnd,
//     //         ast::BinaryOper::LOr => BinaryOper::LOr,
//     //     }
//     // }
//     //
//     // #[inline]
//     // pub fn lower_binary_oper_sn(binary_oper_sn: &SN<ast::BinaryOper>) -> SN<BinaryOper> {
//     //     binary_oper_sn
//     //         .transpose_ref()
//     //         .map_inner(Self::lower_binary_oper)
//     // }
//
//     pub fn lower_type(r#type: &hir::Type) -> Type {
//         match *r#type {
//             hir::Type::BaseType(ref b) => Type::BaseType(Self::lower_base_type_sn(b)),
//             hir::Type::ArrayType(ref a) => Self::lower_array_type_sbn(a),
//             hir::Type::PairType(ref fst, ref snd) => Type::PairType(
//                 Box::new(Self::lower_pair_elem_type(fst)),
//                 Box::new(Self::lower_pair_elem_type(snd)),
//             ),
//         }
//     }
//
//     #[inline]
//     pub fn lower_type_sn(type_sn: &SN<hir::Type>) -> Type {
//         Self::lower_type(type_sn.inner())
//     }
//
//     #[inline]
//     pub const fn lower_base_type(base_type: &hir::BaseType) -> BaseType {
//         match *base_type {
//             hir::BaseType::Int => BaseType::Int,
//             hir::BaseType::Bool => BaseType::Bool,
//             hir::BaseType::Char => BaseType::Char,
//             hir::BaseType::String => BaseType::String,
//         }
//     }
//
//     #[inline]
//     pub const fn lower_base_type_sn(base_type_sn: &SN<hir::BaseType>) -> BaseType {
//         Self::lower_base_type(base_type_sn.inner())
//     }
//
//     #[inline]
//     pub fn lower_array_type(array_type: &hir::ArrayType) -> Type {
//         Type::ArrayType(Box::new(Self::lower_type(&array_type.elem_type)))
//     }
//
//     #[inline]
//     pub fn lower_array_type_sbn(array_type_sbn: &SBN<hir::ArrayType>) -> Type {
//         Self::lower_array_type(array_type_sbn.inner())
//     }
//
//     pub fn lower_pair_elem_type(pair_elem_type: &hir::PairElemType) -> Type {
//         match *pair_elem_type {
//             hir::PairElemType::ArrayType(ref a) => Self::lower_array_type_sbn(a),
//             hir::PairElemType::BaseType(ref b) => Type::BaseType(Self::lower_base_type_sn(b)),
//             hir::PairElemType::Pair(_) => Type::erased_pair_type(),
//         }
//     }
//
//     #[inline]
//     pub fn lookup_ident_sn(&mut self, ident: SN<hir::Ident>) -> Ident {
//         // find actual type, or emmit error
//         let actual_type = if let Some(actual_type) = self.hir_ident_symbol_table.get(ident.inner())
//         {
//             actual_type
//         } else {
//             self.add_error(HirLoweringError::UntypedIdentEncountered(ident.clone()));
//
//             // the identifier is of type `Any` at this point, which casts to all other types
//             return Ident {
//                 ident: ident.inner().clone(),
//                 r#type: Type::Any,
//             };
//         };
//
//         // return resulting actual type
//         Ident {
//             ident: ident.inner().clone(),
//             r#type: actual_type.clone(),
//         }
//     }
//
//     #[inline]
//     pub fn expect_ident_sn(&mut self, ident_sn: SN<hir::Ident>, expected_type: Type) -> Ident {
//         let Ident {
//             ident,
//             r#type: actual_type,
//         } = self.lookup_ident_sn(ident_sn.clone());
//
//         // TODO: replace coercability checking rather than naiive type-equality
//         //       and ensure we are swapping in the right types with most precision
//         if expected_type == actual_type.clone() {
//             Ident {
//                 // TODO: ensure type-swapping is correct
//                 ident,
//                 r#type: actual_type,
//             }
//         } else {
//             self.add_error(HirLoweringError::TypeMismatch {
//                 span: ident_sn.span(),
//                 expected: expected_type,
//                 actual: actual_type.clone(),
//             });
//
//             // the identifier is of type `Any` at this point, which casts to all other types
//             Ident {
//                 ident,
//                 r#type: Type::Any,
//             }
//         }
//     }
//     //
//     // #[inline]
//     // fn lower_funcname_sn(&mut self, name: SN<ast::Ident>) -> SN<ast::Ident> {
//     //     // Use ident part of name and then check if it exists in the function table
//     //     // return name regardless but add an error if it doesn't exist
//     //     let ident = name.inner();
//     //     if self.func_symbol_table.functions.contains_key(ident) {
//     //     } else {
//     //         self.add_error(AstLoweringError::UndefinedIdent(name.clone()));
//     //         // Return a dummy value so we can maybe very hopefully
//     //         // allow multiple semantic errors
//     //     }
//     //     name
//     // }
// }
