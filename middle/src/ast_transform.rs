use std::collections::HashMap;

use syntax::ast::{
    ArrayElem, BinaryOper, Expr, Func, FuncParam, Ident, LValue, Liter, PairElem, Program, RValue,
    Stat, StatBlock, UnaryOper,
};
use syntax::rename::RenamedName;
use syntax::typecheck::TypeResolver;
use syntax::{rename::IdFuncTable, types::SemanticType};
use syntax::source::SourcedNode;
use crate::wackir::{
    BinaryOperator, ConvertToMidIdent as _, MidIdent, UnaryOperator, WackConst, WackFunction,
    WackInstruction, WackProgram, WackValue,
};

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
    let mut context = create_lowering_context(type_resolver);
    let mut main_body = Vec::new();
    context.lower_stat_block(program.body, &mut main_body);

    let wack_functions = program
        .funcs
        .into_iter()
        .map(|func| context.lower_func(func))
        .collect();

    let wack_program = WackProgram {
        functions: wack_functions,
        main_body,
    };

    (wack_program, context.counter())
}

/* ================== INTERNAL API ================== */
struct Lowerer {
    counter: usize,
    func_table: HashMap<Ident, (SemanticType, Vec<SemanticType>)>,
    symbol_table: HashMap<MidIdent, SemanticType>,
    // used to rename function to wacc_function
    func_map: HashMap<Ident, Ident>,
}

fn rename_function_ident(ident: &Ident) -> Ident {
    Ident::from_str(&format!("wacc_{ident}"))
}

fn create_lowering_context(type_resolver: TypeResolver) -> Lowerer {
    let renamer = type_resolver.renamer;
    let mut func_map: HashMap<Ident, Ident> = HashMap::new();
    let mut counter = renamer.counter();
    let func_table = renamer
        .id_func_table
        .functions
        .into_iter()
        .map(|(id, (func, params))| {
            let new_id = rename_function_ident(&id);
            func_map.insert(id, new_id.clone());
            (new_id, (func, params))
        })
        .collect();
    let symbol_table: HashMap<MidIdent, SemanticType> = type_resolver
        .symid_table
        .into_iter()
        .map(|(id, symid)| (id.to_mid_ident(&mut counter), symid))
        .collect();
    Lowerer {
        counter,
        func_table,
        symbol_table,
        func_map,
    }
}

impl Lowerer {
    const fn counter(&self) -> usize {
        self.counter
    }
    // Makes a temporary wacky variable
    fn make_temporary(&mut self) -> MidIdent {
        // Eventually we may want to replace temp with a function name
        // for better debugging
        let ident = Ident::from_str("temp");
        ident.to_mid_ident(&mut self.counter)
    }

    // Makes a label for jump's for now
    fn make_label(&mut self, name: &str) -> MidIdent {
        let ident = Ident::from_str(name);
        ident.to_mid_ident(&mut self.counter)
    }

    fn lower_func(&mut self, func: TypedFunc) -> WackFunction {
        // TODO: Figure out how/when to take in a list of instructions as parameter
        let mut instructions: Vec<WackInstruction> = Vec::new();
        self.lower_stat_block(func.body, &mut instructions);
        let params = func
            .params
            .into_iter()
            .map(|param| self.lower_func_param(&param))
            .collect();

        let name = self
            .func_map
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
    fn lower_func_param(&mut self, param: &TypedFuncParam) -> MidIdent {
        param.name.to_mid_ident(&mut self.counter)
    }

    fn lower_stat_block(
        &mut self,
        stat_block: TypedStatBlock,
        instructions: &mut Vec<WackInstruction>,
    ) {
        for stat in stat_block.0 {
            self.lower_stat(stat.into_inner(), instructions);
        }
    }

    fn lower_stat(&mut self, stat: TypedStat, instructions: &mut Vec<WackInstruction>) {
        match stat {
            TypedStat::Skip => (),
            TypedStat::VarDefinition {
                r#type,
                name,
                rvalue,
            } => {
                let mid_ident = name.into_inner().to_mid_ident(&mut self.counter);
                let lhs = WackValue::Var(mid_ident);
                let sem_type = r#type;
                let rhs = self.lower_rvalue(rvalue.into_inner(), instructions);
                let instr = WackInstruction::Copy { src: rhs, dst: lhs };
                instructions.push(instr);
            }
            TypedStat::Assignment { lvalue, rvalue } => {
                let lhs = self.lower_lvalue(lvalue.into_inner(), instructions);
                let rhs = self.lower_rvalue(rvalue.into_inner(), instructions);
                let instr = WackInstruction::Copy { src: rhs, dst: lhs };
                instructions.push(instr);
            }
            TypedStat::Read(lvalue) => {
                let sem_type = lvalue.inner().get_type();
                let value = self.lower_lvalue(lvalue.into_inner(), instructions);
                let instr = WackInstruction::Read { dst: value, ty: sem_type };
                instructions.push(instr);
            },
            TypedStat::Free(expr) => {
                let sem_type = expr.inner().get_type();
                match sem_type {
                    SemanticType::Array(_)
                    | SemanticType::Pair(_, _)
                    | SemanticType::ErasedPair => (),
                    _ => unreachable!("free should not be called on anything except pairs and arrays"),
                }
                let value = self.lower_expr(expr.into_inner(), instructions);
                let instr = WackInstruction::Free(value);
                instructions.push(instr);
            },
            TypedStat::Return(expr) => {
                let value = self.lower_expr(expr.into_inner(), instructions);
                let instr = WackInstruction::Return(value);
                instructions.push(instr);
            }
            TypedStat::Exit(expr) => {
                let sem_type = expr.inner().get_type();
                if sem_type != SemanticType::Int {
                    unreachable!("exit should be provided only with int exit value");
                }
                let value = self.lower_expr(expr.into_inner(), instructions);
                let instr = WackInstruction::Exit(value);
                instructions.push(instr);
            },
            TypedStat::Print(expr) => {
                let sem_type = expr.inner().get_type();
                let value = self.lower_expr(expr.into_inner(), instructions);
                let instr = WackInstruction::Print {
                    src: value,
                    ty: sem_type,
                };
                instructions.push(instr);
            },
            TypedStat::Println(expr) => {
                let sem_type = expr.inner().get_type();
                let value = self.lower_expr(expr.into_inner(), instructions);
                let instr = WackInstruction::Println {
                    src: value,
                    ty: sem_type,
                };
                instructions.push(instr);
            },
            TypedStat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => {
                // Makes my life easier
                use WackInstruction as Instr;

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
                use WackInstruction as Instr;
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
    fn lower_expr(
        &mut self,
        expr: TypedExpr,
        instructions: &mut Vec<WackInstruction>,
    ) -> WackValue {
        match expr {
            TypedExpr::Liter(liter, _t) => Self::lower_literal(liter),
            TypedExpr::Ident(sn_ident, _t) => {
                WackValue::Var(sn_ident.into_inner().to_mid_ident(&mut self.counter))
            }
            TypedExpr::ArrayElem(array_elem, t) => panic!("ArrayElem not implem in Wacky"),
            TypedExpr::Unary(sn_unary, sn_expr, _t) => {
                self.lower_unary(sn_unary.into_inner(), sn_expr.into_inner(), instructions)
            }
            TypedExpr::Binary(sn_expr1, sn_binop, sn_expr2, t) => self.lower_binary(
                sn_expr1.into_inner(),
                sn_binop.into_inner(),
                sn_expr2.into_inner(),
                instructions,
            ),
            TypedExpr::Paren(sn_expr, _t) => self.lower_expr(sn_expr.into_inner(), instructions),
            TypedExpr::Error(_) => panic!("Bug somewhere in frontend."),
        }
    }

    fn lower_rvalue(
        &mut self,
        rvalue: TypedRValue,
        instructions: &mut Vec<WackInstruction>,
    ) -> WackValue {
        match rvalue {
            TypedRValue::Expr(expr, _) => self.lower_expr(expr.into_inner(), instructions),
            TypedRValue::ArrayLiter(_, _) => unimplemented!(),
            TypedRValue::NewPair(_, _, _) => unimplemented!(),
            // TODO: please add types to this
            TypedRValue::PairElem(_, _) => unimplemented!(),
            TypedRValue::Call {
                func_name,
                args,
                return_type,
            } => {
                let wacky_args = args
                    .into_iter()
                    .map(|arg| self.lower_expr(arg.into_inner(), instructions))
                    .collect();
                let dst = WackValue::Var(self.make_temporary());
                let wacky_func_name = self
                    .func_map
                    .get(&func_name)
                    .expect("Function should be in map");
                let instr = WackInstruction::FunCall {
                    fun_name: wacky_func_name.clone(),
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
        instructions: &mut Vec<WackInstruction>,
    ) -> WackValue {
        match lvalue {
            TypedLValue::Ident(ident, _) => {
                WackValue::Var(ident.into_inner().to_mid_ident(&mut self.counter))
            }
            TypedLValue::ArrayElem(array_elem, _) => panic!("ArrayElem not implemented in Wacky"),
            TypedLValue::PairElem(pair_elem, _) => panic!("PairElem not implemented in Wacky"),
        }
    }

    // Very confusing but converts a syntax literal to Wacky Value
    // For now their definitions are basically the same
    fn lower_literal(liter: Liter) -> WackValue {
        WackValue::Constant(liter.into())
    }

    // Very confusing but converts a syntax unary operand to Wacky Operator
    // For now their definitions are the same, but they may diverge in the future
    // TODO: check this later

    fn lower_unary(
        &mut self,
        unary_op: UnaryOper,
        expr: TypedExpr,
        instr: &mut Vec<WackInstruction>,
    ) -> WackValue {
        let src = self.lower_expr(expr, instr);
        let dst_name = self.make_temporary();
        let dst = WackValue::Var(dst_name);
        let wacky_op: UnaryOperator = unary_op.into();
        let new_instr = WackInstruction::Unary {
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
        instr: &mut Vec<WackInstruction>,
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
        instr: &mut Vec<WackInstruction>,
    ) -> WackValue {
        // We handle And/Or differently since we'll make them short circuit here
        let src1 = self.lower_expr(expr1, instr);
        let src2 = self.lower_expr(expr2, instr);
        let dst_name = self.make_temporary();
        let dst = WackValue::Var(dst_name);
        let wacky_op: BinaryOperator = binop.into();
        let new_instr = WackInstruction::Binary {
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
        instr: &mut Vec<WackInstruction>,
    ) -> WackValue {
        // Makes my life easier
        use WackInstruction as Instr;

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
            src: WackValue::Constant(WackConst::Bool(0)),
            dst: dst.clone(),
        });
        // Jump over the true branch
        instr.push(Instr::Jump(end_label.clone()));

        // True branch
        instr.push(Instr::Label(true_label));
        instr.push(Instr::Copy {
            src: WackValue::Constant(WackConst::Bool(1)),
            dst: dst.clone(),
        });

        instr.push(Instr::Label(end_label));

        dst
    }

    fn lower_and_expr(
        &mut self,
        expr1: TypedExpr,
        expr2: TypedExpr,
        instr: &mut Vec<WackInstruction>,
    ) -> WackValue {
        // Makes my life easier
        use WackInstruction as Instr;

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
            src: WackValue::Constant(WackConst::Bool(1)),
            dst: dst.clone(),
        });

        // Jump over the false branch
        instr.push(Instr::Jump(end_label.clone()));

        instr.push(Instr::Label(false_label));
        instr.push(Instr::Copy {
            src: WackValue::Constant(WackConst::Bool(0)),
            dst: dst.clone(),
        });
        instr.push(Instr::Label(end_label));

        dst
    }
}
