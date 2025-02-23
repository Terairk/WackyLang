use std::collections::HashMap;

use syntax::ast::{
    ArrayElem, Expr, Func, FuncParam, Ident, LValue, Liter, PairElem, Program, RValue, Stat,
    StatBlock, UnaryOper,
};
use syntax::rename::RenamedName;
use syntax::typecheck::TypeResolver;
use syntax::{rename::IdFuncTable, types::SemanticType};

use crate::wackir::{
    ConvertToMidIdent as _, MidIdent, UnaryOperator, WackFunction, WackInstruction, WackProgram,
    WackValue,
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

impl Lowerer {
    const fn counter(&self) -> usize {
        self.counter
    }
}

fn rename_function(ident: &Ident) -> Ident {
    Ident::from_str(&format!("wacc_{}", ident))
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
            let new_id = rename_function(&id);
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
    // Makes a temporary wacky variable
    fn make_temporary(&mut self) -> MidIdent {
        // Eventually we may want to replace temp with a function name
        // for better debugging
        let ident = Ident::from_str("temp");
        ident.to_mid_ident(&mut self.counter)
    }

    fn lower_func(&mut self, func: TypedFunc) -> WackFunction {
        let name = func.name.into_inner();
        // TODO: Figure out how/when to take in a list of instructions as parameter
        let params = func
            .params
            .into_iter()
            .map(|param| self.lower_func_param(param))
            .collect();

        let mut instructions: Vec<WackInstruction> = Vec::new();
        self.lower_stat_block(func.body, &mut instructions);
        WackFunction {
            name,
            params,
            body: instructions,
        }
    }

    /* TODO: find where we care about the types of params */
    fn lower_func_param(&mut self, param: TypedFuncParam) -> MidIdent {
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
            } => panic!("VarDefinition not implemented in Wacky"),
            TypedStat::Assignment { lvalue, rvalue } => {
                panic!("Assignment not implemented in Wacky")
            }
            TypedStat::Read(lvalue) => panic!("Read not implemented in Wacky"),
            TypedStat::Free(expr) => panic!("Write not implemented in Wacky"),
            TypedStat::Return(expr) => {
                // TODO: look into if we need the type
                // let sem_type = expr.inner().get_type();
                let value = self.lower_expr(expr.into_inner(), instructions);
                let instr = WackInstruction::Return(value);
                instructions.push(instr);
            }
            TypedStat::Exit(expr) => panic!("Exit not implemented in Wacky"),
            TypedStat::Print(expr) => panic!("Print not implemented in Wacky"),
            TypedStat::Println(expr) => panic!("Println not implemented in Wacky"),
            TypedStat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => panic!("If not implemented in Wacky"),
            TypedStat::WhileDo { while_cond, body } => panic!("While not implemented in Wacky"),
            // Not sure if scoped is implemented correctly here
            // TODO: check this later
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
            TypedExpr::Ident(sn_ident, t) => panic!("Ident not implemented in Wacky"),
            TypedExpr::ArrayElem(array_elem, t) => panic!("ArrayElem not implem in Wacky"),
            TypedExpr::Unary(sn_unary, sn_expr, _t) => {
                self.lower_unary(sn_unary.into_inner(), sn_expr.into_inner(), instructions)
            }
            TypedExpr::Binary(sn_expr1, sn_binop, sn_expr2, t) => {
                panic!("Binary not implemented in Wacky")
            }
            TypedExpr::Paren(sn_expr, _t) => self.lower_expr(sn_expr.into_inner(), instructions),
            TypedExpr::Error(_) => panic!("Bug somewhere in frontend."),
        }
    }

    fn lower_rvalue(&mut self, rvalue: TypedRValue, instructions: &mut Vec<WackInstruction>) {
        match rvalue {
            TypedRValue::Expr(_, _) => unimplemented!(),
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
                    dst,
                };
                instructions.push(instr);
            }
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
}
