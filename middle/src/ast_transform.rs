use std::collections::HashMap;

use syntax::ast::{
    ArrayElem, Expr, Func, FuncParam, LValue, Liter, PairElem, Program, RValue, Stat, StatBlock,
};
use syntax::rename::RenamedName;
use syntax::typecheck::TypeResolver;
use syntax::{rename::IdFuncTable, types::SemanticType};

use crate::wackir::{ConvertToMidIdent as _, Instruction, MidIdent, TopLevel, Value, WackProgram};

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
pub fn lower_program(program: TypedAST, type_resolver: TypeResolver) -> WackProgram {
    let context = create_lowering_context(type_resolver);
    unimplemented!();
    // let mut wack_program = WackProgram::Program {
    //     top_level: Vec::new(),
    //     body: Vec::new(),
    // };
    // for func in program.funcs.iter() {
    //     let top_level = lower_func(func, &context);
    //     wack_program.top_level.push(top_level);
    // }
    // wack_program
}

/* ================== INTERNAL API ================== */
struct Lowerer {
    counter: usize,
    func_table: IdFuncTable,
    symbol_table: HashMap<MidIdent, SemanticType>,
}

fn create_lowering_context(type_resolver: TypeResolver) -> Lowerer {
    let renamer = type_resolver.renamer;
    let mut counter = renamer.counter();
    let func_table = renamer.id_func_table;
    let symbol_table: HashMap<MidIdent, SemanticType> = type_resolver
        .symid_table
        .into_iter()
        .map(|(id, symid)| (id.to_mid_ident(&mut counter), symid))
        .collect();
    Lowerer {
        counter,
        func_table,
        symbol_table,
    }
}

impl Lowerer {
    fn lower_func(&mut self, func: TypedFunc) -> TopLevel {
        // TODO: Figure out how/when to take in a list of instructions as parameter
        let name = func.name.to_mid_ident(&mut self.counter);
        let params = func
            .params
            .into_iter()
            .map(|param| self.lower_func_param(param))
            .collect();

        let mut instructions: Vec<Instruction> = Vec::new();
        self.lower_stat_block(func.body, &mut instructions);
        TopLevel::Function {
            name,
            params,
            body: instructions,
        }
    }

    fn lower_func_param(&mut self, param: TypedFuncParam) -> MidIdent {
        param.name.to_mid_ident(&mut self.counter)
    }

    fn lower_stat_block(
        &mut self,
        stat_block: TypedStatBlock,
        instructions: &mut Vec<Instruction>,
    ) -> Vec<Instruction> {
        unimplemented!();
    }

    // TODO: check this return type later
    fn lower_expr(&mut self, expr: TypedExpr, instructions: &mut Vec<Instruction>) -> Value {
        match expr {
            TypedExpr::Liter(liter, t) => self.lower_literal(liter, t, instructions),
            TypedExpr::Ident(sn_ident, t) => panic!("Ident not implemented in Wacky"),
            TypedExpr::ArrayElem(array_elem, t) => panic!("ArrayElem not implem in Wacky"),
            TypedExpr::Unary(sn_unary, sn_expr, t) => todo!(),
            TypedExpr::Binary(sn_expr1, sn_binop, sn_expr2, t) => {
                panic!("Binary not implemented in Wacky")
            }
            TypedExpr::Paren(sn_expr, t) => panic!("Paren not implemented in Wacky"),
            TypedExpr::Error(_) => panic!("Bug somewhere in frontend."),
        }
    }

    fn lower_literal(
        &mut self,
        liter: Liter,
        t: SemanticType,
        instructions: &mut Vec<Instruction>,
    ) -> Value {
        match liter {
            Liter::IntLiter(i) => unimplemented!(),
            Liter::BoolLiter(b) => unimplemented!(),
            Liter::CharLiter(c) => unimplemented!(),
            Liter::StrLiter(s) => unimplemented!(),
            Liter::PairLiter => panic!("wtf is this"),
        }
    }
}
