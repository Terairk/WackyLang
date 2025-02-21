use middle::wackir::{WackConst, WackInstruction, WackProgram, WackValue};

use crate::{
    assembly_ast::{AsmFunction, AsmInstruction, AsmProgram, Operand},
    gen_flags::GenFlags,
};

/* ================== PUBLIC API ================== */

#[inline]
pub fn tacky_to_assembly(program: WackProgram) -> AsmProgram {
    let wack_functions = program.top_level;
    let main_wack_function = program.body;
    unimplemented!()
}

/* ================== INTERNAL API ================== */

struct AsmGen {
    counter: usize,
    gen_flags: GenFlags,
    // TODO: add counter for string literals
    // and a table to store these literals with their lengths
    // we need to mangle them as well
    // also maybe keep track of RIP relative addressing

    // Probably need a backend symbol table
    // honestly we probably don't need a symbol table for functions
    // as we dont have external functions other than the ones used by
    // Wacc's standard statements like read, print etc
    // So we automatically know if we should use PLT or not
}

impl AsmGen {
    fn new(counter: usize) -> Self {
        AsmGen {
            counter,
            gen_flags: GenFlags::empty(),
        }
    }

    fn lower_main_asm(&mut self, instrs: Vec<WackInstruction>) -> AsmFunction {
        let mut asm_instructions = Vec::new();
        for wack_instr in instrs {
            self.lower_instruction(wack_instr, &mut asm_instructions);
        }

        asm_instructions.push(AsmInstruction::Ret);

        AsmFunction {
            name: "main".to_owned(),
            global: true,
            external: false,
            instructions: asm_instructions,
        }
    }

    fn lower_instruction(
        &mut self,
        instr: WackInstruction,
        asm_instructions: &mut Vec<AsmInstruction>,
    ) {
        use AsmInstruction as Asm;
        // TODO: finish this scaffolding
        unimplemented!()
        // match instr {
        //     Return(value) => self.lower_return(value, asm_instructions),
        // }
    }

    fn lower_return(&mut self, value: WackValue, asm_instructions: &mut Vec<AsmInstruction>) {
        use AsmInstruction as Asm;
        let value = self.lower_value(value, asm_instructions);
        // TODO: add return here
        unimplemented!()
    }

    // This lowers a WackValue to an Asm Operand
    fn lower_value(&mut self, value: WackValue, asm_instructions: &mut Vec<AsmInstruction>) {
        use WackConst::*;
        use WackValue::*;
        // TODO: find out where to convert these to an int
        let instr = match value {
            Constant(Int(i)) => Operand::Imm(i),
            Constant(Bool(b)) => Operand::Imm(b),
            Constant(Char(c)) => Operand::Imm(c),
            // TODO: StringLit, need to add to symbol table with current function probably
            // while keeping track of a unique counter just for string constants
            // so we can emit properly, also this should emit a thing for RIP relative addressing
            Constant(StringLit(s)) => unimplemented!(),
            Constant(NullPair) => Operand::Imm(0),
            // TODO: need to figure out if its a Scalar or Aggregate Value
            // so I can do either Pseudo or PseudoMem
            Var(ident) => unimplemented!(),
        };
    }
}
