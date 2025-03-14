use internment::Intern;
use middle::optimizations::make_cfg;
use middle::wackir::{WackGlobIdent, WackInstr, WackLiteral, WackTempIdent, WackValue};

fn main() {
    let mut counter = 0;
    let x_ident = WackTempIdent::create_new("x", &mut counter);
    let target_ident = WackTempIdent::create_new("Target", &mut counter);

    let wack_instructions: Vec<WackInstr> = vec![
        WackInstr::Copy {
            src: WackValue::Literal(WackLiteral::Int(5)),
            dst: x_ident.clone(),
        },
        WackInstr::Jump(target_ident.clone()),
        WackInstr::FunCall {
            fun_name: WackGlobIdent::new(Intern::from("my_function")),
            args: vec![],
            dst: x_ident.clone(),
        },
        WackInstr::Label(target_ident),
        WackInstr::Return(WackValue::Var(x_ident)),
    ];
    let cfg = make_cfg(wack_instructions, "function_name");
    // let cfg = eliminate_unreachable_code(cfg);

    // Print CFG visualization
    let mut counter = 0;
    if let Ok(png_path) = cfg.print_graphviz(&mut counter) {
        println!("Generated CFG visualization: {png_path}");
    }
}
