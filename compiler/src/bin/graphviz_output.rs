use internment::ArcIntern;
use middle::wackir::{WackGlobIdent, WackInstr, WackLiteral, WackTempIdent, WackValue};
use util::CFG;

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
            fun_name: WackGlobIdent::new(ArcIntern::from("my_function")),
            args: vec![],
            dst: x_ident.clone(),
        },
        WackInstr::Label(target_ident.clone()),
        WackInstr::Return(WackValue::Var(x_ident)),
    ];
    let cfg =
        CFG::<WackInstr, ()>::from_instructions("function_name".to_string(), &wack_instructions);

    // Print CFG visualization
    if let Ok(png_path) = cfg.print_graphviz() {
        println!("Generated CFG visualization: {}", png_path);
    }
}
