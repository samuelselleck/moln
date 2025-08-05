use std::error::Error;

use moln::{Parser, SemanticContext};

fn main() -> Result<(), Box<dyn Error>> {
    // read a files source
    let file_name = std::env::args()
        .skip(1)
        .next()
        .unwrap_or("test_files/test_file.ln".to_owned());
    let source = std::fs::read_to_string(&file_name).unwrap();

    // parse it into a vadermoln AST
    let ast = Parser::new(&source)
        .vadermoln()
        .and_then(|ast| SemanticContext::new().semantic_analysis(ast));

    // print results
    println!("-------DONE-------");
    match ast {
        Ok(ast) => println!("parsed AST: {:#?}", ast),
        Err(e) => e.print_with_file(&file_name, &source)?,
    };
    Ok(())
}
