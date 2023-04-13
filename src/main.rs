use beesafe::lexer;
use beesafe::parser::Parser;

fn main() {
    // let input_string = r#"
    //     return
    //     {}
    //     if 
    //     variable
    //     +
    //     - 
    //     /
    //     *
    //     "string literal"
    //     'single quotes'
    //     1 + 1
    //     a = 0
    // "#;
    let input_string = r#"
        1 + 2 - 5
        7 * 4 
        9 - 0 
        8 * 9 + 5 
        93 + 4
    "#;
    let source_input = String::from(input_string);
    let mut lexer = lexer::Lexer::new(source_input);
    // let tokens = lexer.parse_tokens();
    // println!("{:?}", tokens);
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    if parser.has_errors() {
        parser.show_errors();
        return;
    }
    for node in &program.statements {
        println!("{:?}", node.visit().visit().downcast::<i32>().unwrap_or(Box::new(-1)));
    }
}
