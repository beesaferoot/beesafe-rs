use beesafe::symbols::Object;
use beesafe::parser;
use beesafe::lexer;
use beesafe::executor;
use beesafe::environment;

#[test]
fn test_add_expression_eval(){
    let input_string = String::from("1 + 3");
    let mut lexer = lexer::Lexer::new(input_string);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    let env = Box::new(environment::Environment::new());

    let exec = executor::Executor::new(env);
    
    let result = match exec.visit_expr(&program.statements[0]).as_ref() {
        Object::NumberObj(num) => Some(num.value),
        _ => None
    };
    assert_eq!(result, Some(4))
}