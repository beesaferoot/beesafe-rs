use beesafe::environment;
use beesafe::executor;
use beesafe::lexer;
use beesafe::parser;
use beesafe::symbols::Object;

#[test]
fn test_add_expression_eval() {
    let input_string = "1 + 3";
    let mut lexer = lexer::Lexer::new(input_string);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    let env = Box::new(environment::Environment::new());

    let mut exec = executor::Executor::new(env, &parser);

    let result = match exec.visit_expr(&program.statements[0]).as_ref() {
        Object::Number(num) => Some(*num),
        _ => None,
    };
    assert_eq!(result, Some(4 as i32))
}
