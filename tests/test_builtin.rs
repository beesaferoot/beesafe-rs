use beesafe::symbols::Object;
use beesafe::{environment, executor, lexer, parser};

#[test]
fn test_print_builtin_returns_null() {
    let input = r#"
        print("a", 1, null, true)
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let mut env = environment::Environment::new();
    let mut exec = executor::Executor::new(&mut env, &parser);
    let results = exec.visit_program(&program);
    match results.last().unwrap().as_ref() {
        Object::Null => (),
        other => panic!("expected Null from print, got {:?}", other),
    }
}
