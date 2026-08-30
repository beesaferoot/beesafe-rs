use beesafe::symbols::Object;
use beesafe::runtime::Runtime;
use beesafe::source::SourceMap;
use beesafe::{executor, lexer, parser};

#[test]
fn test_print_builtin_returns_null() {
    let input = r#"
        print("a", 1, null, true)
    "#;
    let mut sources = SourceMap::new();
    let source = sources.add(input.to_string());
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer, source);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let mut runtime = Runtime::new();
    let mut exec = executor::Executor::new(&mut runtime, source);
    let results = exec.visit_program(&program);
    match exec.read_view(*results.last().unwrap()).unwrap() {
        Object::Null => (),
        other => panic!("expected Null from print, got {:?}", other),
    }
}
