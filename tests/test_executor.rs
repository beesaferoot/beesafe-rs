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

    let mut exec = executor::Executor::new(env.clone(), &parser);

    let mut global_scope = env.clone();

    let result = match exec.visit_expr(&program.statements[0], global_scope.as_mut()).as_ref() {
        Object::Number(num) => Some(*num),
        _ => None,
    };
    assert_eq!(result, Some(4 as i32))
}

#[test]
fn test_for_over_range_exec() {
    let input = r#"
        for i in (1..3) { }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let env = Box::new(environment::Environment::new());
    let mut exec = executor::Executor::new(env, &parser);
    let results = exec.visit_program(&program);
    assert_eq!(results.len(), 1);
    match results[0].as_ref() {
        Object::Null => (),
        _ => panic!("expected Null from for-loop execution"),
    }
}

#[test]
fn test_for_over_array_exec() {
    let input = r#"
        for i in [1, 2] { }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let env = Box::new(environment::Environment::new());
    let mut exec = executor::Executor::new(env, &parser);
    let results = exec.visit_program(&program);
    assert_eq!(results.len(), 1);
    match results[0].as_ref() {
        Object::Null => (),
        _ => panic!("expected Null from for-loop execution"),
    }
}

#[test]
fn test_for_over_string_exec() {
    let input = r#"
        for ch in "ab" { }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let env = Box::new(environment::Environment::new());
    let mut exec = executor::Executor::new(env, &parser);
    let results = exec.visit_program(&program);
    assert_eq!(results.len(), 1);
    match results[0].as_ref() {
        Object::Null => (),
        _ => panic!("expected Null from for-loop execution"),
    }
}

#[test]
fn test_for_non_iterable_error() {
    let input = r#"
        for i in 1 { }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let env = Box::new(environment::Environment::new());
    let mut exec = executor::Executor::new(env, &parser);
    let results = exec.visit_program(&program);
    assert_eq!(results.len(), 1);
    match results[0].as_ref() {
        Object::Error(_) => (),
        _ => panic!("expected Error when iterating a non-iterable"),
    }
}

#[test]
fn test_comparisons_exec() {
    let input = r#"
        1 < 2
        2 <= 2
        3 > 1
        3 >= 3
        4 == 4
        5 != 6
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let env = Box::new(environment::Environment::new());
    let mut exec = executor::Executor::new(env, &parser);
    let results = exec.visit_program(&program);
    let bools: Vec<bool> = results
        .into_iter()
        .map(|c| match c.as_ref() {
            Object::Bool(b) => *b,
            _ => false,
        })
        .collect();
    assert_eq!(bools, vec![true, true, true, true, true, true]);
}

#[test]
fn test_declare_init_ident_and_if_while() {
    let input = r#"
        declare x, y
        init x = 1
        init y = 0
        if (x > 0) {
            init y = 2
        } else {
            init y = 3
        }
        while (y > 0) { init y = y - 1 }
        y
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let env = Box::new(environment::Environment::new());
    let mut exec = executor::Executor::new(env, &parser);
    let results = exec.visit_program(&program);
    match results.last().unwrap().as_ref() {
        Object::Number(n) => assert_eq!(*n, 0),
        other => panic!("expected final y to be 0, got {:?}", other),
    }
}

#[test]
fn test_function_define_and_call() {
    let input = r#"
        define add(a, b){
            return a + b
        }
        add(2, 3)
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let env = Box::new(environment::Environment::new());
    let mut exec = executor::Executor::new(env, &parser);
    let results = exec.visit_program(&program);
    match results.last().unwrap().as_ref() {
        Object::Number(n) => assert_eq!(*n, 5),
        other => panic!("expected 5, got {:?}", other),
    }
}

#[test]
fn test_function_expression_and_call() {
    let input = r#"
        init a = define(){
            return "flower"
        }()
        a
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());

    let env = Box::new(environment::Environment::new());
    let mut exec = executor::Executor::new(env, &parser);
    let results = exec.visit_program(&program);
    match results.last().unwrap().as_ref() {
        Object::String(s) => assert_eq!(*s, "flower"),
        other => panic!("expected 'flower', got {:?}", other),
    }
}