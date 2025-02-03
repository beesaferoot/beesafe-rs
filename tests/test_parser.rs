use beesafe::ast::Node;
use beesafe::ast::NodeType;
use beesafe::lexer;
use beesafe::parser;

#[test]
fn test_expression() {
    let input = r#"
        1 + 1 
        2 - 3 + 5 
        3 / 5 
        8 * 9
        -9
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 5);

    for stmt in program.statements {
        match stmt {
            Node::BinaryOp(op) => match op.ttype() {
                NodeType::Add => (),
                NodeType::Mul => (),
                NodeType::Sub => (),
                NodeType::Div => (),
                _ => assert!(true, "not recognized"),
            },
            Node::UniaryOp(op) => match op.ttype() {
                NodeType::USub => (),
                _ => assert!(true, "not recognized"),
            },
            _ => (),
        }
    }
    assert_eq!(parser.has_errors(), false);
}

#[test]
fn test_for_int_range_stmt() {
    let input = r#"
    for i in 1..4 {}
"#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 1);
}

#[test]
fn test_error() {
    let input = r#"
    3 // 4
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 1);
    assert_eq!(parser.has_errors(), true);
}
