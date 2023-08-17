use beesafe::ast::Node;
use beesafe::ast::NodeType;
use beesafe::parser;
use beesafe::lexer;

#[test]
fn test_expression(){
    let input_string = String::from(r#"
        1 + 2 - 4 
        7 * 4 
        9 - 0 
        8 * 9 + 5 
        3 + 4
    "#);
    let mut lexer = lexer::Lexer::new(input_string);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 5);


    for stmt in program.statements {
        match stmt {
            Node::BinaryOp(op) => {
                match op.ttype() {
                    NodeType::Add => (),
                    NodeType::Mul => (), 
                    NodeType::Sub => (), 
                    NodeType::Div => (),
                    _ => assert!(true, "not recognized")
                }
            }
            _ => ()
        }
    }
    assert_eq!(parser.has_errors(), false);
}

#[test]
fn test_error(){
    let input_string = String::from(r#"
    3 / 4
    "#);
    let mut lexer = lexer::Lexer::new(input_string);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 0);
    assert_eq!(parser.has_errors(), true);
}