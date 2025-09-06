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
    for i in (1..4) {}
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
    assert_eq!(parser.has_errors(), false);
}

#[test]
fn test_function_calls() {
    let input = r#"
        init a = define(x){ x + 2}(2)
        add(2, 2*4)
        sqrt(2, 3)
        ((3*8) - (1 + power(2, 3)))
        init a = define(){
            return "flower"
        }()
        entry("headers")
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 6);
    assert!(!parser.has_errors());
}

#[test]
fn test_function_statements() {
    let input = r#"
        define subtext(){
            return 'hello world'
        }
        define entry(){
            declare text
            text = 6
            init subval = 6
        }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 2);
    assert!(!parser.has_errors());
}

#[test]
fn test_comments_in_complex_code() {
    let input = r#"
        // leading comment
        define compute(a, b){ // inline after signature
            // inside block
            declare x, y // declare vars
            init x = a + b // sum
            if (x > 0) { // positive branch
                init y = x * 2 // double
            } else {
                // negative or zero
                init y = 0
            }
            while (y > 0) { // countdown
                init y = y - 1
            }
        }
        // trailing comment after function
        compute(1, 2) // call with comments
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(!parser.has_errors());
    assert_eq!(program.statements.len(), 2);
}

#[test]
fn test_function_literals() {
    let input = r#"
        init fn = define (){}
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 1);
    assert!(!parser.has_errors());
}

#[test]
fn test_return_statements() {
    let input = r#"
        return (1+1)
        return 1 - 9
        return 3
        return 8 * 9
        return 4 / 2
        return (1 > 9)
        return (1 != 0)
        return (9 == 9)
        return (20 >= 100)
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 9);
    assert!(!parser.has_errors());
}

#[test]
fn test_if_statements() {
    let input = r#"
        if(1 >= 1){
        }else{
        }
        if(0 == 0){
        }
        if(0 < 0){
        }
        if(19 > 18){
        }
        if(17 != 90){
        }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 5);
    assert!(!parser.has_errors());
}

#[test]
fn test_relational_and_equality_expressions() {
    let input = r#"
        1 < 2
        1 <= 2
        1 > 2
        1 >= 2
        1 == 2
        1 != 2
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 6);

    let expected = [
        NodeType::Lt,
        NodeType::LtEq,
        NodeType::Gt,
        NodeType::GtEq,
        NodeType::Eq,
        NodeType::NotEq,
    ];

    for (stmt, et) in program.statements.iter().zip(expected.iter()) {
        verify_node_type(stmt, et);
    }

    assert!(!parser.has_errors());
}

#[test]
fn test_comparison_precedence() {
    let input = r#"
        (1 + 2) > 2
        3 * 3 == 9
        10 - 5 <= 4
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 3);

    let expected = [NodeType::Gt, NodeType::Eq, NodeType::LtEq];
    for (stmt, et) in program.statements.iter().zip(expected.iter()) {
        verify_node_type(stmt, et);
    }

    assert!(!parser.has_errors());
}

#[test]
fn test_declare_statements() {
    let input = r#"
        declare x
        declare x, y
        declare x, y, z
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 3);
    assert!(!parser.has_errors());
}

#[test]
fn test_complex_expressions() {
    let input = r#"
        (7 * (3 + 4 - 4))
        -2
        (-3)
        (6 + (8+9) - 8)
        3 * 8 + (9 + 0)
        (9) * (8-9)
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 6);
    assert!(!parser.has_errors());
}

#[test]
fn test_complex_statements() {
    let input = r#"
        declare x, y
        init x = 9
        init z = 1 + (3 + 2)
        for a in (0..3){
            init o = 0
            declare p
        }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 4);
    parser.show_errors();
    assert!(!parser.has_errors());
}

#[test]
fn test_syntax_errors() {
    let input = r#"
        (-
        (]
        -=
        =-
        ++
        --
        /)
        -+
        )-
        --
        !)
        (
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert!(parser.has_errors());
    assert_eq!(program.statements.len(), 0);
}

#[test]
fn test_incomplete_blocks() {
    let input = r#"
        for i in (3..4){
            if(i<4){
                return i
            }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 1);
    assert!(parser.has_errors());
}

#[test]
fn test_function_calls_in_expressions() {
    let input = r#"
        add(2, 3) + 5
        multiply(subtract(10, 5), 2)
        (get_value() + 5) * 2
        init result = calculate(1, 2, 3)
        if (is_valid(x)) { }
        while (has_next()) { }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 6);
    parser.show_errors();
    assert!(!parser.has_errors());
}

#[test]
fn test_not_operator() {

    let input = r#"
        !true
        !false
        !(1 == 2)
        !(1 < 2)
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 4);
    assert!(!parser.has_errors());
}

#[test]
fn test_iterable_protocols() {
    let input = r#"
        for i in x { }
        for i in getItems() { }
        for i in [1, 2, 3] { }
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    
    assert_eq!(program.statements.len(), 3);

    for stmt in &program.statements {
        match stmt {
            Node::For(_) => (),
            _ => panic!("Expected For node, got {:?}", stmt),
        }
    }

    assert!(!parser.has_errors());
}

#[test]
fn test_array_literals() {
    let input = r#"
        [1, 2, 3]
        []
    "#;
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 2);
    
    for stmt in &program.statements {
        match stmt {
            Node::Array(_) => (),
            _ => panic!("Expected Array node, got {:?}", stmt),
        }
    }

    assert!(!parser.has_errors());
}

fn verify_node_type(node: &Node, expected_type: &NodeType) {
    match node {
        Node::BinaryOp(op) => assert_eq!(op.ttype(), *expected_type),
        Node::UniaryOp(op) => assert_eq!(op.ttype(), *expected_type),
        Node::Number(_) => assert_eq!(NodeType::Number, *expected_type),
        Node::Boolean(_) => assert_eq!(NodeType::Boolean, *expected_type),
        Node::Null(_) => assert_eq!(NodeType::Null, *expected_type),
        Node::StringLiteral(_) => assert_eq!(NodeType::StringLiteral, *expected_type),
        Node::Ident(_) => assert_eq!(NodeType::Identifier, *expected_type),
        Node::If(_) => assert_eq!(NodeType::If, *expected_type),
        Node::While(_) => assert_eq!(NodeType::While, *expected_type),
        Node::For(_) => assert_eq!(NodeType::For, *expected_type),
        Node::Function(_) => assert_eq!(NodeType::Function, *expected_type),
        Node::FunctionExpr(_) => assert_eq!(NodeType::FunctionExpr, *expected_type),
        Node::Call(_) => assert_eq!(NodeType::Call, *expected_type),
        Node::Declare(_) => assert_eq!(NodeType::Declare, *expected_type),
        Node::Init(_) => assert_eq!(NodeType::Init, *expected_type),
        Node::Return(_) => assert_eq!(NodeType::Return, *expected_type),
        Node::Block(_) => assert_eq!(NodeType::Block, *expected_type),
        Node::Range(_) => assert_eq!(NodeType::Range, *expected_type),
        Node::Array(_) => assert_eq!(NodeType::Array, *expected_type),
        Node::Error(_) => assert_eq!(NodeType::Err, *expected_type),
        _ => panic!("Unexpected node type"),
    }
}
