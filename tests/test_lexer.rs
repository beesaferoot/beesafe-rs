use beesafe::lexer::{Lexer, TType, Token};

#[test]
fn test_next_token() {
    let input = "declare x
   // comment here
   declare y
   for i in 1..4 {}
   // trailing comment";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.parse_tokens();
    println!("{:?} ", tokens);
    assert_eq!(
        tokens.get(0),
        Some(&Token::from("declare", TType::Declare, 0))
    );
    assert_eq!(tokens.get(1), Some(&Token::from("x", TType::Id, 8)));
    assert_eq!(tokens.get(2), Some(&Token::from("\n", TType::Newline, 9)));
    assert_eq!(tokens.get(3), Some(&Token::from("\n", TType::Newline, 28)));
    assert_eq!(tokens.get(4), Some(&Token::from("declare", TType::Declare, 32)));
    assert_eq!(tokens.get(5), Some(&Token::from("y", TType::Id, 40)));
    assert_eq!(tokens.get(6), Some(&Token::from("\n", TType::Newline, 41)));
    assert_eq!(tokens.get(7), Some(&Token::from("for", TType::For, 45)));
    assert_eq!(tokens.get(8), Some(&Token::from("i", TType::Id, 49)));
    assert_eq!(tokens.get(9), Some(&Token::from("in", TType::In, 51)));
    assert_eq!(tokens.get(10), Some(&Token::from("1", TType::Num, 54)));
    assert_eq!(tokens.get(11), Some(&Token::from("..", TType::Range, 55)));
    assert_eq!(tokens.get(12), Some(&Token::from("4", TType::Num, 57)));
    assert_eq!(tokens.get(13), Some(&Token::from("{", TType::Lbrace, 59)));
    assert_eq!(tokens.get(14), Some(&Token::from("}", TType::Rbrace, 60)));
}
