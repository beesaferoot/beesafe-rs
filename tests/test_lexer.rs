use beesafe::lexer::{Lexer, Token, TType};

#[test]
fn test_next_token() {
   let input_string = String::from("declare x
   declare y");
   let mut lexer = Lexer::new(input_string);
   let tokens = lexer.parse_tokens();
   assert_eq!(tokens.get(0), Some(&Token::from("declare", TType::Declare, 0)));
   assert_eq!(tokens.get(1), Some(&Token::from("x", TType::Id, 8)));
   assert_eq!(tokens.get(2), Some(&Token::from("\n", TType::Newline, 9)));
   assert_eq!(tokens.get(3), Some(&Token::from("declare", TType::Declare, 13)));
   assert_eq!(tokens.get(4), Some(&Token::from("y", TType::Id, 21)));
}
