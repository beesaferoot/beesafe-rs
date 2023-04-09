/*

Lexer module: contains code for tokenization phase.

*/


use std::collections::HashMap;


pub struct Lexer {
    source: String, 
    read_position: i32,  
    keywords:  HashMap<String, TType>,
    line_no: i32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub lexeme: String,
    pub ttype: TType,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum TType {
    Num, 
    Id,
    True, 
    False, 
    Eq, 
    NotEq, 
    Gt,
    Lt,
    LtEq,
    GtEq,
    Assign,
    Plus, 
    UMinus,
    Minus,
    Asterisk,
    Div,
    Define,
    Declare,
    Init,
    Function,
    Newline,
    Eob, 
    Invalid,
    Lbrace, 
    Rbrace,
    Lparen,
    Rparen,
    If,
    Else,
    For,
    Return,
    Null,
    Error,
    Comma,
    Literal,
    Bang,
}

impl Token {

    pub fn from(lexeme: &str, ttype: TType) -> Self {
        Self { lexeme: lexeme.to_string(), ttype: ttype }
    }

    fn from_char(lexeme: char, ttype: TType) -> Self {
        Self { lexeme: lexeme.to_string(), ttype: ttype }
    }
}

impl Lexer {
    
    pub fn new(input_src: String) -> Self {
        Lexer { 
            source: input_src, 
            read_position: 0, 
            line_no: 1, 
            keywords: Self::create_reserved(),
        }
    }

    pub fn lineno(&self) -> i32 {
        self.line_no
    }

    pub fn parse_tokens(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        let mut current_token: Token = self.next_token();
        while current_token.ttype != TType::Eob {
            tokens.push(current_token);
            current_token = self.next_token();
        }
        tokens.push(current_token);
        tokens
    }

    // return next available token
    pub fn next_token(&mut self) -> Token {
        let ch = self.read_char_no_withspace();
        match ch {
            '\0' => Token {lexeme: ch.to_string(), ttype: TType::Eob },
            '\n' => { 
                self.line_no += 1;
                Token {lexeme: ch.to_string(), ttype: TType::Newline} 
            },
            '+' => Token { lexeme: ch.to_string(), ttype: TType::Plus },
            '-' => Token { lexeme: ch.to_string(), ttype: TType::Minus },
            '*' => Token { lexeme: ch.to_string(), ttype: TType::Asterisk },
            '/' => Token { lexeme: ch.to_string(), ttype: TType::Div },
            '=' =>  { 
                if self.peek_char() == '=' {
                    let mut op = String::from(ch);
                    op.push(self.read_char());
                    return Token { lexeme: op, ttype: TType::Eq}
                }
                Token::from_char(ch, TType::Assign)
            },
            ',' => Token { lexeme: ch.to_string(), ttype: TType::Comma },
            '{' => Token { lexeme: ch.to_string(), ttype: TType::Lbrace },
            '}' => Token { lexeme: ch.to_string(), ttype: TType::Rbrace },
            '(' => Token { lexeme: ch.to_string(), ttype: TType::Lparen },
            ')' => Token { lexeme: ch.to_string(), ttype: TType::Rparen },
            '<' => {
                if self.peek_char() == '=' {
                    let mut op = String::from(ch);
                    op.push(self.read_char());
                    return Token { lexeme: op, ttype: TType::LtEq}
                }
                Token::from_char(ch, TType::Lt)
            },
            '>' => {
                if self.peek_char() == '=' {
                    let mut op = String::from(ch);
                    op.push(self.read_char());
                    return Token { lexeme: op, ttype: TType::GtEq}
                } 
                Token::from_char(ch, TType::Gt)
            }, 
            '!' => {
               if self.peek_char() == '=' {
                let mut op = String::from(ch);
                op.push(self.read_char());
                return Token { lexeme: op, ttype: TType::NotEq}
               }
               Token::from_char(ch, TType::Bang)
            },
            _ => {
                if ch.is_alphabetic() || ch == '_' {
                    self.read_identifer(ch)
                } else if ch.is_numeric() {
                    self.read_number(ch)
                } else if ch == '\'' || ch == '\"' {
                    self.read_string_literal(ch)
                } else {
                    Token::from_char(ch, TType::Invalid)
                }
            }
        }
    }

    fn peek_char(& self) -> char {
        let pos = self.read_position as usize;
        if self.source.len() > pos {
           let s = &self.source[pos..];
           return s.chars().next().unwrap_or('\0');
        }
        '\0'
    }

    fn read_char(&mut self) -> char {
        let pos = self.read_position as usize;
        let s = &self.source[pos..];
        match s.chars().next() {
            Some(ch) => {
                self.read_position += 1;
                ch
            },
            None => return '\0'
        }
    }
    

    fn read_char_no_withspace(&mut self) -> char {
        let mut ch = self.read_char();
        while ch.is_whitespace() && ch != '\n' {
            ch = self.read_char();
        }
        ch
    }

    fn read_identifer(&mut self, ch: char) -> Token {
        let mut ident = String::from(ch);
        let mut look_ahead: char;
        while self.peek_char().is_alphanumeric() || self.peek_char() == '_' {
            look_ahead = self.read_char();
            ident.push(look_ahead);
        }

        match self.keywords.get(&ident) {
            Some(ttype) => return Token { lexeme: ident, ttype: *ttype},
            None => ()
        }
        Token { lexeme: ident, ttype: TType::Id }

    }

    fn read_number(&mut self, ch: char) -> Token {
        let mut num_literal = String::from(ch);
        let mut look_ahead: char;
        while self.peek_char().is_numeric() {
            look_ahead = self.read_char();
            num_literal.push(look_ahead);
        }
        Token { lexeme: num_literal, ttype: TType::Num }
    }

    fn read_string_literal(&mut self, ch: char) -> Token {
        let mut str_literal = String::from("");

        while self.peek_char() != ch {
            str_literal.push(self.read_char())
        }
        // move read position one step forward to cover the closing quote 
        self.read_char();
        Token { lexeme: str_literal, ttype: TType::Literal }

    }

    // create table for keywords
    fn create_reserved() ->  HashMap<String, TType> {
        let keywords: HashMap<String,TType> = HashMap::from([
            ("true".to_string(), TType::True), 
            ("false".to_string(), TType::False),
            ("init".to_string(), TType::Init),
            ("if".to_string(), TType::If),
            ("else".to_string(), TType::Else), 
            ("for".to_string(), TType::For),
            ("return".to_string(), TType::Return), 
            ("null".to_string(), TType::Null),
            ("declare".to_string(), TType::Declare),
            ("define".to_string(), TType::Define),
        ]);
        keywords
    }
}
