/*

Lexer module: contains code for tokenization phase.

*/

use std::collections::HashMap;

pub struct Lexer {
    source: &'static str,
    read_position: i32,
    keywords: HashMap<String, TType>,
    line_no: i32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub lexeme: String,
    pub ttype: TType,
    pub offset: i32,
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
    Minus,
    Asterisk,
    Div,
    Define,
    Declare,
    Init,
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
    While,
    Return,
    Null,
    Error,
    Comma,
    Literal,
    Bang,
    In,
    Range,
    Lbracket,
    Rbracket,
    Call,
}

impl Token {
    pub fn from(lexeme: &str, ttype: TType, offset: i32) -> Self {
        Self {
            lexeme: lexeme.to_string(),
            ttype: ttype,
            offset,
        }
    }

    fn from_char(lexeme: char, ttype: TType, offset: i32) -> Self {
        Self {
            lexeme: lexeme.to_string(),
            ttype: ttype,
            offset,
        }
    }

}

impl Lexer {
    pub fn new(input_src: &'static str) -> Self {
        Lexer {
            source: input_src,
            read_position: 0,
            line_no: 1,
            keywords: Self::create_reserved(),
        }
    }

    pub fn source(&self) -> &'static str {
        &self.source
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
        let offset = self.read_position - ch.len_utf8() as i32;
        match ch {
            '\0' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Eob,
                offset,
            },
            '\n' => {
                self.line_no += 1;
                Token {
                    lexeme: ch.to_string(),
                    ttype: TType::Newline,
                    offset,
                }
            }
            '+' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Plus,
                offset,
            },
            '-' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Minus,
                offset,
            },
            '*' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Asterisk,
                offset,
            },
            '/' => {
                if self.peek_char() == '/' {
                    self.read_char();
                    let mut next = self.read_char();
                    while next != '\n' && next != '\0' {
                        next = self.read_char();
                    }
                    // The comment ends at the newline, so that is the token's offset.
                    if next == '\n' {
                        self.line_no += 1;
                        return Token::from("\n", TType::Newline, self.read_position - 1);
                    }
                    return Token::from("\0", TType::Eob, self.read_position - 1);
                }
                Token::from_char(ch, TType::Div, offset)
            },
            '=' => {
                if self.peek_char() == '=' {
                    let mut op = String::from(ch);
                    op.push(self.read_char());
                    return Token {
                        lexeme: op,
                        ttype: TType::Eq,
                        offset,
                    };
                }
                Token::from_char(ch, TType::Assign, offset)
            }
            ',' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Comma,
                offset,
            },
            '{' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Lbrace,
                offset,
            },
            '}' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Rbrace,
                offset,
            },
            '(' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Lparen,
                offset,
            },
            ')' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Rparen,
                offset,
            },
            '[' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Lbracket,
                offset,
            },
            ']' => Token {
                lexeme: ch.to_string(),
                ttype: TType::Rbracket,
                offset,
            },
            '<' => {
                if self.peek_char() == '=' {
                    let mut op = String::from(ch);
                    op.push(self.read_char());
                    return Token {
                        lexeme: op,
                        ttype: TType::LtEq,
                        offset,
                    };
                }
                Token::from_char(ch, TType::Lt, offset)
            }
            '>' => {
                if self.peek_char() == '=' {
                    let mut op = String::from(ch);
                    op.push(self.read_char());
                    return Token {
                        lexeme: op,
                        ttype: TType::GtEq,
                        offset,
                    };
                }
                Token::from_char(ch, TType::Gt, offset)
            }
            '!' => {
                if self.peek_char() == '=' {
                    let mut op = String::from(ch);
                    op.push(self.read_char());
                    return Token {
                        lexeme: op,
                        ttype: TType::NotEq,
                        offset,
                    };
                }
                Token::from_char(ch, TType::Bang, offset)
            }
            '.' => {
                if self.peek_char() == '.' {
                    let mut op = String::from(ch);
                    op.push(self.read_char());
                    return Token {
                        lexeme: op,
                        ttype: TType::Range,
                        offset,
                    };
                }
                Token::from_char(ch, TType::Invalid, offset)
            }
            _ => {
                if ch.is_alphabetic() || ch == '_' {
                    self.read_identifer(ch, offset)
                } else if ch.is_numeric() {
                    self.read_number(ch, offset)
                } else if ch == '\'' || ch == '\"' {
                    self.read_string_literal(ch, offset)
                } else {
                    Token::from_char(ch, TType::Invalid, offset)
                }
            }
        }
    }

    fn peek_char(&self) -> char {
        self.rest().chars().next().unwrap_or('\0')
    }

    // read_position is a byte offset, so it advances by the char's utf-8 width.
    fn read_char(&mut self) -> char {
        match self.rest().chars().next() {
            Some(ch) => {
                self.read_position += ch.len_utf8() as i32;
                ch
            }
            None => '\0',
        }
    }

    fn rest(&self) -> &'static str {
        &self.source[self.read_position as usize..]
    }

    fn read_char_no_withspace(&mut self) -> char {
        let mut ch = self.read_char();
        while ch.is_whitespace() && ch != '\n' {
            ch = self.read_char();
        }
        ch
    }

    fn read_identifer(&mut self, ch: char, offset: i32) -> Token {
        let mut ident = String::from(ch);
        let mut look_ahead: char;
        while self.peek_char().is_alphanumeric() || self.peek_char() == '_' {
            look_ahead = self.read_char();
            ident.push(look_ahead);
        }

        match self.keywords.get(&ident) {
            Some(ttype) => {
                return Token {
                    lexeme: ident,
                    ttype: *ttype,
                    offset,
                }
            }
            None => (),
        }
        Token {
            lexeme: ident,
            ttype: TType::Id,
            offset,
        }
    }

    fn read_number(&mut self, ch: char, offset: i32) -> Token {
        let mut num_literal = String::from(ch);
        let mut look_ahead: char;
        while self.peek_char().is_numeric() {
            look_ahead = self.read_char();
            num_literal.push(look_ahead);
        }
        Token {
            lexeme: num_literal,
            ttype: TType::Num,
            offset,
        }
    }

    fn read_string_literal(&mut self, quote: char, offset: i32) -> Token {
        let mut literal = String::new();
        loop {
            match self.read_char() {
                c if c == quote => break,
                // Unterminated: report the opening quote and what we managed to read.
                '\0' | '\n' => {
                    let mut lexeme = String::from(quote);
                    lexeme.push_str(&literal);
                    return Token::from(&lexeme, TType::Invalid, offset);
                }
                '\\' => match self.read_char() {
                    'n' => literal.push('\n'),
                    't' => literal.push('\t'),
                    'r' => literal.push('\r'),
                    '\0' => return Token::from(&literal, TType::Invalid, offset),
                    other => literal.push(other),
                },
                c => literal.push(c),
            }
        }
        Token::from(&literal, TType::Literal, offset)
    }

    // create table for keywords
    fn create_reserved() -> HashMap<String, TType> {
        let keywords: HashMap<String, TType> = HashMap::from([
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
            ("in".to_string(), TType::In),
            ("while".to_string(), TType::While),
        ]);
        keywords
    }
}
