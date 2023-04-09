/*

Parser module: Houses implementation for beesafe language parser

*/

use std::rc::Rc;

use crate::ast::*;
use crate::lexer::{Lexer, Token, TType};


pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    operator_stack: Vec<Token>,
    operand_stack: Vec<Box<dyn Node>>,
    errors: Vec<Box<dyn Node>>,
}

#[derive(Clone)]
pub enum ParseError {
    MissingIdent(String),
    UndeterminedType(String),
    PlaceHolder(String),
}

impl<'a> Parser<'a> {
 
    pub fn new(lexer: &'a mut Lexer) -> Self {
        
        Parser { 
            lexer: lexer, 
            current_token: None, 
            peek_token: None,
            operator_stack: vec![],
            operand_stack: vec![],
            errors: vec![],
        }
    }

    pub fn has_errors(&self) -> bool {
        self.errors.is_empty() == false
    }

    pub fn show_errors(&self) {
        for err in &self.errors {
            println!("{}", err.visit().visit().downcast::<String>().unwrap());
        }
    }

    fn consume_next_token(& mut self) {

        match self.peek_token.clone() {
            Some(token) => { 
                self.current_token = Some(token);
                self.peek_token = Some(self.lexer.next_token());
             },
            None =>  { 
                self.current_token = Some(self.lexer.next_token());
                self.peek_token = Some(self.lexer.next_token());
            }
        }
    }

    // parse expression using the shunting yard technique (https://en.wikipedia.org/wiki/Shunting_yard_algorithm) 
    fn parse_expression(& mut self) -> Box<dyn Node> {
        /*
        
        (expr) -> (produce) 
        (produce) -> num|id|define|string_literal|(expr)|null 
        num: <-> 1|2|..|intergers
        id: <-> identifiers
        string_literal <-> "string literal" | ''
        null <-> null keyword

        */
        self.expression();
        let node = self.operand_stack.pop().unwrap_or(Box::new(Error{error_type: ParseError::PlaceHolder(format!(""))}));
        node
    }

    // expr -> produce
    fn expression(& mut self) {
        let null_token = Token{lexeme: "".to_string(), ttype: TType::Null};
        self.operator_stack.push(null_token);
        self.produce();
        let mut look_ahead = self.peek_token.as_ref().unwrap();
        let mut ttype =  look_ahead.ttype;
        while self.resolve_binary_op(ttype) {
            let lexeme = look_ahead.lexeme.clone();
            let op_token = Token { lexeme: lexeme, ttype: ttype.clone()};
            self.push_operator(op_token);
            self.consume_next_token();
            self.consume_next_token();
            self.produce();
            ttype = self.peek_token.as_ref().unwrap().ttype;
            look_ahead = self.peek_token.as_ref().unwrap();
        }
        let mut op = self.operator_stack.last().unwrap();
        while op.ttype != TType::Error && op.ttype != TType::Null {
            let is_binary = self.resolve_binary_op(op.ttype);
            self.pop_operator(is_binary);
            op = self.operator_stack.last().unwrap();
        }
        self.operator_stack.pop();
    }

    // (produce) -> ...
    fn produce(& mut self) {
        let curr_token = self.current_token.as_ref().unwrap();
        let  ttype =  curr_token.ttype;
        match ttype {
            TType::Num => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node)
            },
            TType::Null => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node);
            },
            TType::Id => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node);
            },
            TType::Literal => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node);
            },
            TType::False => {
                let node = self.parse_bool();
                self.operand_stack.push(node);
            },
            _ => {
                if !self.resolve_binary_op(ttype) {
                    let token = Token { lexeme: curr_token.lexeme.clone(), ttype: curr_token.ttype.clone()};
                    self.push_operator(token);
                    self.consume_next_token();
                    self.produce();
                    self.pop_operator(false);
                }else {
                    //TODO: store error
                }
            }
        }
    }

    fn resolve_binary_op(&self, op_type: TType) -> bool {
        match op_type {
             TType::Plus => true,
             TType::Minus => true,
             TType::Asterisk => true, 
             TType::Div => true,
             TType::Lt => true,
             TType::LtEq => true,
             TType::Gt => true,
             TType::GtEq => true,
             TType::NotEq => true,
             TType::Eq => true,
             TType::Assign => true,
             _ => false,
        }
    }

    fn resolve_precedence(& self, op_type: &TType) -> Precendence {
        match op_type {
            TType::Plus => Precendence::Add(2),
            TType::Minus => Precendence::Minus(2),
            TType::Asterisk => Precendence::Mul(3), 
            TType::Div => Precendence::Div(3),
            TType::Lt => Precendence::Lt(1),
            TType::LtEq => Precendence::LtEq(1),
            TType::Gt => Precendence::Gt(1),
            TType::GtEq => Precendence::GtEq(1),
            TType::NotEq => Precendence::NotEq(1),
            TType::Eq => Precendence::Eq(1),
            TType::Assign => Precendence::Assign(1),
            TType::UMinus => Precendence::USub(2),
            _ => Precendence::Base(0),
        }
    }

    fn push_operator(&mut self, op: Token) {

        let precedence = self.resolve_precedence(&op.ttype);
        let mut last_op = self.operator_stack.last().unwrap();
        while self.resolve_precedence(&last_op.ttype) >= precedence {
            self.pop_operator( self.resolve_binary_op(op.ttype));
            last_op = self.operator_stack.last().unwrap();
        }
        self.operator_stack.push(op);
    }

    fn pop_operator(&mut self, is_binary: bool) {
        if self.operand_stack.len() >= 2  && is_binary {
            let right = self.operand_stack.pop().unwrap();
            let left = self.operand_stack.pop().unwrap();
            let  op = self.operator_stack.pop().unwrap();
            let node = self.parse_binary_op(
                Token { lexeme: op.lexeme.clone(), ttype: op.ttype.clone() }
                , 
                left, 
                right
            );
            self.operand_stack.push(node);
        } else if self.operand_stack.len() > 0 {
            let operand_node = self.operand_stack.pop().unwrap();
            let  op = self.operator_stack.pop().unwrap();
            let node = self.parse_uniary_op(
                Token { lexeme: op.lexeme.clone(), ttype: op.ttype.clone() },
                operand_node
            );
            self.operand_stack.push(node);
        }

    }

    fn create_node(&mut self, op_type: &TType) -> Box<dyn Node> {
        let  current_token = self.current_token.as_ref().unwrap();
        let lexeme = current_token.lexeme.to_string();
        let ttype =  current_token.ttype;
        match op_type {
            TType::Num => 
                Box::new(Number{
                token: Rc::new(current_token.clone()), 
                lineno: self.lexer.lineno(),
                value: lexeme.parse().unwrap()
            }),
            TType::Literal => 
                Box::new(StringLiteral{token: Rc::new(current_token.clone()),
                lineno: self.lexer.lineno(),
                literal: lexeme.parse().unwrap(),
            }),
            TType::Id => {
            self.consume_next_token();
            Box::new(Ident{token: Rc::new(Token{lexeme: lexeme.clone(), ttype: ttype.clone() }),
            lineno: self.lexer.lineno(),
            value: lexeme.parse().unwrap()
            })
        },
            _ => Box::new(Error{
                error_type: ParseError::UndeterminedType(
                    format!("couldn't parse the terminal type {}", lexeme))     
            })
        }
    }

    fn parse_uniary_op(&mut self, 
        op_token: Token, 
        right_operand: Box<dyn Node>
    ) -> Box<dyn Node> {
        match op_token.ttype {
            TType::UMinus => {
                Box::new(
                UniaryOp{
                    lineno: self.lexer.lineno(),
                    token: Rc::new(Token { lexeme: op_token.lexeme.clone(), ttype: op_token.ttype.clone() }),
                    right: right_operand,
                })
            },
            _ => Box::new(Error{
                error_type: ParseError::UndeterminedType(
                    format!("couldn't parse uniary operator type {}", op_token.lexeme))     
            })
        }
    }

    fn parse_binary_op(&mut self, 
        op_token: Token, 
        left_operand: Box<dyn Node> , 
        right_operand: Box<dyn Node>
    ) -> Box<dyn Node> {
        
        match op_token.ttype {
            TType::Plus => { 
            Box::new(
            BinaryOp{
                lineno: self.lexer.lineno(),
                token: Rc::new(op_token),
                right: right_operand,
                left: left_operand
            })
        },
        TType::Minus => {
            Box::new(
            BinaryOp{
                lineno: self.lexer.lineno(),
                token: Rc::new(op_token),
                right: right_operand,
                left: left_operand
            })
        },
        TType::Asterisk => {
            Box::new(
                BinaryOp{
                    lineno: self.lexer.lineno(),
                    token: Rc::new(op_token),
                    right: right_operand,
                    left: left_operand
            })
        },
            _ => Box::new(Error{
                error_type: ParseError::UndeterminedType(
                    format!("couldn't parse binary operator type {}", op_token.lexeme))     
            })
        }
    }

    fn parse_bool(&mut self) -> Box<dyn Node> {
        let  current_token = self.current_token.as_ref().unwrap();
        if current_token.ttype == TType::True {
            return Box::new(Boolean{lineno: self.lexer.lineno(),
            token: current_token.clone(),
        value: true});
        }
        return Box::new(Boolean{lineno: self.lexer.lineno(),
            token: current_token.clone(),
        value: false});
    }

    pub fn parse_program(& mut self) -> Program {
        let mut program = Program{statements: vec![]};
        self.consume_next_token();
        let mut current_token = self.current_token.as_ref().unwrap();
        let mut ttype = current_token.ttype;
        while ttype != TType::Eob {

            if ttype == TType::Newline{
                self.consume_next_token();
                current_token = self.current_token.as_ref().unwrap();
                ttype = current_token.ttype;
                continue;
            }
            let node = match ttype {
                _ => self.parse_expression()
            };
            if node.ttype() == NodeType::Err {
                self.errors.push(node);
            }else {
                program.statements.push(node);
            }
            self.consume_next_token();
            current_token = self.current_token.as_ref().unwrap();
            ttype = current_token.ttype;
        }
        program
    }
    

}
