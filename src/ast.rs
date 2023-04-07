/* 
    Ast module: houses the Abstract Syntax Tree structure used during parse phase.
*/

use std::rc::Rc;

use crate::lexer::{Token, TType};
use crate::symbols::*;
use crate::parser::ParseError;

pub trait Node {

    fn visit(&self) -> Box<dyn Object>; 
    fn ttype(&self) -> NodeType;
}

#[derive(Debug, PartialEq)]
pub enum NodeType {
    Undefined, 
    Boolean,
    Stmt, 
    Block,
    Null,
    Number,
    Mul,
    Add, 
    Sub,
    Div,
    Assign,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Eq,
    NotEq,
    Not,
    Expression,
    Return,
    If, 
    StringLiteral,
    Identifier,
    While,
    Function,
    FunctionExpr,
    Init,
    Range,
    Declare,
    For,
    Call,
    Program,
    Err
}

#[derive(PartialEq, PartialOrd)]
pub enum Precendence {
    Base(i8),
    Lt(i8),
    Gt(i8),
    LtEq(i8),
    GtEq(i8),
    Eq(i8),
    NotEq(i8),
    Add(i8),
    Sub(i8),
    Div(i8),
    Mul(i8),
    USub(i8),
    Not(i8),
    Call(i8),
    Assign(i8),
    Minus(i8),
}


enum Association {
    Left = 0,
    Right = 1,
}

pub struct Block<'a> {
    // pub ttype: NodeType,
    pub lineno: i32,
    pub token: Token,
    pub statements: Vec<&'a dyn Node>,
}

pub struct Return<'a> {
    // pub ttype: NodeType,
    pub lineno: i32,
    pub token: Token,
    pub value: &'a dyn Node,
}

pub struct Program {
    pub statements: Vec<Box<dyn Node>>,
}

pub struct Null {
    pub lineno: i32,
    pub token: Token,
}

pub struct Ident {
    pub lineno: i32,
    pub token: Rc<Token>,
    pub value: String
}

pub struct StringLiteral {
    pub lineno: i32,
    pub token: Rc<Token>,
    pub literal: String,
}

pub struct Number {
    pub lineno: i32,
    pub token: Rc<Token>,
    pub value: i32,
}

pub struct BinaryOp {
    pub lineno: i32,
    pub token: Rc<Token>,
    pub right: Box<dyn Node>,
    pub left: Box<dyn Node>, 
}

pub struct UniaryOp {
    pub lineno: i32,
    pub token: Rc<Token>,
    pub right: Box<dyn Node>,
}

pub struct Init {
    pub lineno: i32,
    pub token: Token,
    pub ident: Box<dyn Node>,
    pub value: Box<dyn Node>,
}

pub struct Declare<'a> {
    pub lineno: i32,
    pub token: Token,
    pub idents: Vec<&'a dyn Node>,
}

pub struct While<'a> {
    // pub ttype: NodeType,
    pub lineno: i32,
    pub token: Token,
    pub predicate: &'a dyn Node,
    pub block: &'a dyn Node, 
}

pub struct If<'a> {
    // pub ttype: NodeType,
    pub lineno: i32,
    pub token: Token,
    pub predicate: &'a dyn Node,
    pub alt: &'a dyn Node, 
    pub block: &'a dyn Node,
}

pub struct Boolean {
    pub lineno: i32,
    pub token: Token,
    pub value: bool,
}

pub struct Error {
    pub error_type: ParseError
}

impl Node for Null {
    
    fn visit(&self) -> Box<dyn Object> {
        //TODO: implement visit method 
        Box::new(NullObj{})
    }

    fn ttype(&self) -> NodeType{
        NodeType::Null
    }
}

impl Node for Number  {
    
    fn visit(&self) -> Box<dyn Object> {
        println!("{}", self.value);
        Box::new(NumberObj{value: self.value})
    }

    fn ttype(&self) -> NodeType {
        NodeType::Number
    }
}

impl Node for Error {

    fn visit(&self) -> Box<dyn Object> {
        Box::new(ErrorObj{parse_error: Some(self.error_type.clone()), type_error: None})
    }

    fn ttype(&self) -> NodeType {
        NodeType::Err
    }
}

impl Node for StringLiteral {
    fn visit(&self) -> Box<dyn Object> {
        Box::new(StringObj{literal: self.literal.clone()})
    }

    fn ttype(&self) -> NodeType {
        NodeType::StringLiteral
    }
}

impl Node for Ident {
    fn visit(&self) -> Box<dyn Object> {
        todo!()
    }

    fn ttype(&self) -> NodeType {
        NodeType::Identifier
    }
}

impl Node for Boolean {
    fn visit(&self) -> Box<dyn Object> {
        todo!()
    }

    

    fn ttype(&self) -> NodeType {
        NodeType::Boolean
    }
}

impl Node for BinaryOp {
    fn visit(&self) -> Box<dyn Object> {
        let result_node : Box<dyn Object>;
        let right_node = self.right.visit();
        let left_node = self.left.visit();

        let rttype = right_node.ttype();
        let lttype = left_node.ttype();
        if rttype == lttype  {
            result_node = match rttype {
                Type::Number => {
                    let rvalue = right_node.visit().downcast::<i32>().unwrap();
                    let lvalue = left_node.visit().downcast::<i32>().unwrap();
                    match self.ttype() {
                        NodeType::Add => Box::new(NumberObj{value: *rvalue + *lvalue}),
                        NodeType::Sub => Box::new(NumberObj{value: *rvalue - *lvalue}),
                        NodeType::Mul => Box::new(NumberObj{value: *rvalue * *lvalue}),
                        NodeType::Eq => Box::new(BoolObj{value: rvalue == lvalue}),
                        _ => Box::new(ErrorObj{parse_error: None, 
                            type_error: Some(TypeError::PlaceHolder(format!("invalid operation on type {:?}", rttype)))
                        })
                    }
                },
                _ => Box::new(ErrorObj{parse_error: None, 
                    type_error: Some(TypeError::PlaceHolder(format!("invalid operation on type {:?}", rttype)))
                })
            };
        }else {
            result_node =  Box::new(ErrorObj{parse_error: None, 
                type_error: Some(TypeError::TypeMismatch(format!("type mismatch {:?} and {:?}", rttype, lttype)))
            })
        }
        result_node
    }

    fn ttype(&self) -> NodeType {
        match self.token.ttype {
            TType::Plus => NodeType::Add,
            TType::Minus => NodeType::Sub, 
            TType::Asterisk => NodeType::Mul,
            TType::Eq => NodeType::Eq,
            TType::Div => NodeType::Div, 
            TType::Gt => NodeType::Gt, 
            TType::Lt => NodeType::Lt, 
            _ => NodeType::Undefined
        }
    }
}

impl Node for UniaryOp {
    fn visit(&self) -> Box<dyn Object> {
        todo!()
    }

    fn ttype(&self) -> NodeType {
        match self.token.ttype {
            TType::UMinus => NodeType::Sub,
            _ => NodeType::Undefined
        }
    }
}

impl Node for Program {
    fn visit(&self) -> Box<dyn Object> {
       for node in &self.statements {
            node.visit();
       }
       Box::new(NullObj{})
    }

    fn ttype(&self) -> NodeType {
        NodeType::Program
    }
}