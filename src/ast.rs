/* 
    Ast module: houses the Abstract Syntax Tree structure used during parse phase.
*/

use std::fmt::Display;

use crate::lexer::{Token, TType};
use crate::parser::ParseError;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug)]
pub enum Node {
    Block(Block),
    Return(Return),
    Program(Program),
    Null(Null),
    Ident(Ident),
    While(While),
    If(If),
    Boolean(Boolean),
    StringLiteral(StringLiteral),
    Number(Number),
    Init(Init),
    Error(Error),
    UniaryOp(UniaryOp), 
    BinaryOp(BinaryOp)
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

#[derive(Debug)]
pub struct Block {
    pub lineno: i32,
    pub token: Token,
    pub statements: Vec<Box<Node>>,
}

#[derive(Debug)]
pub struct Return {
    pub lineno: i32,
    pub token: Token,
    pub value: Box<Node>,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Node>,
}

#[derive(Debug)]
pub struct Null {
    pub lineno: i32,
    pub token: Token,
}

#[derive(Debug)]
pub struct Ident {
    pub lineno: i32,
    pub token: Token,
    pub value: String
}

#[derive(Debug)]
pub struct StringLiteral {
    pub lineno: i32,
    pub token: Token,
    pub literal: String,
}

#[derive(Debug)]
pub struct Number {
    pub lineno: i32,
    pub token: Token,
    pub value: i32,
}

#[derive(Debug)]
pub struct BinaryOp {
    pub lineno: i32,
    pub token: Token,
    pub right: Box<Node>,
    pub left: Box<Node>, 
}

#[derive(Debug)]
pub struct UniaryOp {
    pub lineno: i32,
    pub token: Token,
    pub right: Box<Node>,
}

#[derive(Debug)]
pub struct Init {
    pub lineno: i32,
    pub token: Token,
    pub ident: Box<Node>,
    pub value: Box<Node>,
}

#[derive(Debug)]
pub struct Declare {
    pub lineno: i32,
    pub token: Token,
    pub idents: Vec<Node>,
}

#[derive(Debug)]
pub struct While {
    pub lineno: i32,
    pub token: Token,
    pub predicate: Box<Node>,
    pub block: Box<Node>, 
}

#[derive(Debug)]
pub struct If {
    pub lineno: i32,
    pub token: Token,
    pub predicate: Box<Node>,
    pub alt: Box<Node>, 
    pub block: Box<Node>,
}

#[derive(Debug)]
pub struct Boolean {
    pub lineno: i32,
    pub token: Token,
    pub value: bool,
}

#[derive(Error, Debug, Diagnostic)]
pub struct Error {
    #[source_code]
    pub src: String,
    pub error_type: ParseError
}

impl Null {
    pub fn ttype(&self) -> NodeType{
        NodeType::Null
    }
}

impl Number  {
    pub fn ttype(&self) -> NodeType {
        NodeType::Number
    }
}

impl Error {
    pub fn display(&self) -> String {
        match &self.error_type {
            ParseError::MissingIdent(msg) => msg.clone(),
            ParseError::UndeterminedType(msg) => msg.clone(),
            ParseError::PlaceHolder(msg) => msg.clone(),
        }
    }

}

impl StringLiteral {

    pub fn ttype(&self) -> NodeType {
        NodeType::StringLiteral
    }
}

impl Ident {

    pub fn ttype(&self) -> NodeType {
        NodeType::Identifier
    }
}

impl Boolean {

    pub fn ttype(&self) -> NodeType {
        NodeType::Boolean
    }

}
 
impl BinaryOp {
    pub fn ttype(&self) -> NodeType {
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

impl  UniaryOp {
    pub fn ttype(&self) -> NodeType {
        match self.token.ttype {
            TType::UMinus => NodeType::Sub,
            _ => NodeType::Undefined
        }
    }

}

impl Program {
    pub fn ttype(&self) -> NodeType {
        NodeType::Program
    }

}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display())
    }

}