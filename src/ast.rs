/* 
    Ast module: houses the Abstract Syntax Tree structure used during parse phase.
*/

use crate::lexer::Token;
use crate::symbols::Object;

pub trait Node {

    fn visit(&self) -> dyn Object; 
    fn ttype(&self) -> NodeType;
}

pub enum NodeType {
    Undefined, 
    Boolean,
    Stmt, 
    Block,
    Null,
    Number,
    Mul,
    Add, 
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
}

#[derive(PartialEq)]
enum Precendence {
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
}


enum Association {
    Left = 0,
    Right = 1,
}

pub struct Block<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    statements: Vec<&'a dyn Node>,
}

pub struct Return<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    value: &'a dyn Node,
}

pub struct Program<'a> {
    ttype: NodeType,
    statements: Vec<&'a dyn Node>,
}

pub struct Null {
    ttype: NodeType,
    lineno: i32,
    token: Token,
}

pub struct Ident<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    value: &'a dyn Node
}

pub struct StringLiteral {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    literal: String,
}

pub struct Number {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    value: i32,
}

pub struct BinaryOp<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    right: &'a dyn Node,
    left: &'a dyn Node, 
}

pub struct UniaryOp<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    right: &'a dyn Node,
}

pub struct Init<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    ident: &'a dyn Node,
    value: &'a dyn Node,
}

pub struct Declare<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    idents: Vec<&'a dyn Node>,
}

pub struct While<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    predicate: &'a dyn Node,
    block: &'a dyn Node, 
}

pub struct If<'a> {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    predicate: &'a dyn Node,
    alt: &'a dyn Node, 
    block: &'a dyn Node,
}

pub struct Boolean {
    ttype: NodeType,
    lineno: i32,
    token: Token,
    value: bool,
}

