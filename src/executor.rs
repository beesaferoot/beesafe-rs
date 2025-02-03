use std::fmt::format;

use crate::allocator::{Allocator, Cell, Heap};
use crate::ast::*;
use crate::environment::Environment;
use crate::lexer::TType;
use crate::lexer::Token;
use crate::parser::Parser;
use crate::symbols::*;

pub struct Executor<'l> {
    global_env: Box<Environment<'l>>,
    heap: Heap<Object>,
    parser: &'l Parser<'l>,
}

impl<'l> Executor<'l> {
    pub fn new(env: Box<Environment<'l>>, parser: &'l Parser<'l>) -> Self {
        Self {
            global_env: env,
            heap: Heap::new(),
            parser,
        }
    }

    pub fn visit_program(&mut self, program: &Program) -> Vec<Cell<Object>> {
        let mut result = Vec::new();

        for stmt in &program.statements {
            result.push(self.visit_expr(stmt));
        }
        result
    }

    pub fn visit_expr(&mut self, node: &Node) -> Cell<Object> {
        match node {
            Node::BinaryOp(op) => match op.ttype() {
                NodeType::Add => self.visit_add(node),
                NodeType::Mul => self.visit_mul(node),
                NodeType::Div => self.visit_div(node),
                NodeType::Sub => self.visit_sub(node),
                _ => self.emit_type_error(
                    format!("invalid operation {}", op.token.lexeme),
                    NodeParseInfo {
                        lineno: op.lineno,
                        token: op.token.clone(),
                    },
                ),
            },
            Node::Number(num) => self.heap.allocate_cell(Object::Number(num.value)),
            Node::Boolean(v) => self.heap.allocate_cell(Object::Bool(v.value)),
            Node::Null(_) => self.heap.allocate_cell(Object::Null),
            _ => todo!(),
        }
    }

    pub fn visit_add(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_mul should only be called with Node::BinaryOp"),
        };

        let left_node = self.visit_expr(&op.left);
        let right_node = self.visit_expr(&op.right);

        let lval = match left_node.as_ref() {
            Object::Number(val) => Ok(val),
            _ => Err("expected a number type"),
        };

        let rval = match right_node.as_ref() {
            Object::Number(val) => Ok(val),
            _ => Err("expected a number type"),
        };
        match (lval, rval) {
            (Ok(lval), Ok(rval)) => self.heap.allocate_cell(Object::Number(lval + rval)),
            (_, _) => self.emit_type_error(
                format!("expected a number type"),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        }
    }

    pub fn visit_sub(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_sub should only be called with Node::BinaryOp"),
        };
        let left_node = self.visit_expr(&op.left);
        let right_node = self.visit_expr(&op.right);

        let lval = match left_node.as_ref() {
            Object::Number(val) => Ok(val),
            _ => Err("expected a number type"),
        };

        let rval = match right_node.as_ref() {
            Object::Number(val) => Ok(val),
            _ => Err("expected a number type"),
        };
        match (lval, rval) {
            (Ok(lval), Ok(rval)) => self.heap.allocate_cell(Object::Number(lval - rval)),
            (_, _) => self.emit_type_error(
                format!("expected a number type"),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        }
    }

    pub fn visit_mul(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_mul should only be called with Node::BinaryOp"),
        };
        let left_node = self.visit_expr(&op.left);
        let right_node = self.visit_expr(&op.right);

        let lval = match left_node.as_ref() {
            Object::Number(val) => Ok(val),
            _ => Err("expected a number type"),
        };

        let rval = match right_node.as_ref() {
            Object::Number(val) => Ok(val),
            _ => Err("expected a number type"),
        };

        match (lval, rval) {
            (Ok(lval), Ok(rval)) => self.heap.allocate_cell(Object::Number(lval * rval)),
            (_, _) => self.emit_type_error(
                format!("expected a number type"),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        }
    }

    pub fn visit_div(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_div should only be called with Node::BinaryOp"),
        };

        let left_node = self.visit_expr(&op.left);
        let right_node = self.visit_expr(&op.right);

        let lval = match left_node.as_ref() {
            Object::Number(val) => Ok(val),
            _ => Err("expected a number type"),
        };

        let rval = match right_node.as_ref() {
            Object::Number(val) => Ok(val),
            _ => Err("expected a number type"),
        };

        match (lval, rval) {
            (Ok(lval), Ok(rval)) => {
                if rval == &0 {
                    return self.emit_division_by_zero_error(NodeParseInfo {
                        lineno: op.lineno,
                        token: op.token.clone(),
                    });
                }
                self.heap.allocate_cell(Object::Number(lval / rval))
            }
            (_, _) => self.emit_type_error(
                format!("expected a number type"),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        }
    }

    fn emit_type_error(&mut self, msg: String, parse_info: NodeParseInfo) -> Cell<Object> {
        let mut err_type = ErrType::TypeError("default error".to_owned());

        let error_msg = match parse_info.token.ttype {
            TType::Num => {
                err_type = ErrType::TypeError("cannot use a number here".to_owned());
                format!(
                    "Type Error at line {}: {}\nOffending number: {}",
                    parse_info.lineno, msg, parse_info.token.lexeme
                )
            }
            TType::Plus => {
                err_type = ErrType::TypeError("Oops can't add incompatible types".to_owned());
                format!("Type Error at line {}: {}\n", parse_info.lineno, msg)
            }
            _ => String::from(""),
        };
        let offset = parse_info.token.offset;
        self.heap.allocate_cell(Object::Error(ErrorObj {
            type_error: Some(TypeError::TypeMismatch(ErrInfo {
                err_type,
                msg: error_msg,
                src: self.parser.source(),
                span: (
                    offset as usize,
                    parse_info.token.lexeme.len() + offset as usize,
                )
                    .into(),
            })),
            run_time_error: None,
        }))
    }

    fn emit_division_by_zero_error(&mut self, parse_info: NodeParseInfo) -> Cell<Object> {
        let msg = format!(
            "Runtime Error at line {}: Division by zero\n",
            parse_info.lineno
        );
        let offset = parse_info.token.offset;
        self.heap.allocate_cell(Object::Error(ErrorObj {
            type_error: None,
            run_time_error: Some(RuntimeError::DivisionByZero(ErrInfo {
                err_type: ErrType::RuntimeError("Division by zero".to_owned()),
                msg,
                src: self.parser.source(),
                span: (
                    offset as usize,
                    parse_info.token.lexeme.len() + offset as usize,
                )
                    .into(),
            })),
        }))
    }
}

pub struct NodeParseInfo {
    pub lineno: i32,
    pub token: Token,
}
