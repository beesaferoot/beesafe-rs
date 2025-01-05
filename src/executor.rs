use crate::allocator::{Allocator, Cell, Heap};
use crate::ast::*;
use crate::environment::Environment;
use crate::symbols::*;

pub struct Executor<'l> {
    global_env: Box<Environment<'l>>,
    heap: Heap<Object>,
}

impl<'l> Executor<'l> {
    pub fn new(env: Box<Environment<'l>>) -> Self {
        Self {
            global_env: env,
            heap: Heap::new(),
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
                NodeType::Add => self.visit_add(op),
                NodeType::Mul => self.visit_mul(op),
                NodeType::Div => self.visit_div(op),
                NodeType::Sub => self.visit_sub(op),
                _ => self.emit_type_error(format!("invalid operation {}", op.token.lexeme)),
            },
            Node::Number(num) => self.heap.allocate_cell(Object::Number(num.value)),
            Node::Boolean(v) => self.heap.allocate_cell(Object::Bool(v.value)),
            Node::Null(_) => self.heap.allocate_cell(Object::Null),
            _ => todo!(),
        }
    }

    pub fn visit_add(&mut self, node: &BinaryOp) -> Cell<Object> {
        let left_node = self.visit_expr(&node.left);
        let right_node = self.visit_expr(&node.right);

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
            (_, _) => self.emit_type_error(format!("expected a number type")),
        }
    }

    pub fn visit_sub(&mut self, node: &BinaryOp) -> Cell<Object> {
        let left_node = self.visit_expr(&node.left);
        let right_node = self.visit_expr(&node.right);

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
            (_, _) => self.emit_type_error(format!("expected a number type")),
        }
    }

    pub fn visit_mul(&mut self, node: &BinaryOp) -> Cell<Object> {
        let left_node = self.visit_expr(&node.left);
        let right_node = self.visit_expr(&node.right);
    

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
            (_, _) => self.emit_type_error(format!("expected a number type")),
        }
    }

    pub fn visit_div(&mut self, node: &BinaryOp) -> Cell<Object> {
        let left_node = self.visit_expr(&node.left);
        let right_node = self.visit_expr(&node.right);

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
                    return self.emit_division_by_zero_error();
                }
                self.heap.allocate_cell(Object::Number(lval / rval))
            }
            (_, _) => self.emit_type_error(format!("expected a number type")),
        }
    }

    fn emit_type_error(&mut self, msg: String) -> Cell<Object> {
        self.heap.allocate_cell(Object::Error(ErrorObj {
            parse_error: None,
            type_error: Some(TypeError::TypeMismatch(msg.to_owned())),
            run_time_error: None,
        }))
    }

    fn emit_division_by_zero_error(&mut self) -> Cell<Object> {
        self.heap.allocate_cell(Object::Error(ErrorObj {
            parse_error: None,
            type_error: None,
            run_time_error: Some(RuntimeError::DivisionByZero),
        }))
    }
}
