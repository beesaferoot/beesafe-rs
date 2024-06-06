use crate::ast::*;
use crate::environment::Environment;
use crate::symbols::*;

pub struct Executor<'l> {
    global_env: Box<Environment<'l>>,
}

impl<'l> Executor<'l> {
    pub fn new(env: Box<Environment<'l>>) -> Self {
        Self { global_env: env, 
        }
    }

    pub fn visit_program(&self, program: &Program) -> Vec<Box<Object>> {
        let mut result = Vec::new();
    
        for stmt in &program.statements {
            result.push(self.visit_expr(stmt));
        }
        result
    }
    pub fn visit_expr(&self, node: &Node) -> Box<Object> {
        match node {
            Node::BinaryOp(op) => match op.ttype() {
                NodeType::Add => self.visit_add(op),
                NodeType::Mul => self.visit_mul(op),
                NodeType::Div => self.visit_div(op),
                NodeType::Sub => self.visit_sub(op),
                _ => self.emit_type_error(format!("invalid operation {}", op.token.lexeme)),
            },
            Node::Number(num) => Box::new(Object::Number(num.value)),
            Node::Boolean(v) => Box::new(Object::Bool(v.value)),
            Node::Null(_) => Box::new(Object::Null),
            _ => todo!(),
        }
    }

    pub fn visit_add(&self, node: &BinaryOp) -> Box<Object> {
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
       return match (lval, rval) {
            (Ok(lval), Ok(rval)) => Box::new(Object::Number(lval + rval)),
           (_, _) => self.emit_type_error(format!("expected a number type"))
        };
    }

    pub fn visit_sub(&self, node: &BinaryOp) -> Box<Object> {
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
       return match (lval, rval) {
            (Ok(lval), Ok(rval)) => Box::new(Object::Number(lval - rval)),
           (_, _) => self.emit_type_error(format!("expected a number type"))
        };
    }

    pub fn visit_mul(&self, node: &BinaryOp) -> Box<Object> {
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
       return match (lval, rval) {
            (Ok(lval), Ok(rval)) =>   Box::new(Object::Number(lval * rval)),
           (_, _) => self.emit_type_error(format!("expected a number type"))
        };
    }

    pub fn visit_div(&self, node: &BinaryOp) -> Box<Object> {
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
       return match (lval, rval) {
            (Ok(lval), Ok(rval)) => { 
                if rval == &0 {
                    return self.emit_division_by_zero_error();
                }
                Box::new(Object::Number(lval / rval))
            },
           (_, _) => self.emit_type_error(format!("expected a number type"))
        };
    }

    fn emit_type_error(&self, msg: String) -> Box<Object> {
        Box::new(Object::Error(ErrorObj {
            parse_error: None,
            type_error: Some(TypeError::TypeMismatch(msg.to_owned())),
            run_time_error: None
        }))
    }

    fn emit_division_by_zero_error(&self) -> Box<Object> {
        Box::new(Object::Error(ErrorObj {
            parse_error: None,
            type_error: None,
            run_time_error: Some(RuntimeError::DivisionByZero)
        }))
    }
}
