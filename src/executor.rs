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

    pub fn visit_expr(&self, node: &Node) -> Box<Object> {
        match node {
            Node::BinaryOp(op) => match op.ttype() {
                NodeType::Add => self.visit_add(op),
                NodeType::Mul => self.visit_mul(op),
                NodeType::Div => self.visit_div(op),
                NodeType::Sub => self.visit_sub(op),
                _ => self.emit_type_error(format!("invalid operation {}", op.token.lexeme)),
            },
            Node::Number(num) => Box::new(Object::NumberObj(NumberObj { value: num.value })),
            Node::Null(_) => Box::new(Object::NullObj),
            _ => todo!(),
        }
    }

    pub fn visit_add(&self, node: &BinaryOp) -> Box<Object> {
        let left_node = self.visit_expr(&node.left);
        let right_node = self.visit_expr(&node.right);

        let lval = match left_node.as_ref() {
            Object::NumberObj(val) => Ok(val.value),
            _ => Err("expected a number type"),
        };

        let rval = match right_node.as_ref() {
            Object::NumberObj(val) => Ok(val.value),
            _ => Err("expected a number type"),
        };
       return match (lval, rval) {
            (Ok(lval), Ok(rval)) => Box::new(Object::NumberObj(NumberObj {
                value: lval + rval,
            })),
           (_, _) => self.emit_type_error(format!("expected a number type"))
        };
    }

    pub fn visit_sub(&self, node: &BinaryOp) -> Box<Object> {
        todo!()
    }

    pub fn visit_mul(&self, node: &BinaryOp) -> Box<Object> {
        todo!()
    }

    pub fn visit_div(&self, node: &BinaryOp) -> Box<Object> {
        todo!()
    }

    fn emit_type_error(&self, msg: String) -> Box<Object> {
        Box::new(Object::ErrorObj(ErrorObj {
            parse_error: None,
            type_error: Some(TypeError::TypeMismatch(msg.to_owned())),
        }))
    }
}
