use crate::allocator::{Allocator, Cell, Heap};
use crate::ast::*;
use crate::environment::Environment;
use crate::lexer::TType;
use crate::lexer::Token;
use crate::parser::Parser;
use crate::symbols::*;

pub struct Executor<'l> {
    global_env: Box<Environment<'static>>,
    heap: Heap<Object>,
    parser: &'l Parser<'l>,
}

impl<'l> Executor<'l> {
    pub fn new(env: Box<Environment<'static>>, parser: &'l Parser<'l>) -> Self {
        Self {
            global_env: env,
            heap: Heap::new(),
            parser,
        }
    }

    pub fn visit_program(&mut self, program: &Program) -> Vec<Cell<Object>> {
        let mut result = Vec::new();
        let mut global_scope = self.global_env.clone();
        for stmt in &program.statements {
            result.push(self.visit_expr(stmt, global_scope.as_mut()));
        }
        result
    }

    pub fn visit_expr(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        match node {
            Node::BinaryOp(op) => match op.ttype() {
                NodeType::Add => self.visit_add(node, env),
                NodeType::Mul => self.visit_mul(node, env),
                NodeType::Div => self.visit_div(node, env),
                NodeType::Sub => self.visit_sub(node, env),
                NodeType::Lt => self.visit_lt(node, env),
                NodeType::LtEq => self.visit_lte(node, env),
                NodeType::Gt => self.visit_gt(node, env),
                NodeType::GtEq => self.visit_gte(node, env),
                NodeType::Eq => self.visit_eq(node, env),
                NodeType::NotEq => self.visit_neq(node, env),
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
            Node::Array(arr) => {
                let mut elements = Vec::new();
                for element in &arr.elements {
                    let obj = self.visit_expr(element, env);
                    elements.push(obj.as_ref().clone());
                }
                self.heap.allocate_cell(Object::Array(elements))
            }
            Node::Range(range) => {
                let start = match self.visit_expr(&range.start, env).as_ref() {
                    Object::Number(n) => *n,
                    _ => {
                        return self.emit_type_error(
                            "Range start must be a number".to_string(),
                            NodeParseInfo {
                                lineno: range.lineno,
                                token: Token {
                                    lexeme: "..".to_string(),
                                    ttype: crate::lexer::TType::Range,
                                    offset: 0,
                                },
                            },
                        )
                    }
                };
                let end = match self.visit_expr(&range.end, env).as_ref() {
                    Object::Number(n) => *n,
                    _ => {
                        return self.emit_type_error(
                            "Range end must be a number".to_string(),
                            NodeParseInfo {
                                lineno: range.lineno,
                                token: Token {
                                    lexeme: "..".to_string(),
                                    ttype: crate::lexer::TType::Range,
                                    offset: 0,
                                },
                            },
                        )
                    }
                };
                self.heap
                    .allocate_cell(Object::Range(crate::symbols::RangeObj { start, end }))
            }
            Node::StringLiteral(s) => self.heap.allocate_cell(Object::String(s.literal.clone())),
            Node::Block(block) => self.visit_block(block, env),
            Node::Declare(decl) => self.visit_declare(decl),
            Node::Init(init) => self.visit_init(init, env),
            Node::Ident(id) => self.visit_ident(id, env),
            Node::If(ifs) => self.visit_if(ifs, env),
            Node::While(w) => self.visit_while(w, env),
            Node::For(forstmt) => self.visit_for(forstmt, env),
            Node::Function(func) => self.visit_func_anon(func),
            Node::FunctionExpr(func) => self.visit_func_expr(func, env),
            Node::Call(func_call) => {
                let parent_scope = env.clone();
                let boxed_parent_scope = Box::new(parent_scope);
                let mut scope = Environment::new_with_prev(&boxed_parent_scope);
                return self.visit_func_call(&mut scope, func_call);
            }
            _ => todo!(),
        }
    }

    fn visit_block(&mut self, block: &Block, env: &mut Environment) -> Cell<Object> {
        for stmt in &block.statements {
            match stmt.as_ref() {
                Node::Return(ret) => {
                    let val = self.visit_expr(&ret.value, env);
                    return val;
                }
                _ => {
                    let _ = self.visit_expr(stmt, env);
                }
            }
        }
        self.heap.allocate_cell(Object::Null)
    }

    fn visit_for(&mut self, forstmt: &ForStmt, env: &mut Environment) -> Cell<Object> {
        let iter_cell = self.visit_expr(&forstmt.iter, env);
        let iter_obj = iter_cell.as_ref();
        let mut iterator = match iter_obj.get_iterator() {
            Some(it) => it,
            None => {
                return self.emit_type_error(
                    "object is not iterable".to_string(),
                    NodeParseInfo {
                        lineno: forstmt.lineno,
                        token: forstmt.token.clone(),
                    },
                )
            }
        };

        let ident_name = match *forstmt.target {
            Node::Ident(ref id) => id.value.clone(),
            _ => String::from(""),
        };

        while iterator.has_next() {
            if let Some(item) = iterator.next() {
                let cell = self.heap.allocate_cell(item);
                if ident_name.len() > 0 {
                    self.global_env.add(&ident_name, cell);
                }
                let _ = self.visit_expr(&forstmt.block, env);
            } else {
                break;
            }
        }
        self.heap.allocate_cell(Object::Null)
    }

    fn visit_declare(&mut self, decl: &Declare) -> Cell<Object> {
        for ident in &decl.idents {
            if let Node::Ident(id) = ident {
                self.global_env
                    .add(&id.value, self.heap.allocate_cell(Object::Null));
            }
        }
        self.heap.allocate_cell(Object::Null)
    }

    fn visit_init(&mut self, init: &Init, env: &mut Environment) -> Cell<Object> {
        let value_cell = self.visit_expr(&init.value, env);
        let name = match *init.ident {
            Node::Ident(ref id) => id.value.clone(),
            _ => String::from(""),
        };
        let  scope = env;

        if name.len() > 0 {
            scope.add(&name, value_cell.clone());
        }
        value_cell
    }

    fn visit_ident(&mut self, id: &Ident, env: &mut Environment) -> Cell<Object> {
        let scope = env;

        match scope.get(&id.value) {
            Some(cell) => {
                let obj = cell.as_ref().clone();
                self.heap.allocate_cell(obj)
            }
            None => self.emit_type_error(
                format!("undefined identifier '{}'", id.value),
                NodeParseInfo {
                    lineno: id.lineno,
                    token: id.token.clone(),
                },
            ),
        }
    }

    fn visit_func_expr(&mut self, func: &FunctionExpr, env: &mut Environment) -> Cell<Object> {
        let f_obj = self
            .heap
            .allocate_cell(Object::Function(FunctionType::Expr(func.clone())));
        match func.name.as_ref() {
            Node::Ident(id) => env.add(&id.value, f_obj.clone()),
            _ => (),
        }
        f_obj
    }

    fn visit_func_anon(&mut self, func: &Function) -> Cell<Object> {
        self.heap
            .allocate_cell(Object::Function(FunctionType::Decl(func.clone())))
    }

    fn visit_func_call(&mut self, scope: &mut Environment, func_call: &Call) -> Cell<Object> {
        if scope.is_recursion_limit_exceded() {
            return self.emit_recursion_limit_reached_error(NodeParseInfo {
                lineno: func_call.lineno,
                token: func_call.token.clone(),
            });
        }

        
        let callee_node = match func_call.func {
            Some(ref node) => node.as_ref(),
            None => {
                return self.emit_type_error(
                    "invalid function call".to_string(),
                    NodeParseInfo { lineno: func_call.lineno, token: func_call.token.clone() },
                )
            }
        };

        let callee_cell = self.visit_expr(callee_node, scope);

        let mut arg_cells: Vec<Cell<Object>> = Vec::with_capacity(func_call.args.len());
        for arg in &func_call.args {
            arg_cells.push(self.visit_expr(arg, scope));
        }

        let callee_obj = callee_cell.as_ref().clone();
        match callee_obj {
            Object::Function(ft) => {
                let (param_names, body_owned): (Vec<String>, Node) = match ft {
                    FunctionType::Decl(f) => {
                        let mut names = Vec::new();
                        for p in &f.params {
                            if let Node::Ident(ref id) = p { names.push(id.value.clone()); }
                        }
                        (names, (*f.body).clone())
                    }
                    FunctionType::Expr(fe) => {
                        match fe.func.as_ref() {
                            Node::Function(f) => {
                                let mut names = Vec::new();
                                for p in &f.params {
                                    if let Node::Ident(ref id) = p { names.push(id.value.clone()); }
                                }
                                (names, (*f.body).clone())
                            }
                            other => (Vec::new(), other.clone()),
                        }
                    }
                };

                if param_names.len() != arg_cells.len() {
                    return self.emit_type_error(
                        "arity mismatch".to_string(),
                        NodeParseInfo { lineno: func_call.lineno, token: func_call.token.clone() },
                    );
                }

                for (i, name) in param_names.iter().enumerate() {
                    scope.add(name, arg_cells[i].clone());
                }

                
                let result = self.visit_expr(&body_owned, scope);
                return result;
            }
            _ => {
                return self.emit_type_error(
                    "object is not callable".to_string(),
                    NodeParseInfo { lineno: func_call.lineno, token: func_call.token.clone() },
                );
            }
        }
    }

    fn visit_if(&mut self, ifs: &If, env: &mut Environment) -> Cell<Object> {
        let pred = self.visit_expr(&ifs.predicate, env);
        match pred.as_ref() {
            Object::Bool(true) => self.visit_expr(&ifs.block, env),
            Object::Bool(false) => self.visit_expr(&ifs.alt, env),
            _ => self.emit_type_error(
                "predicate must be boolean".to_string(),
                NodeParseInfo {
                    lineno: ifs.lineno,
                    token: ifs.token.clone(),
                },
            ),
        }
    }

    fn visit_while(&mut self, w: &While, env: &mut Environment) -> Cell<Object> {
        loop {
            let pred = self.visit_expr(&w.predicate, env);
            match pred.as_ref() {
                Object::Bool(true) => {
                    let _ = self.visit_expr(&w.block, env);
                }
                Object::Bool(false) => break,
                _ => {
                    return self.emit_type_error(
                        "predicate must be boolean".to_string(),
                        NodeParseInfo {
                            lineno: w.lineno,
                            token: w.token.clone(),
                        },
                    )
                }
            }
        }
        self.heap.allocate_cell(Object::Null)
    }

    pub fn visit_lt(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let l = self.visit_expr(&op.left, env);
        let r = self.visit_expr(&op.right, env);
        match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => self.heap.allocate_cell(Object::Bool(a < b)),
            _ => self.emit_type_error(
                "expected numbers for <".to_string(),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        }
    }

    pub fn visit_lte(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let l = self.visit_expr(&op.left, env);
        let r = self.visit_expr(&op.right, env);
        match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => self.heap.allocate_cell(Object::Bool(a <= b)),
            _ => self.emit_type_error(
                "expected numbers for <=".to_string(),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        }
    }

    pub fn visit_gt(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let l = self.visit_expr(&op.left, env);
        let r = self.visit_expr(&op.right, env);
        match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => self.heap.allocate_cell(Object::Bool(a > b)),
            _ => self.emit_type_error(
                "expected numbers for >".to_string(),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        }
    }

    pub fn visit_gte(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let l = self.visit_expr(&op.left, env);
        let r = self.visit_expr(&op.right, env);
        match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => self.heap.allocate_cell(Object::Bool(a >= b)),
            _ => self.emit_type_error(
                "expected numbers for >=".to_string(),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        }
    }

    pub fn visit_eq(&mut self, node: &Node, env:& mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let l = self.visit_expr(&op.left, env);
        let r = self.visit_expr(&op.right, env);
        let res = match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => a == b,
            (Object::Bool(a), Object::Bool(b)) => a == b,
            (Object::String(a), Object::String(b)) => a == b,
            (Object::Null, Object::Null) => true,
            _ => false,
        };
        self.heap.allocate_cell(Object::Bool(res))
    }

    pub fn visit_neq(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let l = self.visit_expr(&op.left, env);
        let r = self.visit_expr(&op.right, env);
        let res = match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => a != b,
            (Object::Bool(a), Object::Bool(b)) => a != b,
            (Object::String(a), Object::String(b)) => a != b,
            (Object::Null, Object::Null) => false,
            _ => true,
        };
        self.heap.allocate_cell(Object::Bool(res))
    }

    pub fn visit_add(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_mul should only be called with Node::BinaryOp"),
        };

        let left_node = self.visit_expr(&op.left, env);
        let right_node = self.visit_expr(&op.right, env);

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

    pub fn visit_sub(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_sub should only be called with Node::BinaryOp"),
        };
        let left_node = self.visit_expr(&op.left, env);
        let right_node = self.visit_expr(&op.right, env);

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

    pub fn visit_mul(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_mul should only be called with Node::BinaryOp"),
        };
        let left_node = self.visit_expr(&op.left, env);
        let right_node = self.visit_expr(&op.right, env);

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

    pub fn visit_div(&mut self, node: &Node, env: &mut Environment) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_div should only be called with Node::BinaryOp"),
        };

        let left_node = self.visit_expr(&op.left, env);
        let right_node = self.visit_expr(&op.right, env);

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

    fn emit_recursion_limit_reached_error(&mut self, parse_info: NodeParseInfo) -> Cell<Object> {
        let msg = format!(
            "Runtime Error at line {}: Recursion limit reached\n",
            parse_info.lineno
        );
        let offset = parse_info.token.offset;
        self.heap.allocate_cell(Object::Error(ErrorObj {
            type_error: None,
            run_time_error: Some(RuntimeError::RecursionLimitReached(ErrInfo {
                err_type: ErrType::RuntimeError("Recursion limit reached".to_owned()),
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
