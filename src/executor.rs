use crate::allocator::{Allocator, Cell, Heap};
use crate::ast::*;
use crate::environment::Environment;
use crate::lexer::TType;
use crate::lexer::Token;
use crate::parser::Parser;
use crate::symbols::*;

// Interned integer range. Loop counters and small literals dominate allocation,
// so these cover most of it.
const SMALL_INT_MIN: i32 = -128;
const SMALL_INT_MAX: i32 = 1024;

pub struct Executor<'l> {
    global_env: &'l mut Environment,
    heap: Heap<Object>,
    parser: &'l Parser<'l>,
    null: Cell<Object>,
    booleans: [Cell<Object>; 2],
    small_ints: Vec<Cell<Object>>,
}

impl<'l> Executor<'l> {
    pub fn new(env: &'l mut Environment, parser: &'l Parser<'l>) -> Self {
        let mut heap = Heap::new();
        // Overridable so tests can collect on every allocation. Correct code
        // produces identical output at any threshold.
        let threshold = std::env::var("BEESAFE_GC_THRESHOLD")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(1000);
        heap.set_gc_threshold(threshold);
        let null = heap.intern(Object::Null);
        let booleans = [
            heap.intern(Object::Bool(false)),
            heap.intern(Object::Bool(true)),
        ];
        let small_ints = (SMALL_INT_MIN..=SMALL_INT_MAX)
            .map(|n| heap.intern(Object::Number(n)))
            .collect();
        Self {
            global_env: env,
            heap,
            parser,
            null,
            booleans,
            small_ints,
        }
    }

    // Evaluate both operands of a binary node, keeping the left rooted while the
    // right is evaluated. Returns the shadow-stack mark for the caller to release.
    fn eval_operands(&mut self, op: &BinaryOp) -> (usize, Cell<Object>, Cell<Object>) {
        let mark = self.heap.temp_mark();
        let left = self.visit_expr(&op.left);
        self.heap.temp_push(left);
        let right = self.visit_expr(&op.right);
        self.heap.temp_push(right);
        (mark, left, right)  
    }

    fn null(&self) -> Cell<Object> {
        self.null
    }

    fn boolean(&self, value: bool) -> Cell<Object> {
        self.booleans[value as usize]
    }

    fn number(&mut self, value: i32) -> Cell<Object> {
        if (SMALL_INT_MIN..=SMALL_INT_MAX).contains(&value) {
            return self.small_ints[(value - SMALL_INT_MIN) as usize];
        }
        self.heap.allocate_cell(Object::Number(value))
    }

    // Constants are shared, so returning the interned cell keeps value semantics
    // while allocating nothing. Everything else still gets a private copy.
    fn constant(&mut self, obj: &Object) -> Option<Cell<Object>> {
        match obj {
            Object::Null => Some(self.null()),
            Object::Bool(b) => Some(self.boolean(*b)),
            Object::Number(n) => {
                if (SMALL_INT_MIN..=SMALL_INT_MAX).contains(n) {
                    Some(self.number(*n))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn visit_program(&mut self, program: &Program) -> Vec<Cell<Object>> {
        let mut result = Vec::new();
        for stmt in &program.statements {
            let mark = self.heap.temp_mark();
            let cell = self.visit_expr(stmt);
            self.heap.add_root(cell);
            result.push(cell);
            // Safety net: a visitor that returns early without releasing only
            // over-roots, and this bounds that to a single statement.
            self.heap.temp_release(mark);
        }
        self.heap.force_gc();
        result
    }

    pub fn visit_expr(&mut self, node: &Node) -> Cell<Object> {
        match node {
            Node::BinaryOp(op) => match op.ttype() {
                NodeType::Add => self.visit_add(node),
                NodeType::Mul => self.visit_mul(node),
                NodeType::Div => self.visit_div(node),
                NodeType::Sub => self.visit_sub(node),
                NodeType::Lt => self.visit_lt(node),
                NodeType::LtEq => self.visit_lte(node),
                NodeType::Gt => self.visit_gt(node),
                NodeType::GtEq => self.visit_gte(node),
                NodeType::Eq => self.visit_eq(node),
                NodeType::NotEq => self.visit_neq(node),
                NodeType::Assign => self.visit_assign(node),
                _ => self.emit_type_error(
                    format!("invalid operation {}", op.token.lexeme),
                    NodeParseInfo {
                        lineno: op.lineno,
                        token: op.token.clone(),
                    },
                ),
            },
            Node::Number(num) => self.number(num.value),
            Node::Boolean(v) => self.boolean(v.value),
            Node::Null(_) => self.null(),
            Node::Array(arr) => {
                let mark = self.heap.temp_mark();
                let mut elements = Vec::new();
                for element in &arr.elements {
                    let obj = self.visit_expr(element);
                    self.heap.temp_push(obj);
                    elements.push(obj);
                }
                let array = self.heap.allocate_cell(Object::Array(elements));
                self.heap.temp_release(mark);
                array
            }
            Node::Range(range) => {
                let start = match self.visit_expr(&range.start).as_ref() {
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
                let end = match self.visit_expr(&range.end).as_ref() {
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
            Node::Block(block) => {
                return self.visit_block(&block);
            }
            Node::Declare(decl) => self.visit_declare(decl),
            Node::Init(init) => self.visit_init(init),
            Node::Ident(id) => self.visit_ident(id),
            Node::If(ifs) => self.visit_if(ifs),
            Node::While(w) => self.visit_while(w),
            Node::For(forstmt) => self.visit_for(forstmt),
            Node::Function(func) => self.visit_func_anon(func),
            Node::FunctionExpr(func) => self.visit_func_expr(func),
            Node::Call(func_call) => {
                return self.visit_func_call(func_call);
            }
            _ => {
                println!("todo node {:?}", node);
                todo!()
            }
        }
    }

    fn visit_block(&mut self, block: &Block) -> Cell<Object> {
        for stmt in &block.statements {
            match stmt.as_ref() {
                Node::Return(ret) => {
                    let mark = self.heap.temp_mark();
                    let val = self.visit_expr(&ret.value);
                    self.heap.temp_push(val);
                    let wrapped = self.heap.allocate_cell(Object::Return(val));
                    self.heap.temp_release(mark);
                    return wrapped;
                }
                _ => {
                    let r = self.visit_expr(stmt);
                    match r.as_ref() {
                        Object::Return(_) => return r,
                        Object::Error(_) => return r,
                        _ => (),
                    };
                }
            }
        }
        self.null()
    }

    fn visit_for(&mut self, forstmt: &ForStmt) -> Cell<Object> {
        let mark = self.heap.temp_mark();
        let iter_cell = self.visit_expr(&forstmt.iter);
        self.heap.temp_push(iter_cell);
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
                    if let Some(prev) = self.global_env.add(&ident_name, cell) {
                        self.heap.remove_root(&prev);
                    }
                    self.heap.add_root(cell);
                }
                let _ = self.visit_expr(&forstmt.block);
            } else {
                break;
            }
        }
        self.heap.temp_release(mark);
        self.null()
    }

    fn visit_declare(&mut self, decl: &Declare) -> Cell<Object> {
        for ident in &decl.idents {
            if let Node::Ident(id) = ident {
                let cell = self.null();
                if let Some(prev) = self.global_env.add(&id.value, cell) {
                    self.heap.remove_root(&prev);
                }
                self.heap.add_root(cell);
            }
        }
        self.null()
    }

    fn visit_init(&mut self, init: &Init) -> Cell<Object> {
        let value_cell = self.visit_expr(&init.value);
        let name = match *init.ident {
            Node::Ident(ref id) => id.value.clone(),
            _ => String::from(""),
        };

        if name.len() > 0 {
            if let Some(prev) = self.global_env.add(&name, value_cell.clone()) {
                self.heap.remove_root(&prev);
            }
            self.heap.add_root(value_cell.clone());
        }
        value_cell
    }

    fn visit_ident(&mut self, id: &Ident) -> Cell<Object> {
        match self.global_env.get(&id.value) {
            Some(cell) => {
                let obj = cell.as_ref().clone();
                match self.constant(&obj) {
                    Some(interned) => interned,
                    None => self.heap.allocate_cell(obj),
                }
            }
            None => self.emit_undefined_error(
                format!("undefined identifier '{}'", id.value),
                NodeParseInfo {
                    lineno: id.lineno,
                    token: id.token.clone(),
                },
            ),
        }
    }

    fn visit_assign(&mut self, node: &Node) -> Cell<Object> {
        let assign = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };

        let value_cell = self.visit_expr(&assign.right);
        let (name, id_ref) = match assign.left.as_ref() {
            Node::Ident(ref id) => (id.value.clone(), id),
            _ => (
                String::from(""),
                &Ident {
                    value: String::new(),
                    lineno: 0,
                    token: Token {
                        lexeme: String::new(),
                        ttype: TType::Id,
                        offset: 0,
                    },
                },
            ),
        };
        if name.len() > 0 {
            if self.global_env.get(&name).is_some() {
                self.heap.add_root(value_cell);
                if let Some(prev) = self.global_env.assign(&name, value_cell) {
                    self.heap.remove_root(&prev);
                }
            } else {
                return self.emit_undefined_error(
                    format!("undefined identifier '{}'", name),
                    NodeParseInfo {
                        lineno: id_ref.lineno,
                        token: id_ref.token.clone(),
                    },
                );
            }
        }
        self.null()
    }

    fn visit_func_expr(&mut self, func: &FunctionExpr) -> Cell<Object> {
        let f_obj = self
            .heap
            .allocate_cell(Object::Function(FunctionType::Expr(func.clone())));
        match func.name.as_ref() {
            Node::Ident(id) => {
                if let Some(prev) = self.global_env.add(&id.value, f_obj.clone()) {
                    self.heap.remove_root(&prev);
                }
                self.heap.add_root(f_obj.clone());
            }
            _ => (),
        }
        f_obj
    }

    fn visit_func_anon(&mut self, func: &Function) -> Cell<Object> {
        self.heap
            .allocate_cell(Object::Function(FunctionType::Decl(func.clone())))
    }

    fn visit_func_call(&mut self, func_call: &Call) -> Cell<Object> {
        let callee_node = match func_call.func {
            Some(ref node) => node.as_ref(),
            None => {
                return self.emit_invalid_function_call_error(
                    "function call is not valid".to_string(),
                    NodeParseInfo {
                        lineno: func_call.lineno,
                        token: func_call.token.clone(),
                    },
                )
            }
        };

        // handle built-ins
        if let Node::Ident(id) = callee_node {
            if self.is_builtin_function(id.value.as_str()) {
                return self.visit_builtin_function(id, func_call);
            }
        }

        let mark = self.heap.temp_mark();
        let callee_cell = self.visit_expr(callee_node);
        self.heap.temp_push(callee_cell);

        match callee_cell.as_ref() {
            Object::Function(_) => (),
            Object::Error(_) => {
                return callee_cell;
            }
            _ => {
                return self.emit_undefined_error(
                    "function is not defined".to_string(),
                    NodeParseInfo {
                        lineno: func_call.lineno,
                        token: func_call.token.clone(),
                    },
                )
            }
        }

        let mut arg_cells: Vec<Cell<Object>> = Vec::with_capacity(func_call.args.len());
        for arg in &func_call.args {
            let cell = self.visit_expr(arg);
            self.heap.temp_push(cell);
            arg_cells.push(cell);
        }

        // Push scope AFTER evaluating arguments (arguments are evaluated in caller's scope)
        self.global_env.push_scope();
        if self.global_env.is_scoping_limit_exceded() {
            self.global_env.pop_scope();
            return self.emit_recursion_limit_reached_error(NodeParseInfo {
                lineno: func_call.lineno,
                token: func_call.token.clone(),
            });
        }

        let callee_obj = callee_cell.as_ref().clone();
        match callee_obj {
            Object::Function(ft) => {
                let (param_names, body_owned): (Vec<String>, Node) = match ft {
                    FunctionType::Decl(f) => {
                        let mut names = Vec::new();
                        for p in &f.params {
                            if let Node::Ident(ref id) = p {
                                names.push(id.value.clone());
                            }
                        }
                        (names, (*f.body).clone())
                    }
                    FunctionType::Expr(fe) => match fe.func.as_ref() {
                        Node::Function(f) => {
                            let mut names = Vec::new();
                            for p in &f.params {
                                if let Node::Ident(ref id) = p {
                                    names.push(id.value.clone());
                                }
                            }
                            (names, (*f.body).clone())
                        }
                        other => (Vec::new(), other.clone()),
                    },
                };

                if param_names.len() != arg_cells.len() {
                    self.global_env.pop_scope();
                    return self.emit_type_error(
                        "arity mismatch".to_string(),
                        NodeParseInfo {
                            lineno: func_call.lineno,
                            token: func_call.token.clone(),
                        },
                    );
                }

                for (i, name) in param_names.iter().enumerate() {
                    if let Some(prev) = self.global_env.add(name, arg_cells[i].clone()) {
                        self.heap.remove_root(&prev);
                    }
                    self.heap.add_root(arg_cells[i].clone());
                }

                let result = self.visit_expr(&body_owned);
                self.heap.temp_push(result);
                let removed = self.global_env.pop_scope();
                for cell in removed {
                    self.heap.remove_root(&cell);
                }
                let value = match result.as_ref() {
                    Object::Return(ret) => *ret,
                    _ => result,
                };
                self.heap.temp_push(value);
                self.heap.temp_release(mark);
                value
            }
            _ => {
                let removed = self.global_env.pop_scope();
                for cell in removed {
                    self.heap.remove_root(&cell);
                }
                return self.emit_type_error(
                    "object is not callable".to_string(),
                    NodeParseInfo {
                        lineno: func_call.lineno,
                        token: func_call.token.clone(),
                    },
                );
            }
        }
    }

    fn is_builtin_function(&self, name: &str) -> bool {
        match name {
            "print" => true,
            _ => false,
        }
    }

    fn visit_builtin_function(&mut self, id: &Ident, func_call: &Call) -> Cell<Object> {
        match id.value.as_str() {
            "print" => {
                let mut printed: Vec<String> = Vec::with_capacity(func_call.args.len());
                for arg in &func_call.args {
                    let c = self.visit_expr(arg);
                    match self.resolve_display_object(&c) {
                        Some(s) => printed.push(s),
                        None => return c,
                    }
                }
                println!("{}", printed.join(" "));
                return self.null();
            }
            _ => panic!("Error: unknown builtin function call {}", id.value),
        }
    }

    fn resolve_display_object(&self, obj: &Cell<Object>) -> Option<String> {
        match obj.as_ref() {
            Object::Null => Some("null".to_string()),
            Object::Bool(b) => Some(format!("{}", b)),
            Object::Number(n) => Some(format!("{}", n)),
            Object::String(s) => Some(s.clone()),
            Object::Error(_) => None,
            Object::Return(ret) => self.resolve_display_object(ret),
            other => Some(format!("{:?}", other)),
        }
    }

    fn visit_if(&mut self, ifs: &If) -> Cell<Object> {
        let pred = self.visit_expr(&ifs.predicate);
        match pred.as_ref() {
            Object::Bool(true) => self.visit_expr(&ifs.block),
            Object::Bool(false) => {
                // Evaluate else-if chain if present
                for alt in &ifs.else_if {
                    if let Node::If(nested_if) = alt.as_ref() {
                        return self.visit_if(nested_if);
                    } else {
                        return self.visit_expr(alt);
                    }
                }

                // Fallback to else block
                if let Some(ref else_blk) = ifs.else_block {
                    return self.visit_expr(else_blk);
                }

                self.null()
            }
            _ => self.emit_type_error(
                "predicate must be boolean".to_string(),
                NodeParseInfo {
                    lineno: ifs.lineno,
                    token: ifs.token.clone(),
                },
            ),
        }
    }

    fn visit_while(&mut self, w: &While) -> Cell<Object> {
        loop {
            let pred = self.visit_expr(&w.predicate);
            match pred.as_ref() {
                Object::Bool(true) => {
                    let _ = self.visit_expr(&w.block);
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
        self.null()
    }

    pub fn visit_lt(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let out = match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => self.boolean(a < b),
            _ => self.emit_type_error(
                "expected numbers for <".to_string(),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    pub fn visit_lte(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let out = match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => self.boolean(a <= b),
            _ => self.emit_type_error(
                "expected numbers for <=".to_string(),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    pub fn visit_gt(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let out = match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => self.boolean(a > b),
            _ => self.emit_type_error(
                "expected numbers for >".to_string(),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    pub fn visit_gte(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let out = match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => self.boolean(a >= b),
            _ => self.emit_type_error(
                "expected numbers for >=".to_string(),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    pub fn visit_eq(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let res = match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => a == b,
            (Object::Bool(a), Object::Bool(b)) => a == b,
            (Object::String(a), Object::String(b)) => a == b,
            (Object::Null, Object::Null) => true,
            _ => false,
        };
        self.heap.temp_release(mark);
        self.boolean(res)
    }

    pub fn visit_neq(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let res = match (l.as_ref(), r.as_ref()) {
            (Object::Number(a), Object::Number(b)) => a != b,
            (Object::Bool(a), Object::Bool(b)) => a != b,
            (Object::String(a), Object::String(b)) => a != b,
            (Object::Null, Object::Null) => false,
            _ => true,
        };
        self.heap.temp_release(mark);
        self.boolean(res)
    }

    pub fn visit_add(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_mul should only be called with Node::BinaryOp"),
        };

        let (mark, left_node, right_node) = self.eval_operands(op);

        let lval = self.extract_number(&left_node);
        let rval = self.extract_number(&right_node);

        let out = match (lval, rval) {
            (Ok(lval), Ok(rval)) => self.number(lval + rval),
            (_, _) => self.emit_type_error(
                format!("expected a number type"),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    pub fn visit_sub(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_sub should only be called with Node::BinaryOp"),
        };
        let (mark, left_node, right_node) = self.eval_operands(op);

        let lval = self.extract_number(&left_node);
        let rval = self.extract_number(&right_node);

        let out = match (lval, rval) {
            (Ok(lval), Ok(rval)) => self.number(lval - rval),
            (_, _) => self.emit_type_error(
                format!("expected a number type"),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    pub fn visit_mul(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_mul should only be called with Node::BinaryOp"),
        };
        let (mark, left_node, right_node) = self.eval_operands(op);

        let lval = self.extract_number(&left_node);
        let rval = self.extract_number(&right_node);

        let out = match (lval, rval) {
            (Ok(lval), Ok(rval)) => self.number(lval * rval),
            (_, _) => self.emit_type_error(
                format!("expected a number type"),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    pub fn visit_div(&mut self, node: &Node) -> Cell<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!("visit_div should only be called with Node::BinaryOp"),
        };

        let (mark, left_node, right_node) = self.eval_operands(op);

        let lval = self.extract_number(&left_node);
        let rval = self.extract_number(&right_node);

        let out = match (lval, rval) {
            (Ok(lval), Ok(rval)) => {
                if rval == 0 {
                    return self.emit_division_by_zero_error(NodeParseInfo {
                        lineno: op.lineno,
                        token: op.token.clone(),
                    });
                }
                self.number(lval / rval)
            }
            (_, _) => self.emit_type_error(
                format!("expected a number type"),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    fn resolve_func_expr(
        &mut self,
        func_type: &FunctionType,
    ) -> Result<Cell<Object>, &'static str> {
        match func_type {
            FunctionType::Expr(func) => {
                let f_ret = self.visit_func_expr(func);
                match f_ret.as_ref() {
                    Object::Number(_) => Ok(f_ret.clone()),
                    _ => Err("Cannot not add type"),
                }
            }
            _ => Err("Unexpected function declaration found"),
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
                span: (offset as usize, parse_info.token.lexeme.len() as usize).into(),
            })),
        }))
    }

    fn emit_type_error(&mut self, msg: String, parse_info: NodeParseInfo) -> Cell<Object> {
        let (err_type, error_msg) = match parse_info.token.ttype {
            TType::Num => (
                ErrType::TypeError("cannot use a number here".to_owned()),
                format!(
                    "Type Error at line {}: {}\nOffending number: {}",
                    parse_info.lineno, msg, parse_info.token.lexeme
                ),
            ),
            TType::Plus => (
                ErrType::TypeError("Oops can't add incompatible types".to_owned()),
                format!("Type Error at line {}: {}\n", parse_info.lineno, msg),
            ),
            _ => (
                ErrType::TypeError(msg.clone()),
                format!("Type Error at line {}: {}\n", parse_info.lineno, msg),
            ),
        };
        let offset = parse_info.token.offset;
        self.heap.allocate_cell(Object::Error(ErrorObj {
            type_error: Some(TypeError::TypeMismatch(ErrInfo {
                err_type,
                msg: error_msg,
                src: self.parser.source(),
                span: (offset as usize, parse_info.token.lexeme.len() as usize).into(),
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
                span: (offset as usize, parse_info.token.lexeme.len() as usize).into(),
            })),
        }))
    }

    fn emit_undefined_error(&mut self, msg: String, parse_info: NodeParseInfo) -> Cell<Object> {
        let offset = parse_info.token.offset;
        self.heap.allocate_cell(Object::Error(ErrorObj {
            run_time_error: Some(RuntimeError::UndefinedIdentifier(ErrInfo {
                err_type: ErrType::RuntimeError(
                    "this identifier may have not been declared".to_string(),
                ),
                msg,
                src: self.parser.source(),
                span: (offset as usize, parse_info.token.lexeme.len() as usize).into(),
            })),
            type_error: None,
        }))
    }

    fn emit_invalid_function_call_error(
        &mut self,
        msg: String,
        parse_info: NodeParseInfo,
    ) -> Cell<Object> {
        let offset = parse_info.token.offset;
        self.heap.allocate_cell(Object::Error(ErrorObj {
            run_time_error: Some(RuntimeError::InvalidFunctionCall(ErrInfo {
                err_type: ErrType::RuntimeError("Invalid function call".to_string()),
                msg,
                src: self.parser.source(),
                span: (offset as usize, parse_info.token.lexeme.len() as usize).into(),
            })),
            type_error: None,
        }))
    }

    fn extract_number(&mut self, obj: &Cell<Object>) -> Result<i32, &'static str> {
        match obj.as_ref() {
            Object::Number(val) => Ok(*val),
            Object::Function(func_type) => {
                let ret = self.resolve_func_expr(func_type)?;
                match ret.as_ref() {
                    Object::Number(v) => Ok(*v),
                    _ => unreachable!("Unexpected code path"),
                }
            }
            Object::Error(obj) => {
                obj.visit();
                Err("expected a number type")
            }
            _ => Err("expected a number type"),
        }
    }
}

pub struct NodeParseInfo {
    pub lineno: i32,
    pub token: Token,
}
