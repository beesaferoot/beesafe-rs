use crate::allocator::{Allocator, CellView, Heap};
use crate::builtins::BUILTINS;
use crate::ast::*;
use crate::environment::Environment;
use crate::lexer::TType;
use crate::lexer::Token;
use crate::parser::Parser;
use crate::symbols::*;
use miette::SourceSpan;

// Integers in this range are interned rather than allocated.
const SMALL_INT_MIN: i32 = -128;
const SMALL_INT_MAX: i32 = 1024;

pub struct Executor<'l> {
    global_env: &'l mut Environment,
    heap: Heap<Object>,
    parser: &'l Parser<'l>,
    null: CellView<Object>,
    booleans: [CellView<Object>; 2],
    small_ints: Vec<CellView<Object>>,
}

impl<'l> Executor<'l> {
    pub fn new(env: &'l mut Environment, parser: &'l Parser<'l>) -> Self {
        let mut heap = Heap::new();
        // BEESAFE_GC_THRESHOLD overrides the collection threshold. Program output
        // must not depend on it.
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
        let mut executor = Self {
            global_env: env,
            heap,
            parser,
            null,
            booleans,
            small_ints,
        };
        for builtin in BUILTINS {
            let cell = executor.heap.intern(Object::Builtin(builtin));
            executor.global_env.add(builtin.name, cell);
        }
        executor
    }

    // Evaluates both operands, keeping the left rooted while the right is
    // evaluated. Returns the shadow-stack mark for the caller to release.
    fn eval_operands(&mut self, op: &BinaryOp) -> (usize, CellView<Object>, CellView<Object>) {
        let mark = self.heap.temp_mark();
        let left = self.visit_expr(&op.left);
        self.heap.temp_push(left);
        let right = self.visit_expr(&op.right);
        self.heap.temp_push(right);
        (mark, left, right)  
    }

    pub fn read_value(&self, view: CellView<Object>) -> &Object {
        self.read(view)
    }

    pub fn null_value(&self) -> CellView<Object> {
        self.null()
    }

    pub fn number_value(&mut self, value: i32) -> CellView<Object> {
        self.number(value)
    }

    pub fn display_object(&self, view: CellView<Object>) -> Option<String> {
        self.resolve_display_object(&view)
    }

    pub fn builtin_type_error(&mut self, msg: &str, at: &NodeParseInfo) -> CellView<Object> {
        self.emit_type_error(
            msg.to_string(),
            NodeParseInfo {
                lineno: at.lineno,
                token: at.token.clone(),
            },
        )
    }

    fn read(&self, view: CellView<Object>) -> &Object {
        self.heap
            .read_cell(view)
            .expect("read of a cell that has been freed")
    }

    fn null(&self) -> CellView<Object> {
        self.null
    }

    fn boolean(&self, value: bool) -> CellView<Object> {
        self.booleans[value as usize]
    }

    fn number(&mut self, value: i32) -> CellView<Object> {
        if (SMALL_INT_MIN..=SMALL_INT_MAX).contains(&value) {
            return self.small_ints[(value - SMALL_INT_MIN) as usize];
        }
        self.heap.allocate_cell(Object::Number(value))
    }

    // Returns the interned cell for a constant, or None for a value that must be
    // copied. Sharing is safe because cells are never mutated in place.
    fn constant(&mut self, obj: &Object) -> Option<CellView<Object>> {
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

    pub fn read_view(&self, view: CellView<Object>) -> Option<&Object> {
        self.heap.read_cell(view)
    }

    pub fn visit_program(&mut self, program: &Program) -> Vec<CellView<Object>> {
        let mut result = Vec::new();
        for stmt in &program.statements {
            let mark = self.heap.temp_mark();
            let cell = self.visit_expr(stmt);
            self.heap.add_root(cell);
            result.push(cell);
            // Bounds any temporary root leaked by an early return to one statement.
            self.heap.temp_release(mark);
        }
        self.heap.force_gc();
        result
    }

    pub fn visit_expr(&mut self, node: &Node) -> CellView<Object> {
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
                let start_cell = self.visit_expr(&range.start);
                let start = match self.read(start_cell) {
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
                let end_cell = self.visit_expr(&range.end);
                let end = match self.read(end_cell) {
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
            Node::UniaryOp(op) => self.visit_unary(op),
            Node::Index(index) => self.visit_index(index),
            Node::Return(ret) => {
                let mark = self.heap.temp_mark();
                let value = self.visit_expr(&ret.value);
                self.heap.temp_push(value);
                let wrapped = self.heap.allocate_cell(Object::Return(value));
                self.heap.temp_release(mark);
                wrapped
            }
            // A node the parser rejected. Carry its diagnostic through as a value
            // rather than evaluating a tree that is known to be wrong.
            Node::Error(err) => {
                let msg = format!("{}", err.error_type);
                self.emit_runtime_error(msg, err.span)
            }
            other => {
                let span = node_span(other).unwrap_or_else(|| (0, 0).into());
                self.emit_runtime_error(
                    format!("unsupported construct: {}", node_name(other)),
                    span,
                )
            }
        }
    }

    fn visit_block(&mut self, block: &Block) -> CellView<Object> {
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
                    let unwind = matches!(self.read(r), Object::Return(_) | Object::Error(_));
                    if unwind {
                        return r;
                    }
                }
            }
        }
        self.null()
    }

    fn visit_for(&mut self, forstmt: &ForStmt) -> CellView<Object> {
        let mark = self.heap.temp_mark();
        let iter_cell = self.visit_expr(&forstmt.iter);
        self.heap.temp_push(iter_cell);
        let iterator = self.read(iter_cell).get_iterator(&self.heap);
        let mut iterator = match iterator {
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
                let body = self.visit_expr(&forstmt.block);
                if matches!(self.read(body), Object::Return(_) | Object::Error(_)) {
                    self.heap.temp_release(mark);
                    return body;
                }
            } else {
                break;
            }
        }
        self.heap.temp_release(mark);
        self.null()
    }

    fn visit_declare(&mut self, decl: &Declare) -> CellView<Object> {
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

    fn visit_init(&mut self, init: &Init) -> CellView<Object> {
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

    fn visit_ident(&mut self, id: &Ident) -> CellView<Object> {
        match self.global_env.get(&id.value) {
            Some(cell) => {
                let obj = self.read(cell).clone();
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

    fn visit_assign(&mut self, node: &Node) -> CellView<Object> {
        let assign = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };

        let mark = self.heap.temp_mark();
        let value_cell = self.visit_expr(&assign.right);
        self.heap.temp_push(value_cell);

        let out = match assign.left.as_ref() {
            Node::Ident(id) => self.assign_to_name(id, value_cell),
            Node::Index(index) => self.assign_to_index(index, value_cell),
            other => self.emit_type_error(
                "left side of an assignment must be a name or an index".to_string(),
                NodeParseInfo {
                    lineno: assign.lineno,
                    token: node_token(other).unwrap_or_else(|| assign.token.clone()),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    fn assign_to_name(&mut self, id: &Ident, value_cell: CellView<Object>) -> CellView<Object> {
        if self.global_env.get(&id.value).is_none() {
            return self.emit_undefined_error(
                format!("undefined identifier '{}'", id.value),
                NodeParseInfo {
                    lineno: id.lineno,
                    token: id.token.clone(),
                },
            );
        }
        self.heap.add_root(value_cell);
        if let Some(prev) = self.global_env.assign(&id.value, value_cell) {
            self.heap.remove_root(&prev);
        }
        self.null()
    }

    fn assign_to_index(&mut self, index: &Index, value_cell: CellView<Object>) -> CellView<Object> {
        let (target, offset) = match self.resolve_index(index) {
            Ok(resolved) => resolved,
            Err(error) => return error,
        };
        // The array owns the element view from here, so root it before the old
        // one is dropped.
        self.heap.add_root(value_cell);
        let previous = match self.heap.read_mut_cell(target) {
            Some(Object::Array(elements)) => std::mem::replace(&mut elements[offset], value_cell),
            _ => unreachable!("resolve_index already checked the target is an array"),
        };
        self.heap.remove_root(&previous);
        self.null()
    }

    // Resolves an index expression to the position it names, so both reading and
    // assigning can share the bounds checking.
    fn resolve_index(&mut self, index: &Index) -> Result<(CellView<Object>, usize), CellView<Object>> {
        let mark = self.heap.temp_mark();
        // Resolve a named target to its binding rather than evaluating it:
        // visit_ident hands back a copy, and writing to a copy is a silent no-op.
        let target = match index.object.as_ref() {
            Node::Ident(id) => match self.global_env.get(&id.value) {
                Some(cell) => cell,
                None => {
                    return Err(self.emit_undefined_error(
                        format!("undefined identifier '{}'", id.value),
                        NodeParseInfo {
                            lineno: id.lineno,
                            token: id.token.clone(),
                        },
                    ))
                }
            },
            other => self.visit_expr(other),
        };
        self.heap.temp_push(target);
        let position = self.visit_expr(&index.index);
        self.heap.temp_push(position);

        let info = || NodeParseInfo {
            lineno: index.lineno,
            token: index.token.clone(),
        };

        let offset = match self.read(position) {
            Object::Number(value) => *value,
            _ => {
                self.heap.temp_release(mark);
                return Err(self.emit_type_error("index must be a number".to_string(), info()));
            }
        };
        let length = match self.read(target) {
            Object::Array(elements) => elements.len(),
            _ => {
                self.heap.temp_release(mark);
                return Err(self.emit_type_error("object is not indexable".to_string(), info()));
            }
        };

        self.heap.temp_release(mark);
        if offset < 0 || offset as usize >= length {
            return Err(self.emit_index_error(offset, length, info()));
        }
        Ok((target, offset as usize))
    }

    fn visit_index(&mut self, index: &Index) -> CellView<Object> {
        let (target, offset) = match self.resolve_index(index) {
            Ok(resolved) => resolved,
            Err(error) => return error,
        };
        match self.read(target) {
            Object::Array(elements) => elements[offset],
            _ => unreachable!("resolve_index already checked the target is an array"),
        }
    }

    fn visit_unary(&mut self, op: &UniaryOp) -> CellView<Object> {
        let mark = self.heap.temp_mark();
        let operand = self.visit_expr(&op.right);
        self.heap.temp_push(operand);

        // Fold to an owned value first: the heap borrow has to end before an
        // error can be allocated.
        let folded = match (op.token.ttype, self.read(operand)) {
            // wrapping_neg so the most negative integer does not panic.
            (TType::Minus, Object::Number(value)) => Some(Object::Number(value.wrapping_neg())),
            (TType::Bang, Object::Bool(value)) => Some(Object::Bool(!value)),
            _ => None,
        };

        let out = match folded {
            Some(value) => match self.constant(&value) {
                Some(interned) => interned,
                None => self.heap.allocate_cell(value),
            },
            None => self.emit_type_error(
                format!("cannot apply '{}' to this type", op.token.lexeme),
                NodeParseInfo {
                    lineno: op.lineno,
                    token: op.token.clone(),
                },
            ),
        };
        self.heap.temp_release(mark);
        out
    }

    fn visit_func_expr(&mut self, func: &FunctionExpr) -> CellView<Object> {
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

    fn visit_func_anon(&mut self, func: &Function) -> CellView<Object> {
        self.heap
            .allocate_cell(Object::Function(FunctionType::Decl(func.clone())))
    }

    fn visit_func_call(&mut self, func_call: &Call) -> CellView<Object> {
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

        let mark = self.heap.temp_mark();
        let callee_cell = self.visit_expr(callee_node);
        self.heap.temp_push(callee_cell);

        // Decide before constructing an error, so the heap borrow is over by then.
        let callee = self.read(callee_cell);
        let is_callable = matches!(callee, Object::Function(_) | Object::Builtin(_));
        let is_error = matches!(callee, Object::Error(_));
        if is_error {
            return callee_cell;
        }
        if !is_callable {
            return self.emit_undefined_error(
                "function is not defined".to_string(),
                NodeParseInfo {
                    lineno: func_call.lineno,
                    token: func_call.token.clone(),
                },
            );
        }

        let mut arg_cells: Vec<CellView<Object>> = Vec::with_capacity(func_call.args.len());
        for arg in &func_call.args {
            let cell = self.visit_expr(arg);
            self.heap.temp_push(cell);
            arg_cells.push(cell);
        }

        // Builtins run in the caller's scope: there are no parameters to bind.
        if let Object::Builtin(builtin) = self.read(callee_cell) {
            let builtin = *builtin;
            let at = NodeParseInfo {
                lineno: func_call.lineno,
                token: func_call.token.clone(),
            };
            let result = if builtin.arity.accepts(arg_cells.len()) {
                (builtin.call)(self, &arg_cells, &at)
            } else {
                self.emit_type_error(
                    format!(
                        "{} expects {} argument(s), got {}",
                        builtin.name,
                        builtin.arity.describe(),
                        arg_cells.len()
                    ),
                    NodeParseInfo {
                        lineno: func_call.lineno,
                        token: func_call.token.clone(),
                    },
                )
            };
            self.heap.temp_push(result);
            self.heap.temp_release(mark);
            return result;
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

        let callee_obj = self.read(callee_cell).clone();
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
                let value = match self.read(result) {
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

    fn resolve_display_object(&self, obj: &CellView<Object>) -> Option<String> {
        match self.read(*obj) {
            Object::Null => Some("null".to_string()),
            Object::Bool(b) => Some(format!("{}", b)),
            Object::Number(n) => Some(format!("{}", n)),
            Object::String(s) => Some(s.clone()),
            Object::Error(_) => None,
            Object::Return(ret) => self.resolve_display_object(ret),
            other => Some(format!("{:?}", other)),
        }
    }

    fn visit_if(&mut self, ifs: &If) -> CellView<Object> {
        let pred = self.visit_expr(&ifs.predicate);
        let taken = match self.read(pred) {
            Object::Bool(value) => Some(*value),
            _ => None,
        };
        match taken {
            Some(true) => self.visit_expr(&ifs.block),
            Some(false) => {
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

    fn visit_while(&mut self, w: &While) -> CellView<Object> {
        loop {
            let pred = self.visit_expr(&w.predicate);
            let keep_going = match self.read(pred) {
                Object::Bool(value) => Some(*value),
                _ => None,
            };
            match keep_going {
                Some(true) => {
                    // A return or an error in the body unwinds the loop, the
                    // same way visit_block stops at one.
                    let body = self.visit_expr(&w.block);
                    if matches!(self.read(body), Object::Return(_) | Object::Error(_)) {
                        return body;
                    }
                }
                Some(false) => break,
                None => {
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

    pub fn visit_lt(&mut self, node: &Node) -> CellView<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let operands = match (self.read(l), self.read(r)) {
            (Object::Number(a), Object::Number(b)) => Some((*a, *b)),
            _ => None,
        };
        let out = match operands {
            Some((a, b)) => self.boolean(a < b),
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

    pub fn visit_lte(&mut self, node: &Node) -> CellView<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let operands = match (self.read(l), self.read(r)) {
            (Object::Number(a), Object::Number(b)) => Some((*a, *b)),
            _ => None,
        };
        let out = match operands {
            Some((a, b)) => self.boolean(a <= b),
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

    pub fn visit_gt(&mut self, node: &Node) -> CellView<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let operands = match (self.read(l), self.read(r)) {
            (Object::Number(a), Object::Number(b)) => Some((*a, *b)),
            _ => None,
        };
        let out = match operands {
            Some((a, b)) => self.boolean(a > b),
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

    pub fn visit_gte(&mut self, node: &Node) -> CellView<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let operands = match (self.read(l), self.read(r)) {
            (Object::Number(a), Object::Number(b)) => Some((*a, *b)),
            _ => None,
        };
        let out = match operands {
            Some((a, b)) => self.boolean(a >= b),
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

    pub fn visit_eq(&mut self, node: &Node) -> CellView<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let res = match (self.read(l), self.read(r)) {
            (Object::Number(a), Object::Number(b)) => a == b,
            (Object::Bool(a), Object::Bool(b)) => a == b,
            (Object::String(a), Object::String(b)) => a == b,
            (Object::Null, Object::Null) => true,
            _ => false,
        };
        self.heap.temp_release(mark);
        self.boolean(res)
    }

    pub fn visit_neq(&mut self, node: &Node) -> CellView<Object> {
        let op = match node {
            Node::BinaryOp(op) => op,
            _ => unreachable!(),
        };
        let (mark, l, r) = self.eval_operands(op);
        let res = match (self.read(l), self.read(r)) {
            (Object::Number(a), Object::Number(b)) => a != b,
            (Object::Bool(a), Object::Bool(b)) => a != b,
            (Object::String(a), Object::String(b)) => a != b,
            (Object::Null, Object::Null) => false,
            _ => true,
        };
        self.heap.temp_release(mark);
        self.boolean(res)
    }

    pub fn visit_add(&mut self, node: &Node) -> CellView<Object> {
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

    pub fn visit_sub(&mut self, node: &Node) -> CellView<Object> {
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

    pub fn visit_mul(&mut self, node: &Node) -> CellView<Object> {
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

    pub fn visit_div(&mut self, node: &Node) -> CellView<Object> {
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
    ) -> Result<CellView<Object>, &'static str> {
        match func_type {
            FunctionType::Expr(func) => {
                let f_ret = self.visit_func_expr(func);
                match self.read(f_ret) {
                    Object::Number(_) => Ok(f_ret.clone()),
                    _ => Err("Cannot not add type"),
                }
            }
            _ => Err("Unexpected function declaration found"),
        }
    }

    fn emit_recursion_limit_reached_error(&mut self, parse_info: NodeParseInfo) -> CellView<Object> {
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

    /// Builds an error value from a span directly, for nodes that carry no token.
    fn emit_runtime_error(&mut self, msg: String, span: SourceSpan) -> CellView<Object> {
        self.heap.allocate_cell(Object::Error(ErrorObj {
            type_error: None,
            run_time_error: Some(RuntimeError::InvalidFunctionCall(ErrInfo {
                err_type: ErrType::RuntimeError(msg.clone()),
                msg,
                src: self.parser.source(),
                span,
            })),
        }))
    }

    fn emit_index_error(
        &mut self,
        offset: i32,
        length: usize,
        parse_info: NodeParseInfo,
    ) -> CellView<Object> {
        self.emit_type_error(
            format!("index {} is out of bounds for length {}", offset, length),
            parse_info,
        )
    }

    fn emit_type_error(&mut self, msg: String, parse_info: NodeParseInfo) -> CellView<Object> {
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

    fn emit_division_by_zero_error(&mut self, parse_info: NodeParseInfo) -> CellView<Object> {
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

    fn emit_undefined_error(&mut self, msg: String, parse_info: NodeParseInfo) -> CellView<Object> {
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
    ) -> CellView<Object> {
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

    fn extract_number(&mut self, obj: &CellView<Object>) -> Result<i32, &'static str> {
        match self.read(*obj).clone() {
            Object::Number(val) => Ok(val),
            Object::Function(func_type) => {
                let ret = self.resolve_func_expr(&func_type)?;
                match self.read(ret) {
                    Object::Number(v) => Ok(*v),
                    _ => Err("expected a number type"),
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

fn node_span(node: &Node) -> Option<SourceSpan> {
    node_token(node).map(|token| (token.offset as usize, token.lexeme.len()).into())
}

fn node_name(node: &Node) -> &'static str {
    match node {
        Node::Program(_) => "program",
        Node::Block(_) => "block",
        Node::Return(_) => "return",
        Node::Error(_) => "parse error",
        _ => "expression",
    }
}

fn node_token(node: &Node) -> Option<Token> {
    match node {
        Node::Ident(id) => Some(id.token.clone()),
        Node::Index(index) => Some(index.token.clone()),
        Node::Number(number) => Some(number.token.clone()),
        Node::Return(ret) => Some(ret.token.clone()),
        Node::While(node) => Some(node.token.clone()),
        Node::If(node) => Some(node.token.clone()),
        Node::For(node) => Some(node.token.clone()),
        Node::UniaryOp(node) => Some(node.token.clone()),
        Node::BinaryOp(node) => Some(node.token.clone()),
        Node::Boolean(node) => Some(node.token.clone()),
        Node::StringLiteral(node) => Some(node.token.clone()),
        _ => None,
    }
}

pub struct NodeParseInfo {
    pub lineno: i32,
    pub token: Token,
}
