use miette::{Diagnostic, SourceSpan};

/*
    Symbol objects for beesafe
*/
use crate::{
    allocator::{Allocator, CellView, Heap, Markable},
    ast::{Function as Func, FunctionExpr as FuncExpr},
    environment::Environment,
    source::{SourceId, SourceMap},
};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Object {
    Null,
    Bool(bool),
    Error(ErrorObj),
    Number(i32),
    String(String),
    Float(f32),
    Array(Vec<CellView<Object>>),
    Range(RangeObj),
    Iterator(IteratorObj),
    Function(FunctionType),
    Builtin(&'static crate::builtins::Builtin),
    Return(CellView<Object>),
}

#[derive(Debug, Clone)]
pub enum FunctionType {
    Decl(Func),
    Expr(FuncExpr),
}
#[derive(Debug, Clone)]
pub struct RangeObj {
    pub start: i32,
    pub end: i32,
}

#[derive(Debug, Clone)]
pub struct IteratorObj {
    pub current: usize,
    pub items: Vec<Object>,
}

#[derive(Debug)]
pub struct NumberObj {
    pub value: i32,
}

#[derive(Clone, Debug)]
pub enum TypeError {
    TypeMismatch(ErrInfo),
    PlaceHolder(ErrInfo),
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub struct ErrInfo {
    pub src: SourceId,
    pub msg: String,
    #[label("{err_type}")]
    pub span: SourceSpan,
    pub err_type: ErrType,
}

#[derive(Clone, Debug, Error)]
pub enum ErrType {
    #[error("Type Error: {0}")]
    TypeError(String),
    #[error("Runtime Error: {0}")]
    RuntimeError(String),
    #[error("Undefined Error: {0}")]
    UndefinedError(String),
}

#[derive(Clone, Debug)]
pub enum RuntimeError {
    DivisionByZero(ErrInfo),
    RecursionLimitReached(ErrInfo),
    InvalidFunctionCall(ErrInfo),
    UndefinedIdentifier(ErrInfo),
}

#[derive(Debug, Clone)]
pub struct ErrorObj {
    // pub parse_error: Option<ParseError>,
    pub type_error: Option<TypeError>,
    pub run_time_error: Option<RuntimeError>,
}

#[derive(Debug)]
pub struct StringObj {
    pub literal: String,
}

#[derive(Debug)]
pub struct BoolObj {
    pub value: bool,
}

impl ErrorObj {
    pub fn visit(&self, sources: &SourceMap) {
        if let Some(info) = self.type_error.as_ref().map(TypeError::info) {
            render(info, sources);
        }
        if let Some(info) = self.run_time_error.as_ref().map(RuntimeError::info) {
            render(info, sources);
        }
    }
}

fn render(info: &ErrInfo, sources: &SourceMap) {
    let text = sources.text(info.src).to_string();
    eprint!(
        "{:?}",
        miette::Report::new(info.clone()).with_source_code(text)
    );
}

impl TypeError {
    fn info(&self) -> &ErrInfo {
        match self {
            TypeError::TypeMismatch(info) | TypeError::PlaceHolder(info) => info,
        }
    }
}

impl RuntimeError {
    fn info(&self) -> &ErrInfo {
        match self {
            RuntimeError::DivisionByZero(info)
            | RuntimeError::RecursionLimitReached(info)
            | RuntimeError::InvalidFunctionCall(info)
            | RuntimeError::UndefinedIdentifier(info) => info,
        }
    }
}

impl NumberObj {
    pub fn visit<'e>(&self, _env: &'e Box<Environment>) -> i32 {
        self.value
    }
}

impl StringObj {
    pub fn visit<'e>(&self, _env: &'e Box<Environment>) -> String {
        self.literal.clone()
    }
}

impl BoolObj {
    pub fn visit<'e>(&self, _env: &'e Box<Environment>) -> bool {
        self.value
    }
}
impl std::fmt::Display for ErrInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Object {
    pub fn is_iterable(&self) -> bool {
        match self {
            Object::Array(_) => true,
            Object::Range(_) => true,
            Object::String(_) => true,
            _ => false,
        }
    }

    pub fn get_iterator(&self, heap: &Heap<Object>) -> Option<IteratorObj> {
        match self {
            Object::Array(cells) => {
                let mut items = Vec::new();
                for cell in cells {
                    match heap.read_cell(*cell) {
                        Some(value) => items.push(value.clone()),
                        None => return None,
                    }
                }
                Some(IteratorObj { current: 0, items })
            }
            Object::Range(range) => {
                let mut items = Vec::new();
                for i in range.start..=range.end {
                    items.push(Object::Number(i));
                }
                Some(IteratorObj { current: 0, items })
            }
            Object::String(s) => {
                let mut items = Vec::new();
                for ch in s.chars() {
                    items.push(Object::String(ch.to_string()));
                }
                Some(IteratorObj { current: 0, items })
            }
            _ => None,
        }
    }
}

impl IteratorObj {
    pub fn has_next(&self) -> bool {
        self.current < self.items.len()
    }

    pub fn next(&mut self) -> Option<Object> {
        if self.has_next() {
            let item = self.items[self.current].clone();
            self.current += 1;
            Some(item)
        } else {
            None
        }
    }
}

/// Renders a value for `print`. `None` for an error, which the caller surfaces
/// as a diagnostic rather than as text.
/// Nesting deeper than this renders as `[...]`.
const DISPLAY_DEPTH: usize = 8;

pub fn display(heap: &Heap<Object>, view: CellView<Object>) -> Option<String> {
    display_at(heap, view, DISPLAY_DEPTH)
}

fn display_at(heap: &Heap<Object>, view: CellView<Object>, depth: usize) -> Option<String> {
    match heap.read_cell(view)? {
        Object::Null => Some("null".to_string()),
        Object::Bool(value) => Some(format!("{}", value)),
        Object::Number(value) => Some(format!("{}", value)),
        Object::Float(value) => Some(format!("{}", value)),
        Object::String(text) => Some(text.clone()),
        Object::Range(range) => Some(format!("{}..{}", range.start, range.end)),
        Object::Iterator(_) => Some("<iterator>".to_string()),
        Object::Function(kind) => Some(match kind {
            FunctionType::Expr(function) => match function_name(&function.name) {
                Some(name) => format!("<function {}>", name),
                None => "<function>".to_string(),
            },
            FunctionType::Decl(_) => "<function>".to_string(),
        }),
        Object::Builtin(builtin) => Some(format!("<builtin {}>", builtin.name)),
        Object::Array(elements) => {
            if depth == 0 {
                return Some("[...]".to_string());
            }
            let mut parts = Vec::with_capacity(elements.len());
            for element in elements {
                parts.push(display_at(heap, *element, depth - 1)?);
            }
            Some(format!("[{}]", parts.join(", ")))
        }
        // Errors are reported as diagnostics, never rendered as text.
        Object::Error(_) => None,
        Object::Return(inner) => display_at(heap, *inner, depth),
    }
}

fn function_name(node: &crate::ast::Node) -> Option<String> {
    match node {
        crate::ast::Node::Ident(id) => Some(id.value.clone()),
        _ => None,
    }
}

impl Markable<Object> for Object {
    // No wildcard arm: adding a variant that holds a Cell must not silently
    // default to "has no references", which would sweep it while live.
    fn get_references(&self) -> Vec<CellView<Object>> {
        match self {
            Object::Array(elements) => elements.clone(),
            Object::Return(cell) => vec![*cell],
            Object::Iterator(it) => it.items.iter().flat_map(|o| o.get_references()).collect(),
            Object::Null
            | Object::Bool(_)
            | Object::Error(_)
            | Object::Number(_)
            | Object::String(_)
            | Object::Float(_)
            | Object::Range(_)
            | Object::Function(_)
            | Object::Builtin(_) => Vec::new(),
        }
    }
}
