use miette::{Diagnostic, SourceSpan};

/*
    Symbol objects for beesafe
*/
use crate::{
    ast::{Function as Func, FunctionExpr as FuncExpr},
    environment::Environment,
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
    Array(Vec<Object>),
    Range(RangeObj),
    Iterator(IteratorObj),
    Function(FunctionType),
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
    pub src: &'static str,
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
}

#[derive(Clone, Debug)]
pub enum RuntimeError {
    DivisionByZero(ErrInfo),
    RecursionLimitReached(ErrInfo),
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
    pub fn visit<'e>(&self) {
        match self.type_error.clone() {
            Some(err) => match err {
                TypeError::TypeMismatch(info) => eprint!(
                    "{:?}",
                    miette::Report::new(info.clone()).with_source_code(info.src)
                ),
                TypeError::PlaceHolder(info) => eprint!(
                    "{:?}",
                    miette::Report::new(info.clone()).with_source_code(info.src)
                ),
            },
            None => (),
        }

        match self.run_time_error.clone() {
            Some(err) => match err {
                RuntimeError::DivisionByZero(info) => eprint!(
                    "{:?}",
                    miette::Report::new(info.clone()).with_source_code(info.src)
                ),
                RuntimeError::RecursionLimitReached(info) => eprint!(
                    "{:?}",
                    miette::Report::new(info.clone()).with_source_code(info.src)
                ),
            },
            None => (),
        }
        return;
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

    pub fn get_iterator(&self) -> Option<IteratorObj> {
        match self {
            Object::Array(arr) => Some(IteratorObj {
                current: 0,
                items: arr.clone(),
            }),
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
