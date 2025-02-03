use miette::{Diagnostic, SourceSpan};

/*
    Symbol objects for beesafe
*/
use crate::environment::Environment;
use thiserror::Error;

#[derive(Debug)]
pub enum Object {
    Null,
    Bool(bool),
    Error(ErrorObj),
    Number(i32),
    String(String),
    Float(f32),
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
}

#[derive(Debug)]
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
                    miette::Report::new(info.clone()).with_source_code(info.src.clone())
                ),
                TypeError::PlaceHolder(info) => eprint!(
                    "{:?}",
                    miette::Report::new(info.clone()).with_source_code(info.src.clone())
                ),
            },
            None => (),
        }

        match self.run_time_error.clone() {
            Some(err) => match err {
                RuntimeError::DivisionByZero(info) => eprint!(
                    "{:?}",
                    miette::Report::new(info.clone()).with_source_code(info.src.clone())
                ),
            },
            None => (),
        }
        return;
    }
}

impl NumberObj {
    pub fn visit<'e>(&self, env: &'e Box<Environment>) -> i32 {
        self.value
    }
}

impl StringObj {
    pub fn visit<'e>(&self, env: &'e Box<Environment>) -> String {
        self.literal.clone()
    }
}

impl BoolObj {
    pub fn visit<'e>(&self, env: &'e Box<Environment>) -> bool {
        self.value
    }
}
impl std::fmt::Display for ErrInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}
