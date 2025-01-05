/*
    Symbol objects for beesafe
*/
use crate::environment::Environment;
use crate::parser::ParseError;

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
    TypeMismatch(String),
    PlaceHolder(String),
}

#[derive(Clone, Debug)]
pub enum RuntimeError {
    DivisionByZero,
}

#[derive(Debug)]
pub struct ErrorObj {
    pub parse_error: Option<ParseError>,
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
    pub fn visit<'e>(&self) -> String {
        let mut err_str = match self.parse_error.clone() {
            Some(err) => match err {
                ParseError::MissingIdent(err_msg) => err_msg,
                ParseError::UndeterminedType(err_msg) => err_msg,
                ParseError::InvalidSyntax(err_msg) => err_msg,
                ParseError::PlaceHolder(_) => String::from(""),
            },
            None => String::from(""),
        };
        if err_str.is_empty() {
            err_str = match self.type_error.clone() {
                Some(err) => match err {
                    TypeError::TypeMismatch(err_msg) => err_msg.to_owned(),
                    TypeError::PlaceHolder(err_msg) => err_msg.to_owned(),
                },
                None => String::from(""),
            }
        }

        if err_str.is_empty() {
            err_str = match self.run_time_error.clone() {
                Some(err) => match err {
                    RuntimeError::DivisionByZero => String::from("Division by zero not allowed"),
                },
                None => String::from(""),
            }
        }
        err_str
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
