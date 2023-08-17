use std::rc::Rc;

/*
    Symbol objects for beesafe 
*/
use crate::parser::ParseError;
use crate::environment::Environment;

pub enum Object {
    NullObj(NullObj),
    BoolObj(BoolObj),
    ErrorObj(ErrorObj),
    NumberObj(NumberObj),
    StringObj(StringObj)
}

#[derive(PartialEq, Debug)]
pub enum Type {
    Number, 
    Bool,
    Null,
    Return,
    Error,
    Function,
    String,
    Range,
    Builtin,
    BuiltinFunction,
}

pub struct NullObj {}
pub struct NumberObj {
    pub value: i32
}


#[derive(Clone)]
pub enum TypeError {
    TypeMismatch(String),
    PlaceHolder(String),
}

pub struct ErrorObj {
    pub parse_error: Option<ParseError>,
    pub type_error: Option<TypeError>
}

pub struct StringObj {
    pub literal: String
}

pub struct BoolObj {
    pub value: bool
}


impl ErrorObj {

    pub fn visit<'e>(&self) -> String {
        let mut err_str = match self.parse_error.clone() {
            Some(err) => match err {
                ParseError::MissingIdent(err_msg) => err_msg,
                ParseError::UndeterminedType(err_msg) => err_msg,
                ParseError::PlaceHolder(_) => String::from("")
            }, 
            None => String::from("")
        };
        if err_str.is_empty() {
            err_str = match self.type_error.clone() {
                Some(err) => match err {
                    TypeError::TypeMismatch(err_msg) => err_msg.to_owned(),
                    TypeError::PlaceHolder(err_msg) => err_msg.to_owned()
                },
                None => String::from("")
            }
        }
        return err_str

    }

    fn ttype(&self) -> Type {
        Type::Error
    }
}

impl NumberObj {
    
    pub fn visit<'e>(&self, env: &'e Rc<Environment>) -> i32 {
        self.value
    }

    fn ttype(&self) -> Type {
        Type::Number
    }
}

impl StringObj {
    pub fn visit<'e>(&self, env: &'e Rc<Environment>) -> String {
        self.literal.clone()
    }

    fn ttype(&self) -> Type {
        Type::String
    }
}

impl BoolObj {
    pub fn visit<'e>(&self, env: &'e Rc<Environment>) -> bool {
        self.value
    }

    pub fn ttype(&self) -> Type {
        Type::Bool
    }
}
