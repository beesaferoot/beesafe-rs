use std::any::Any;

/*
    Symbol objects for beesafe 
*/
use crate::parser::ParseError;

pub trait Object {

    fn visit(&self) -> Box<dyn Any>; 
    fn ttype(&self) -> Type;
    // fn value(&self) -> ;
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

impl Object for NullObj {
    fn visit(&self) -> Box<dyn Any> {
        //TODO: implement visit method 
        Box::new(String::from(""))
    }

    fn ttype(&self) -> Type {
        Type::Null
    }
}

impl Object for ErrorObj {

    fn visit(&self) -> Box<dyn Any> {
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
                    TypeError::TypeMismatch(err_msg) => err_msg,
                    TypeError::PlaceHolder(err_msg) => err_msg
                },
                None => String::from("")
            }
        }
        return Box::new(err_str);

    }

    fn ttype(&self) -> Type {
        Type::Error
    }
}

impl Object for NumberObj {
    
    fn visit(&self) -> Box<dyn Any> {
        Box::new(self.value)
    }

    fn ttype(&self) -> Type {
        Type::Number
    }
}

impl Object for StringObj {
    fn visit(&self) -> Box<dyn Any> {
        Box::new(self.literal.to_string())
    }

    fn ttype(&self) -> Type {
        Type::String
    }
}

impl Object for BoolObj {
    fn visit(&self) -> Box<dyn Any> {
        Box::new(self.value.to_string())
    }

    fn ttype(&self) -> Type {
        Type::Bool
    }
}


