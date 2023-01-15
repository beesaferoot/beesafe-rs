/*
    Symbol objects for beesafe 
*/

pub trait Object {

    fn visit(&self) -> String; 
    fn ttype(&self) -> Type;
}

pub enum Type {
    Int, 
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
