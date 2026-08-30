/*
    builtins.rs contains the functions the interpreter provides itself.

    Each one is bound into the global scope at startup as an ordinary value, so a
    call goes through the same path as a user-defined function and a user
    definition of the same name shadows it.
*/

use crate::allocator::{Allocator, CellView, Heap};
use crate::symbols::{display, Object};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    Exact(usize),
    AtLeast(usize),
}

impl Arity {
    pub fn accepts(&self, count: usize) -> bool {
        match self {
            Arity::Exact(n) => count == *n,
            Arity::AtLeast(n) => count >= *n,
        }
    }

    pub fn describe(&self) -> String {
        match self {
            Arity::Exact(n) => format!("exactly {}", n),
            Arity::AtLeast(0) => "any number of".to_string(),
            Arity::AtLeast(n) => format!("at least {}", n),
        }
    }
}

/// Name and implementation sit in the same value, so they cannot drift apart the
/// way a lookup table and a separate dispatch can.
#[derive(Debug, Clone, Copy)]
pub struct Builtin {
    pub name: &'static str,
    pub arity: Arity,
    pub call: fn(&mut Heap<Object>, &[CellView<Object>]) -> Result<Object, String>,
}

pub const BUILTINS: &[Builtin] = &[
    Builtin {
        name: "print",
        arity: Arity::AtLeast(0),
        call: print,
    },
    Builtin {
        name: "len",
        arity: Arity::Exact(1),
        call: len,
    },
];

fn print(heap: &mut Heap<Object>, args: &[CellView<Object>]) -> Result<Object, String> {
    let mut parts: Vec<String> = Vec::with_capacity(args.len());
    for arg in args {
        match display(heap, *arg) {
            Some(text) => parts.push(text),
            // An error argument is handed back rather than printed, so it
            // reaches the diagnostics instead of being rendered as text.
            None => return Ok(heap.read_cell(*arg).cloned().unwrap_or(Object::Null)),
        }
    }
    println!("{}", parts.join(" "));
    Ok(Object::Null)
}

fn len(heap: &mut Heap<Object>, args: &[CellView<Object>]) -> Result<Object, String> {
    let length = match heap.read_cell(args[0]).ok_or("argument has been collected")? {
        // Characters rather than bytes: source is UTF-8, so len("café") is 4.
        Object::String(text) => Some(text.chars().count() as i32),
        Object::Array(elements) => Some(elements.len() as i32),
        // Ranges are inclusive of their end, matching how they iterate.
        Object::Range(range) => Some((range.end - range.start + 1).max(0)),
        Object::Null
        | Object::Bool(_)
        | Object::Error(_)
        | Object::Number(_)
        | Object::Iterator(_)
        | Object::Function(_)
        | Object::Builtin(_)
        | Object::Return(_) => None,
    };

    length
        .map(Object::Number)
        .ok_or_else(|| "len expects a string, array or range".to_string())
}
