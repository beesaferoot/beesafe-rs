/*
    runtime.rs contains the state a program manipulates.

    It outlives any one program: a REPL evaluates each line separately, and the
    values a line binds must still be readable from the next.
*/

use crate::allocator::{Allocator, CellView, Heap};
use crate::builtins::BUILTINS;
use crate::environment::Environment;
use crate::source::SourceMap;
use crate::symbols::Object;

/// Integers in this range are interned rather than allocated.
const SMALL_INT_MIN: i32 = -128;
const SMALL_INT_MAX: i32 = 1024;

/// Values allocated once and shared. Interned, so they are never collected.
pub struct Constants {
    null: CellView<Object>,
    booleans: [CellView<Object>; 2],
    small_ints: Vec<CellView<Object>>,
}

impl Constants {
    fn new(heap: &mut Heap<Object>) -> Self {
        Self {
            null: heap.intern(Object::Null),
            booleans: [
                heap.intern(Object::Bool(false)),
                heap.intern(Object::Bool(true)),
            ],
            small_ints: (SMALL_INT_MIN..=SMALL_INT_MAX)
                .map(|value| heap.intern(Object::Number(value)))
                .collect(),
        }
    }

    pub fn null(&self) -> CellView<Object> {
        self.null
    }

    pub fn boolean(&self, value: bool) -> CellView<Object> {
        self.booleans[value as usize]
    }

    pub fn small_int(&self, value: i32) -> Option<CellView<Object>> {
        if (SMALL_INT_MIN..=SMALL_INT_MAX).contains(&value) {
            Some(self.small_ints[(value - SMALL_INT_MIN) as usize])
        } else {
            None
        }
    }

    /// The interned view for a constant, or `None` for a value that must be copied.
    pub fn of(&self, object: &Object) -> Option<CellView<Object>> {
        match object {
            Object::Null => Some(self.null()),
            Object::Bool(value) => Some(self.boolean(*value)),
            Object::Number(value) => self.small_int(*value),
            _ => None,
        }
    }
}

pub struct Runtime {
    pub globals: Environment,
    pub heap: Heap<Object>,
    pub constants: Constants,
    pub sources: SourceMap,
}

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new();
        // BEESAFE_GC_THRESHOLD overrides the collection threshold. Program output
        // must not depend on it.
        let threshold = std::env::var("BEESAFE_GC_THRESHOLD")
            .ok()
            .and_then(|value| value.parse().ok())
            .unwrap_or(1000);
        heap.set_gc_threshold(threshold);

        let constants = Constants::new(&mut heap);
        let mut globals = Environment::new();
        for builtin in BUILTINS {
            let cell = heap.intern(Object::Builtin(builtin));
            globals.add(builtin.name, cell);
        }

        Self {
            globals,
            heap,
            constants,
            sources: SourceMap::new(),
        }
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}
