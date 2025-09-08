use std::collections::HashMap;
use crate::allocator::Cell;
use crate::symbols::Object;

pub struct Environment {
    depth: i32,
    scopes: Vec<HashMap<String, Cell<Object>>>,
    scoping_limit: i32,
}

impl Environment {
    pub fn new() -> Self {
       Self {
            depth: 1,
            scopes: vec![HashMap::new()],
            scoping_limit: 50,
        }
    }

    pub fn add(&mut self, identifier: &str, value: Cell<Object>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(identifier.to_string(), value);
        }
    }

    pub fn get(&self, identifier: &str) -> Option<Cell<Object>> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(identifier) {
                return Some(val.clone());
            }
        }
        None
    }

    pub fn push_scope(&mut self) {
        self.depth += 1;
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.depth > 1 {
            self.depth -= 1;
            self.scopes.pop();
        }
    }

    pub fn is_scoping_limit_exceded(&self) -> bool {
        self.scoping_limit <= self.depth
    }

    pub fn depth(&self) -> i32 {
        self.depth
    }

}
