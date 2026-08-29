use crate::allocator::Cell;
use crate::symbols::Object;
use std::collections::HashMap;

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

    pub fn add(&mut self, identifier: &str, value: Cell<Object>) -> Option<Cell<Object>> {
        if let Some(scope) = self.scopes.last_mut() {
            return scope.insert(identifier.to_string(), value);
        }
        None
    }

    // Rebinds an existing name in whichever scope holds it, returning the old cell.
    // Rebinding rather than mutating in place keeps shared cells intact.
    pub fn assign(&mut self, identifier: &str, value: Cell<Object>) -> Option<Cell<Object>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(slot) = scope.get_mut(identifier) {
                return Some(std::mem::replace(slot, value));
            }
        }
        None
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

    pub fn pop_scope(&mut self) -> Vec<Cell<Object>> {
        if self.depth > 1 {
            self.depth -= 1;
            if let Some(scope) = self.scopes.pop() {
                return scope.into_values().collect();
            }
        }
        Vec::new()
    }

    pub fn is_scoping_limit_exceded(&self) -> bool {
        self.scoping_limit <= self.depth
    }

    pub fn depth(&self) -> i32 {
        self.depth
    }

}
