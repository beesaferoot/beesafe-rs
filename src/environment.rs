use std::collections::HashMap;
use crate::symbols::Object;

pub struct Environment<'a> {
    prev: Option<&'a Box<Environment<'a>>>,
    depth: i32,
    table: HashMap<String, Box<Object>>,
    recursion_limit: i32
}

impl<'a> Environment<'a> {
    
    pub fn new() -> Self {
        Self { 
            prev: None, 
            depth: 1, 
            table:  HashMap::from([]), 
            recursion_limit: 50 
        }
    }

    pub fn new_with_prev(prev: &'a Box<Environment>) -> Self {
        Self { 
            prev: Some(prev), 
            depth: prev.depth + 1, 
            table:  HashMap::from([]), 
            recursion_limit: 50 
        }
    }

    pub fn add(&mut self, identifier: &String, value: Box<Object>) {
        self.table.insert(identifier.to_string(), value);
    }

    pub fn get(&self, identifier: &String) -> Option<&Box<Object>> {
        let mut current_env = self.prev();
        let mut ident_value: Option<&Box<Object>> = None;
        if self.table.contains_key(identifier) {
            return self.table.get(identifier);
        }
        while !current_env.is_none() {
            match current_env.unwrap().get(identifier) {
                Some(value) => ident_value = Some(value),
                None => {}
            }
            current_env = current_env.unwrap().prev();
        }

        ident_value
        
    }

    pub fn prev(&self) -> Option<&Box<Environment>> {
        self.prev
    }

    pub fn is_recursion_limit_exceded(&self) -> bool {
        self.recursion_limit <= self.depth
    }

    pub fn depth(&self) -> i32 {
        self.depth
    }

}

