use beesafe::allocator::{Allocator, Heap};
use beesafe::environment::Environment;
use beesafe::symbols::*;

#[test]
fn test_env_init() {
    let env = Environment::new();
    assert_eq!(env.is_scoping_limit_exceded(), false)
}

#[test]
fn test_env_scoping() {
    let mut heap = Heap::new();
    let mut env = Environment::new();
    env.add(&"name".to_string(), heap.allocate_cell(Object::Number(3)));
    env.push_scope();
    assert_eq!(env.depth(), 2);
    assert!(match env.get(&"name".to_string()) {
        None => false,
        Some(_) => true,
    });
    env.pop_scope();
    assert_eq!(env.depth(), 1);
}
