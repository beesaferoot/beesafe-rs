use std::cell::RefCell;
use std::rc::Rc;

use beesafe::environment::Environment; 
use beesafe::symbols::*;

#[test]
fn test_env_init() {
    let env = Environment::new();
    assert_eq!(env.is_recursion_limit_exceded(), false)
}

#[test]     
fn test_env_init_with_prev_env() {
    let ref_env = RefCell::new(Environment::new());
    ref_env.borrow_mut().add(&"name".to_string(), Box::new(NumberObj{value: 3}));
    let rc_env = Rc::new(ref_env.into_inner());
    let env = Environment::new_with_prev(&rc_env);
    assert_eq!(env.depth(), 2);
    assert!(match env.get(&"name".to_string()) {
        None => false,
        Some(_) => true
    })
}