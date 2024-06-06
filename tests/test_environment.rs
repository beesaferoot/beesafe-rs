use beesafe::environment::Environment; 
use beesafe::symbols::*;

#[test]
fn test_env_init() {
    let env = Environment::new();
    assert_eq!(env.is_recursion_limit_exceded(), false)
}

#[test]     
fn test_env_init_with_prev_env() {
    let mut pre_env = Box::new(Environment::new());
    pre_env.add(&"name".to_string(), Box::new(Object::Number(3)));
    let env = Environment::new_with_prev(&pre_env);
    assert_eq!(env.depth(), 2);
    assert!(match env.get(&"name".to_string()) {
        None => false,
        Some(_) => true
    })
}