use std::cell::RefCell;

use crate::environment::Environment;

pub struct Executor<'l> {
  global_env: RefCell<Environment<'l>>
}


impl<'l> Executor<'l> {
    
}