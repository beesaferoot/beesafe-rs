use std::fs;
use std::path::Path;

use crate::environment::Environment;
use crate::executor::Executor;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::symbols::Object;

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub fn run_str(&mut self, src: &'static str) -> Vec<Object> {
        let mut lexer = Lexer::new(src);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        // println!("{:?}", program);

        if parser.has_errors() {
            parser.show_errors();
            return vec![];
        }

        let mut exec = Executor::new(&mut self.env, &parser);
        let results = exec.visit_program(&program);
        results.into_iter().map(|c| c.as_ref().clone()).collect()
    }

    pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Vec<Object>, String> {
        let content = fs::read_to_string(&path).map_err(|e| e.to_string())?;
        // Leak to 'static to satisfy current Lexer signature
        let leaked: &'static str = Box::leak(content.into_boxed_str());
        let objects = self.run_str(leaked);
        Ok(objects)
    }
}
