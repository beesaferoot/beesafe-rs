use std::fs;
use std::path::Path;

use crate::executor::Executor;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Runtime;
use crate::source::SourceMap;
use crate::symbols::Object;

pub struct Interpreter {
    runtime: Runtime,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            runtime: Runtime::new(),
        }
    }

    pub fn sources(&self) -> &SourceMap {
        &self.runtime.sources
    }

    pub fn run_str(&mut self, src: &str) -> Vec<Object> {
        let source = self.runtime.sources.add(src.to_string());
        let mut lexer = Lexer::new(src);
        let mut parser = Parser::new(&mut lexer, source);
        let program = parser.parse_program();

        if parser.has_errors() {
            parser.show_errors(&self.runtime.sources);
            return vec![];
        }

        let mut exec = Executor::new(&mut self.runtime, source);
        let results = exec.visit_program(&program);
        let values = results
            .iter()
            .filter_map(|view| exec.read_view(*view).cloned())
            .collect();
        // visit_program roots each statement result; without this the root set
        // would grow for the life of the session.
        exec.release_results(&results);
        values
    }

    pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Vec<Object>, String> {
        let content = fs::read_to_string(&path).map_err(|e| e.to_string())?;
        Ok(self.run_str(&content))
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
