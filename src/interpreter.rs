use std::fs;
use std::path::Path;

use crate::executor::Executor;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Runtime;
use crate::source::SourceMap;
use crate::ast::Node;
use crate::symbols::Object;

/// What one program produced: the value of each statement, and the rendering of
/// the last one when it was an expression.
pub struct Outcome {
    pub values: Vec<Object>,
    pub echo: Option<String>,
}

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

    pub fn global_names(&self) -> Vec<String> {
        self.runtime
            .globals
            .names()
            .into_iter()
            .map(str::to_string)
            .collect()
    }

    pub fn run_str(&mut self, src: &str) -> Outcome {
        let source = self.runtime.sources.add(src.to_string());
        let mut lexer = Lexer::new(src);
        let mut parser = Parser::new(&mut lexer, source);
        let program = parser.parse_program();

        if parser.has_errors() {
            parser.show_errors(&self.runtime.sources);
            return Outcome {
                values: Vec::new(),
                echo: None,
            };
        }

        let echoes = program.statements.last().map_or(false, is_expression);

        let mut exec = Executor::new(&mut self.runtime, source);
        let results = exec.visit_program(&program);
        let values = results
            .iter()
            .filter_map(|view| exec.read_view(*view).cloned())
            .collect();
        // Null is what a call or an assignment evaluates to, and reporting it
        // would answer back on every `print` and every `=`.
        let echo = match (echoes, results.last()) {
            (true, Some(view)) if !matches!(exec.read_view(*view), Some(Object::Null)) => {
                exec.render(*view)
            }
            _ => None,
        };
        // visit_program roots each statement result; without this the root set
        // would grow for the life of the session.
        exec.release_results(&results);

        Outcome { values, echo }
    }

    pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Vec<Object>, String> {
        let content = fs::read_to_string(&path).map_err(|e| e.to_string())?;
        Ok(self.run_str(&content).values)
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Statements are evaluated for their effect, so only an expression is worth
/// reporting back. `init` returns the value it bound, which is why the decision
/// is made on the node rather than on the value.
fn is_expression(node: &Node) -> bool {
    !matches!(
        node,
        Node::Init(_)
            | Node::Declare(_)
            | Node::While(_)
            | Node::For(_)
            | Node::If(_)
            | Node::Function(_)
            | Node::FunctionExpr(_)
            | Node::Return(_)
            | Node::Block(_)
            | Node::Program(_)
            | Node::Error(_)
    )
}
