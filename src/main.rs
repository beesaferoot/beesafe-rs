use beesafe::environment::Environment;
use beesafe::parser::Parser;
use beesafe::symbols::Object;
use beesafe::{executor, lexer};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

fn main() {
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let line_static: &'static str = Box::leak(line.to_string().into_boxed_str());
                let mut lexer = lexer::Lexer::new(line_static);
                // println!("{:?}", lexer.parse_tokens());
                let mut parser = Parser::new(&mut lexer);
                let program = parser.parse_program();
                println!("{:?}", program);
                let mut env = Environment::new();
                let mut executor = executor::Executor::new(&mut env, &parser);
                if parser.has_errors() {
                    parser.show_errors();
                } else {
                    let executed_stmts = executor.visit_program(&program);
                    for stmt in executed_stmts {
                        // match stmt {
                        //     Node::Return(value) => println!("{}", value),
                        //     _ => ()
                        // }
                        match stmt.as_ref() {
                            Object::Error(err) => err.visit(),
                            _ => {
                                println!("{:?}", stmt.as_ref())
                            }
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                // CTRL-C so exit
                break;
            }
            Err(ReadlineError::Eof) => {
                // CTRL-D so exit
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
