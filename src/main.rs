use beesafe::environment::Environment;
use beesafe::symbols::Object;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use beesafe::{executor, lexer};
use beesafe::parser::Parser;

fn main() {
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let mut lexer = lexer::Lexer::new(line.as_str());
                // println!("{:?}", lexer.parse_tokens());
                let mut parser = Parser::new(&mut lexer);
                let program = parser.parse_program();
                println!("{:?}", program);
                let executor = executor::Executor::new(Box::new(Environment::new()));
                if parser.has_errors() {
                    parser.show_errors();
                }else {
                    let executed_stmts = executor.visit_program(&program);
                    for stmt in executed_stmts {
                        // match stmt {
                        //     Node::Return(value) => println!("{}", value),
                        //     _ => ()
                        // } 
                        match  stmt.as_ref() {
                            Object::Error(err) => {
                                println!("{}", err.visit())
                            },
                            _ => {
                                println!("{:?}", stmt)
                            }
                        }
                    }
                }

            }
            Err(ReadlineError::Interrupted) => {
                // CTRL-C so exit 
                break
            },
            Err(ReadlineError::Eof) => {
                // CTRL-D so exit
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            } 
        }
    }
}
