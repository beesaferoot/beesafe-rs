use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use beesafe::lexer;
use beesafe::parser::Parser;

fn main() {
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let mut lexer = lexer::Lexer::new(line);
                let mut parser = Parser::new(&mut lexer);
                let program = parser.parse_program();
                if parser.has_errors() {
                    parser.show_errors();
                }else {
                    println!("{:?}", program);   
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
