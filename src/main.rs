use beesafe::interpreter::Interpreter;
use beesafe::symbols::Object;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::env;
use std::path::PathBuf;

fn main() {
    let mut raw_args = env::args().skip(1).collect::<Vec<String>>();
    let mut debug = false;
    // extract debug flag (-d/--debug) from anywhere in args
    raw_args.retain(|a| {
        if a == "-d" || a == "--debug" {
            debug = true;
            false
        } else {
            true
        }
    });

    // Usage:
    // beesafe                    -> REPL
    // beesafe file.bs            -> run file
    // beesafe -i                 -> REPL
    // beesafe -i file.bs         -> run file, then REPL (keep env)
    // beesafe -h|--help          -> show help
    // beesafe -v|--version       -> show version

    if raw_args.is_empty() {
        repl(None, debug);
        return;
    }

    if raw_args[0] == "-h" || raw_args[0] == "--help" {
        print_usage();
        return;
    }

    if raw_args[0] == "-v" || raw_args[0] == "--version" {
        print_version();
        return;
    }

    if raw_args[0] == "-i" || raw_args[0] == "--interactive" {
        if raw_args.len() >= 2 {
            // run script, then stay in REPL with same interpreter
            let mut interp = Interpreter::new();
            let path = PathBuf::from(&raw_args[1]);
            match interp.run_file(path) {
                Ok(results) => {
                    if debug {
                        print_results(&results);
                    } else {
                        print_errors(&results);
                    }
                }
                Err(e) => {
                    eprintln!("{}", e);
                }
            }
            repl(Some(interp), debug);
        } else {
            repl(None, debug);
        }
        return;
    }

    // Otherwise, treat first arg as filename
    let mut interp = Interpreter::new();
    let path = PathBuf::from(&raw_args[0]);
    if !is_valid_script_path(&path) {
        eprintln!("error: expected a .bs file, got {:?}", path);
        return;
    }
    match interp.run_file(path) {
        Ok(results) => {
            if debug {
                print_results(&results);
            } else {
                print_errors(&results);
            }
        }
        Err(e) => eprintln!("{}", e),
    }
}

fn repl(existing: Option<Interpreter>, debug: bool) {
    let mut rl = DefaultEditor::new().unwrap();
    let mut interp = existing.unwrap_or_else(Interpreter::new);
    let mut buffer = String::new();
    loop {
        let prompt = if buffer.is_empty() { ">> " } else { ".. " };
        let readline = rl.readline(prompt);
        match readline {
            Ok(line) => {
                // Accumulate lines until the input looks complete
                buffer.push_str(&line);
                buffer.push('\n');

                if !is_input_complete(&buffer) {
                    continue;
                }

                let line_static: &'static str = Box::leak(buffer.clone().into_boxed_str());
                let results = interp.run_str(line_static);
                if debug {
                    print_results(&results);
                } else {
                    print_errors(&results);
                }
                buffer.clear();
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

fn print_results(results: &Vec<Object>) {
    for obj in results {
        match obj {
            Object::Error(err) => err.visit(),
            _ => println!("{:?}", obj),
        }
    }
}

fn print_errors(results: &Vec<Object>) {
    for obj in results {
        if let Object::Error(err) = obj {
            err.visit();
        }
    }
}

fn print_usage() {
    println!("beesafe - minimal beesafe interpreter\n");
    println!("Usage:");
    println!("  beesafe                 Start REPL");
    println!("  beesafe <file.bs>       Run a script file");
    println!("  beesafe -i              Start REPL");
    println!("  beesafe -i <file.bs>    Run script, then enter REPL with same env");
    println!("  beesafe -h, --help      Show this help");
    println!("  beesafe -v, --version   Show version");
}

fn print_version() {
    println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
}

fn is_input_complete(src: &str) -> bool {
    let mut paren: i32 = 0;
    let mut brace: i32 = 0;
    let mut bracket: i32 = 0;
    let mut in_single = false;
    let mut in_double = false;

    let mut chars = src.chars().peekable();
    while let Some(ch) = chars.next() {
        // skip line comments
        if ch == '/' {
            if let Some('/') = chars.peek() {
                // consume until newline or end
                while let Some(c) = chars.next() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
        }

        if in_single {
            if ch == '\'' {
                in_single = false;
            }
            continue;
        }
        if in_double {
            if ch == '"' {
                in_double = false;
            }
            continue;
        }

        match ch {
            '\'' => in_single = true,
            '"' => in_double = true,
            '(' => paren += 1,
            ')' => paren -= 1,
            '{' => brace += 1,
            '}' => brace -= 1,
            '[' => bracket += 1,
            ']' => bracket -= 1,
            _ => (),
        }
    }

    paren <= 0 && brace <= 0 && bracket <= 0 && !in_single && !in_double
}

fn is_valid_script_path(path: &PathBuf) -> bool {
    match path.extension().and_then(|s| s.to_str()) {
        Some(ext) if ext.eq_ignore_ascii_case("bs") => true,
        _ => false,
    }
}
