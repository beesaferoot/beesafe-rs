use beesafe::interpreter::Interpreter;
use beesafe::source::SourceMap;
use beesafe::symbols::Object;
use rustyline::error::ReadlineError;
use beesafe::repl::{input_state, InputState, ReplRenderer};
use rustyline::Editor;
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
                        print_results(&results, interp.sources());
                    } else {
                        print_errors(&results, interp.sources());
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
                print_results(&results, interp.sources());
            } else {
                print_errors(&results, interp.sources());
            }
        }
        Err(e) => eprintln!("{}", e),
    }
}

fn repl(existing: Option<Interpreter>, debug: bool) {
    let mut rl = Editor::new().unwrap();
    rl.set_helper(Some(ReplRenderer::new()));

    let mut interp = existing.unwrap_or_else(Interpreter::new);
    let mut buffer = String::new();

    loop {
        if let Some(renderer) = rl.helper_mut() {
            renderer.set_names(interp.global_names());
        }
        let prompt = if buffer.is_empty() { ">> " } else { ".. " };
        match rl.readline(prompt) {
            Ok(line) => {
                buffer.push_str(&line);
                buffer.push('\n');

                match input_state(&buffer) {
                    InputState::Incomplete => continue,
                    InputState::Invalid(reason) => {
                        eprintln!("{}", reason);
                        buffer.clear();
                        continue;
                    }
                    InputState::Complete => (),
                }

                let _ = rl.add_history_entry(buffer.trim_end());
                let outcome = interp.run_str(&buffer);
                if debug {
                    print_results(&outcome.values, interp.sources());
                } else {
                    print_errors(&outcome.values, interp.sources());
                }
                if let Some(text) = outcome.echo {
                    println!("{}", text);
                }
                buffer.clear();
            }
            // Ctrl-C abandons whatever is half-typed; Ctrl-D ends the session.
            Err(ReadlineError::Interrupted) => {
                buffer.clear();
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

fn print_results(results: &Vec<Object>, sources: &SourceMap) {
    for obj in results {
        match obj {
            Object::Error(err) => err.visit(sources),
            _ => println!("{:?}", obj),
        }
    }
}

fn print_errors(results: &Vec<Object>, sources: &SourceMap) {
    for obj in results {
        if let Object::Error(err) = obj {
            err.visit(sources);
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


fn is_valid_script_path(path: &PathBuf) -> bool {
    match path.extension().and_then(|s| s.to_str()) {
        Some(ext) if ext.eq_ignore_ascii_case("bs") => true,
        _ => false,
    }
}
