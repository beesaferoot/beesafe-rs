### Beesafe

Beesafe is a small, educational interpreter written in Rust. It includes a lexer, parser, AST, and executor, along with a simple REPL. The goal is to explore how programming languages are built end-to-end in a clear, approachable way.

### Features

- **REPL**: interactive prompt powered by `rustyline`.
- **Lexer and Parser**: tokenization and parsing into an AST.
- **Executor**: walks the AST to evaluate programs.
- **Environment**: scoped variable bindings and symbol handling.
- **Error reporting**: friendly diagnostics via `miette` and `thiserror`.

### Getting started

Prerequisites: Rust (edition 2021). Install via `rustup` if needed.

- **Run the REPL**:

```bash
cargo run
```

You should see a prompt like `>>`. Type a line of code, press Enter to see the parsed program and evaluated result. Use Ctrl-C or Ctrl-D to exit.

- **Run tests**:

```bash
cargo test
```

### Project layout

- `src/lexer.rs`: tokenization
- `src/parser.rs`: parsing to AST
- `src/ast.rs`: AST node definitions
- `src/executor.rs`: AST evaluation
- `src/environment.rs`: variable scopes and symbols
- `src/symbols.rs`: object/value representations
- `src/allocator.rs`: internal memory utilities
- `src/main.rs`: REPL entrypoint
- `tests/`: unit tests for the core components

### Notes

- The interpreter is a work-in-progress; syntax and semantics may change.
- Contributions and experiments are welcome for learning and exploration.


