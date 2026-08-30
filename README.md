### Beesafe

A small interpreted language written in Rust: hand-written lexer, Pratt parser,
tree-walking evaluator, and a mark-sweep garbage collector. Built to explore how
languages work end to end.

```rust
define add(a, b) {
    return a + b
}

init xs = [3, 1, 2]
print(add(len(xs), 1))   // 4
```

### Getting started

Prerequisites: Rust (edition 2021).

```bash
cargo run                  # REPL
cargo run -- program.bs    # run a file
cargo test
```

The REPL highlights as you type, completes names, and echoes expression results.
Ctrl-C cancels the current entry; Ctrl-D exits.

### Notes

- Garbage collected in safe Rust; no `unsafe`.
- No closures, `break`/`continue`, or floats yet. Blocks don't introduce scopes;
  function calls do.
- Diagnostics via `miette` and `thiserror`.
- A work in progress; syntax and semantics may change.
