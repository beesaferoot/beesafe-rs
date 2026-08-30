use beesafe::interpreter::Interpreter;
use beesafe::symbols::Object;
use std::fs;
use std::path::PathBuf;

#[test]
fn test_run_str_basic_math() {
    let mut interp = Interpreter::new();
    let out = interp.run_str("1 + 2\n3 * 4");
    let nums: Vec<i32> = out
        .into_iter()
        .map(|o| match o {
            Object::Number(n) => n,
            _ => -1,
        })
        .collect();
    assert_eq!(nums, vec![3, 12]);
}

#[test]
fn test_run_file() {
    let mut tmp = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    tmp.push("tests/tmp_script.bs");
    let script = "\ndeclare x\ninit x = 41\nx = x + 1\nx\n";
    fs::write(&tmp, script).unwrap();
    let mut interp = Interpreter::new();
    let result = interp.run_file(&tmp).unwrap();
    let last = result.last().unwrap();
    match last {
        Object::Number(n) => assert_eq!(*n, 42),
        other => panic!("expected 42, got {:?}", other),
    }
    fs::remove_file(&tmp).ok();
}

#[test]
fn bindings_survive_across_runs() {
    let mut interp = Interpreter::new();
    interp.run_str("init s = \"hello\"\n");
    interp.run_str("init a = [1, 2, 3]\n");
    interp.run_str("define dbl(n) { return n * 2 }\n");
    interp.run_str("init big = 5000\n");

    // A view is only meaningful to the heap that issued it, so each of these
    // reads a value the previous run allocated.
    match interp.run_str("s\n").last().unwrap() {
        Object::String(text) => assert_eq!(text, "hello"),
        other => panic!("expected the string binding, got {:?}", other),
    }
    match interp.run_str("len(a)\n").last().unwrap() {
        Object::Number(n) => assert_eq!(*n, 3),
        other => panic!("expected a length, got {:?}", other),
    }
    match interp.run_str("a[1]\n").last().unwrap() {
        Object::Number(n) => assert_eq!(*n, 2),
        other => panic!("expected an element, got {:?}", other),
    }
    match interp.run_str("dbl(21)\n").last().unwrap() {
        Object::Number(n) => assert_eq!(*n, 42),
        other => panic!("expected 42, got {:?}", other),
    }
    match interp.run_str("big\n").last().unwrap() {
        Object::Number(n) => assert_eq!(*n, 5000),
        other => panic!("expected 5000, got {:?}", other),
    }
}
