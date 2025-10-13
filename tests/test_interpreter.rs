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
