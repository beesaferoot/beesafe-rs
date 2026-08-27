// Each case in tests/cases is one .bs file: the program, then `// ---`, then the
// expected stdout as comments. `!error` means a diagnostic is expected on stderr,
// `!panic` a crash, `!timeout` a run that outlived TIMEOUT.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

const TIMEOUT: Duration = Duration::from_secs(5);

#[test]
fn integration() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/cases");
    let mut cases: Vec<PathBuf> = fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().map_or(false, |e| e == "bs"))
        .collect();
    cases.sort();
    assert!(!cases.is_empty(), "no cases in {}", dir.display());

    let mut failed = Vec::new();
    for case in &cases {
        let want = expected(&fs::read_to_string(case).unwrap());
        let got = run(case);
        if got != want {
            let name = case.file_name().unwrap().to_string_lossy();
            failed.push(format!("{name}\n  want:\n{}  got:\n{}", indent(&want), indent(&got)));
        }
    }
    assert!(failed.is_empty(), "\n{} case(s) failed:\n\n{}", failed.len(), failed.join("\n"));
}

fn run(case: &Path) -> String {
    let mut child = Command::new(env!("CARGO_BIN_EXE_beesafe"))
        .arg(case)
        .env("NO_COLOR", "1")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let deadline = Instant::now() + TIMEOUT;
    while child.try_wait().unwrap().is_none() {
        if Instant::now() >= deadline {
            let _ = child.kill();
            let _ = child.wait();
            return String::from("!timeout\n");
        }
        thread::sleep(Duration::from_millis(10));
    }

    let out = child.wait_with_output().unwrap();
    let mut s = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr);
    if stderr.contains("panicked at") {
        s.push_str("!panic\n");
    } else if !stderr.trim().is_empty() {
        s.push_str("!error\n");
    }
    s
}

fn expected(src: &str) -> String {
    let mut out = String::new();
    let mut past_marker = false;
    for line in src.lines() {
        if !past_marker {
            past_marker = line.trim() == "// ---";
            continue;
        }
        let l = line.trim_start();
        let l = l.strip_prefix("//").unwrap_or(l);
        out.push_str(l.strip_prefix(' ').unwrap_or(l));
        out.push('\n');
    }
    out
}

fn indent(s: &str) -> String {
    s.lines().map(|l| format!("    {l}\n")).collect()
}
