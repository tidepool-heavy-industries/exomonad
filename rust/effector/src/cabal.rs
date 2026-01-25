use anyhow::{Result, Context};
use std::process::Command;
use std::io::{BufReader, BufRead};
use regex::Regex;
use crate::types::{CabalBuildResult, CabalTestResult, TestFailure, BuildError, BuildWarning};

pub fn build(cwd: &str) -> Result<()> {
    let output = Command::new("cabal")
        .arg("build")
        .arg("--ghc-options=-ferror-spans -fdiagnostics-color=never")
        .current_dir(cwd)
        .output()
        .context("Failed to execute cabal build")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let all_output = format!("{}\n{}", stdout, stderr);

    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Regex to match error/warning headers
    // Examples:
    // src/Foo.hs:42:5: error: [GHC-12345]
    // src/Foo.hs:42:5-10: error: [GHC-12345]
    // src/Bar.hs:10:1: warning: [-Wunused-imports]
    let header_re = Regex::new(r"^\s*(\S+):(\d+):(\d+)(?:-\d+)?:\s+(error|warning):\s*(?:\[([^\]]+)\])?(.*)$").unwrap();

    struct Pending {
        is_error: bool,
        file: String,
        line: u32,
        column: u32,
        lines: Vec<String>,
    }

    let mut current: Option<Pending> = None;

    // Helper closure to flush pending message
    let push_message = |p: Pending, errors: &mut Vec<BuildError>, warnings: &mut Vec<BuildWarning>| {
        let message = p.lines.join("\n").trim().to_string();
        if p.is_error {
            let error_type = if message.contains("Couldn't match type") || message.contains("Expected type") || message.contains("No instance for") {
                "type-error".to_string()
            } else if message.contains("Not in scope") || message.contains("not in scope") {
                "scope-error".to_string()
            } else if message.contains("parse error") {
                "parse-error".to_string()
            } else {
                "other".to_string()
            };

            errors.push(BuildError {
                file: p.file,
                line: p.line,
                column: p.column,
                message,
                error_type,
            });
        } else {
            warnings.push(BuildWarning {
                file: p.file,
                line: p.line,
                message,
            });
        }
    };

    for line in all_output.lines() {
        if let Some(caps) = header_re.captures(line) {
            // Flush existing
            if let Some(p) = current.take() {
                push_message(p, &mut errors, &mut warnings);
            }

            // Start new
            let file = caps[1].to_string();
            let line = caps[2].parse().unwrap_or(0);
            let column = caps[3].parse().unwrap_or(0);
            let type_str = &caps[4];
            let rest = caps[6].to_string(); // Message start if any

            let mut lines = Vec::new();
            if !rest.trim().is_empty() {
                lines.push(rest);
            }

            current = Some(Pending {
                is_error: type_str == "error",
                file,
                line,
                column,
                lines,
            });
        } else if let Some(ref mut p) = current {
            p.lines.push(line.to_string());
        }
    }

    // Flush last
    if let Some(p) = current.take() {
        push_message(p, &mut errors, &mut warnings);
    }

    let result = CabalBuildResult {
        success: output.status.success(),
        errors,
        warnings,
    };

    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

pub fn test(cwd: &str, _package: Option<String>) -> Result<()> {
    let mut child = Command::new("cabal")
        .arg("test")
        .arg("--test-show-details=streaming")
        .arg("--color=never")
        .current_dir(cwd)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .context("Failed to spawn cabal test")?;

    let stdout = child.stdout.take().context("Failed to capture stdout")?;
    let reader = BufReader::new(stdout);

    let mut result = CabalTestResult {
        passed: 0,
        failed: 0,
        failures: vec![],
    };

    // Regex for HSpec/Tasty summaries
    let summary_re = Regex::new(r"(\d+) tests? passed, (\d+) tests? failed").unwrap();
    // Regex for test suite result
    let suite_result_re = Regex::new(r"Test suite (\S+): (\w+)").unwrap();
    // Regex for failure header (HSpec)
    let failure_header_re = Regex::new(r"^\s*\d+\)\s+(.+)").unwrap();

    let mut current_suite = String::from("unknown");
    let mut in_failure_block = false;
    let mut current_failure: Option<TestFailure> = None;

    for line in reader.lines() {
        let line = line?;
        
        if let Some(caps) = suite_result_re.captures(&line) {
            current_suite = caps[1].to_string();
        }

        if let Some(caps) = summary_re.captures(&line) {
            result.passed += caps[1].parse::<u32>().unwrap_or(0);
            result.failed += caps[2].parse::<u32>().unwrap_or(0);
        }

        // Simplistic failure parsing
        if let Some(caps) = failure_header_re.captures(&line) {
            if let Some(fail) = current_failure.take() {
                result.failures.push(fail);
            }
            in_failure_block = true;
            current_failure = Some(TestFailure {
                suite: current_suite.clone(),
                test_name: caps[1].trim().to_string(),
                message: String::new(),
                location: None,
            });
        } else if in_failure_block {
            if line.trim().is_empty() {
                // Potential end of failure block if we see multiple empty lines or a summary, 
                // but let's keep it simple for now.
            } else if let Some(ref mut fail) = current_failure {
                if !fail.message.is_empty() {
                    fail.message.push('\n');
                }
                fail.message.push_str(&line);
            }
        }
    }

    if let Some(fail) = current_failure {
        result.failures.push(fail);
    }

    let _ = child.wait();

    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}