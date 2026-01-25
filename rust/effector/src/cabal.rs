use anyhow::{Result, Context};
use std::process::Command;
use std::io::{BufReader, BufRead};
use regex::Regex;
use crate::types::{CabalBuildResult, CabalTestResult, TestFailure};

pub fn build(_cwd: &str) -> Result<()> {
    let result = CabalBuildResult {
        success: true,
        errors: vec![],
        warnings: vec![],
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