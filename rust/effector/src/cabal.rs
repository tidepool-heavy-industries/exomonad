use crate::types::{BuildError, BuildWarning, CabalBuildResult, CabalTestResult, TestFailure};
use anyhow::{Context, Result};
use regex::Regex;
use std::io::{BufRead, BufReader};
use std::process::Command;

/// Classify an error message into a type based on its content.
pub fn classify_error_type(message: &str) -> String {
    if message.contains("Couldn't match type")
        || message.contains("Expected type")
        || message.contains("No instance for")
        || message.contains("Ambiguous type variable")
        || message.contains("Couldn't match expected type")
    {
        "type-error".to_string()
    } else if message.contains("Not in scope") || message.contains("not in scope") {
        "scope-error".to_string()
    } else if message.contains("parse error") {
        "parse-error".to_string()
    } else {
        "other".to_string()
    }
}

/// Parse cabal build output (combined stdout + stderr) into errors and warnings.
pub fn parse_build_output(output: &str) -> (Vec<BuildError>, Vec<BuildWarning>) {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Regex to match error/warning headers with named captures
    // Examples:
    // src/Foo.hs:42:5: error: [GHC-12345]
    // src/Foo.hs:42:5-10: error: [GHC-12345]
    // src/Bar.hs:10:1: warning: [-Wunused-imports]
    let header_re = Regex::new(
        r"^\s*(?P<file>\S+):(?P<line>\d+):(?P<col>\d+)(?:-\d+)?:\s+(?P<level>error|warning):\s*(?:\[(?P<code>[^\]]+)\])?(?P<rest>.*)$"
    ).unwrap();

    struct Pending {
        is_error: bool,
        file: String,
        line: u32,
        column: u32,
        lines: Vec<String>,
    }

    let mut current: Option<Pending> = None;

    // Helper closure to flush pending message
    let push_message =
        |p: Pending, errors: &mut Vec<BuildError>, warnings: &mut Vec<BuildWarning>| {
            let message = p.lines.join("\n").trim().to_string();
            if p.is_error {
                let error_type = classify_error_type(&message);
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

    for line in output.lines() {
        if let Some(caps) = header_re.captures(line) {
            // Flush existing
            if let Some(p) = current.take() {
                push_message(p, &mut errors, &mut warnings);
            }

            // Start new
            let file = caps["file"].to_string();
            let line_num = caps["line"].parse().unwrap_or(0);
            let column = caps["col"].parse().unwrap_or(0);
            let level = &caps["level"];
            let rest = caps["rest"].to_string(); // Message start if any

            let mut lines = Vec::new();
            if !rest.trim().is_empty() {
                lines.push(rest);
            }

            current = Some(Pending {
                is_error: level == "error",
                file,
                line: line_num,
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

    (errors, warnings)
}

/// Parse cabal test output into a CabalTestResult.
pub fn parse_test_output(lines: impl Iterator<Item = String>) -> CabalTestResult {
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

    for line in lines {
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
                // Potential end of failure block
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

    result
}

pub fn build(cwd: &str, package: Option<String>) -> Result<()> {
    let mut cmd = Command::new("cabal");
    cmd.arg("build");
    if let Some(pkg) = package {
        cmd.arg(pkg);
    }
    cmd.arg("--ghc-options=-ferror-spans -fdiagnostics-color=never")
        .current_dir(cwd);

    let output = cmd.output().context("Failed to execute cabal build")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let all_output = format!("{}\n{}", stdout, stderr);

    let (errors, warnings) = parse_build_output(&all_output);

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

    let result = parse_test_output(reader.lines().map_while(Result::ok));

    let _ = child.wait();

    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // === Error classification tests ===

    #[test]
    fn test_classify_type_error() {
        assert_eq!(
            classify_error_type("Couldn't match type 'Int' with 'String'"),
            "type-error"
        );
        assert_eq!(classify_error_type("Expected type: Int"), "type-error");
        assert_eq!(
            classify_error_type("No instance for (Show Foo)"),
            "type-error"
        );
        assert_eq!(
            classify_error_type("Ambiguous type variable 'a'"),
            "type-error"
        );
        assert_eq!(
            classify_error_type("Couldn't match expected type 'Bool'"),
            "type-error"
        );
    }

    #[test]
    fn test_classify_scope_error() {
        assert_eq!(classify_error_type("Not in scope: 'foo'"), "scope-error");
        assert_eq!(
            classify_error_type("Variable not in scope: bar"),
            "scope-error"
        );
    }

    #[test]
    fn test_classify_parse_error() {
        assert_eq!(
            classify_error_type("parse error on input '='"),
            "parse-error"
        );
    }

    #[test]
    fn test_classify_other_error() {
        assert_eq!(classify_error_type("Some other error message"), "other");
    }

    // === Build output parsing tests ===

    #[test]
    fn test_build_success_no_errors() {
        let output = "Building library...\nLinking...\n";
        let (errors, warnings) = parse_build_output(output);
        assert!(errors.is_empty());
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_build_type_error() {
        let output = r#"
src/Foo.hs:42:5: error: [GHC-83865]
    Couldn't match type 'Int' with 'String'
    Expected: String
    Actual: Int
"#;
        let (errors, warnings) = parse_build_output(output);
        assert_eq!(errors.len(), 1);
        assert!(warnings.is_empty());

        let err = &errors[0];
        assert_eq!(err.file, "src/Foo.hs");
        assert_eq!(err.line, 42);
        assert_eq!(err.column, 5);
        assert_eq!(err.error_type, "type-error");
        assert!(err.message.contains("Couldn't match type"));
    }

    #[test]
    fn test_build_scope_error() {
        let output = r#"
src/Bar.hs:10:1: error: [GHC-76037]
    Not in scope: 'undefined_function'
"#;
        let (errors, _) = parse_build_output(output);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].error_type, "scope-error");
    }

    #[test]
    fn test_build_parse_error() {
        let output = r#"
src/Baz.hs:5:10: error:
    parse error on input '='
"#;
        let (errors, _) = parse_build_output(output);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].error_type, "parse-error");
    }

    #[test]
    fn test_build_with_warnings() {
        let output = r#"
src/Lib.hs:20:1: warning: [-Wunused-imports]
    The import of 'Data.List' is redundant
"#;
        let (errors, warnings) = parse_build_output(output);
        assert!(errors.is_empty());
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].file, "src/Lib.hs");
        assert_eq!(warnings[0].line, 20);
        assert!(warnings[0].message.contains("redundant"));
    }

    #[test]
    fn test_build_multiline_error() {
        let output = r#"
src/Complex.hs:100:5-10: error: [GHC-83865]
    Couldn't match expected type 'IO ()'
                  with actual type 'Maybe a0'
    In the expression: return Nothing
    In an equation for 'main'
"#;
        let (errors, _) = parse_build_output(output);
        assert_eq!(errors.len(), 1);

        let err = &errors[0];
        assert_eq!(err.file, "src/Complex.hs");
        assert_eq!(err.line, 100);
        assert_eq!(err.column, 5);
        assert!(err.message.contains("IO ()"));
        assert!(err.message.contains("Maybe"));
    }

    #[test]
    fn test_build_multiple_errors() {
        let output = r#"
src/A.hs:1:1: error:
    First error
src/B.hs:2:2: error:
    Second error
"#;
        let (errors, _) = parse_build_output(output);
        assert_eq!(errors.len(), 2);
        assert_eq!(errors[0].file, "src/A.hs");
        assert_eq!(errors[1].file, "src/B.hs");
    }

    #[test]
    fn test_error_location_parsing() {
        let output = "src/file.hs:42:5: error:\n    Some error\n";
        let (errors, _) = parse_build_output(output);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].line, 42);
        assert_eq!(errors[0].column, 5);
    }

    // === Test output parsing tests ===

    #[test]
    fn test_test_all_pass() {
        let lines = vec![
            "Running tests...".to_string(),
            "5 tests passed, 0 tests failed".to_string(),
        ];
        let result = parse_test_output(lines.into_iter());
        assert_eq!(result.passed, 5);
        assert_eq!(result.failed, 0);
        assert!(result.failures.is_empty());
    }

    #[test]
    fn test_test_with_failures() {
        let lines = vec![
            "Test suite my-tests: RUNNING...".to_string(),
            "  1) should work correctly".to_string(),
            "    expected: True".to_string(),
            "    got: False".to_string(),
            "3 tests passed, 1 test failed".to_string(),
        ];
        let result = parse_test_output(lines.into_iter());
        assert_eq!(result.passed, 3);
        assert_eq!(result.failed, 1);
        assert_eq!(result.failures.len(), 1);
        assert_eq!(result.failures[0].test_name, "should work correctly");
    }

    #[test]
    fn test_test_hspec_format() {
        let lines = vec![
            "Test suite my-tests: RUNNING...".to_string(),
            "  MyModule".to_string(),
            "    function1".to_string(),
            "      should do something [v]".to_string(),
            "1 test passed, 0 tests failed".to_string(),
        ];
        let result = parse_test_output(lines.into_iter());
        assert_eq!(result.passed, 1);
        assert_eq!(result.failed, 0);
    }

    #[test]
    fn test_test_suite_name_extraction() {
        let lines = vec![
            "Test suite awesome-tests: RUNNING...".to_string(),
            "  1) failure here".to_string(),
            "    error message".to_string(),
            "0 tests passed, 1 test failed".to_string(),
        ];
        let result = parse_test_output(lines.into_iter());
        assert_eq!(result.failures.len(), 1);
        assert_eq!(result.failures[0].suite, "awesome-tests");
    }

    #[test]
    fn test_test_empty_output() {
        let lines: Vec<String> = vec![];
        let result = parse_test_output(lines.into_iter());
        assert_eq!(result.passed, 0);
        assert_eq!(result.failed, 0);
        assert!(result.failures.is_empty());
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_parse_build_output_never_panics(input in ".*") {
            // Should never panic regardless of input
            let _ = parse_build_output(&input);
        }

        #[test]
        fn prop_classify_error_type_never_panics(input in ".*") {
            // Should never panic regardless of input
            let _ = classify_error_type(&input);
        }

        #[test]
        fn prop_parse_test_output_never_panics(
            lines in prop::collection::vec(".*", 0..100)
        ) {
            // Should never panic regardless of input
            let _ = parse_test_output(lines.into_iter());
        }
    }
}
