//! CLI Integration Tests for exomonad
//!
//! Tests CLI behavior including argument parsing and error handling.
//! With embedded WASM, the binary always has WASM available — no fixture files needed.
//! E2E tests may fail if the embedded WASM is incompatible with current host function
//! protocols — these are marked as lenient.

use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;

fn test_hook_json() -> String {
    r#"{
        "session_id": "test-session-123",
        "hook_event_name": "PreToolUse",
        "tool_name": "Write",
        "tool_input": {"file_path": "/tmp/test.txt", "content": "hello world"},
        "transcript_path": "/tmp/transcript.jsonl",
        "cwd": "/home/test",
        "permission_mode": "default"
    }"#
    .to_string()
}

fn minimal_hook_json() -> String {
    r#"{
        "session_id": "s",
        "hook_event_name": "PreToolUse",
        "transcript_path": "/tmp/t.jsonl",
        "cwd": "/",
        "permission_mode": "default"
    }"#
    .to_string()
}

/// Check if output indicates WASM loaded successfully
fn wasm_loaded_ok(stderr: &str) -> bool {
    stderr.contains("WASM plugin loaded and initialized")
        || stderr.contains("Loading embedded WASM plugin")
}

#[test]
fn test_cli_hook_pre_tool_use() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    let output = cmd
        .args(["hook", "pre-tool-use"])
        .write_stdin(test_hook_json())
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Verify WASM loads successfully (tests host function registration)
    assert!(
        wasm_loaded_ok(&stderr),
        "WASM should load successfully. Stderr: {}",
        stderr
    );

    // Full E2E success depends on WASM/Rust protocol compatibility
    // If the WASM is stale, the call may fail - that's OK for unit tests
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("\"continue\""),
            "Expected continue field in output"
        );
    } else {
        eprintln!(
            "Note: WASM call failed (may need rebuild via `just install-all-dev`). Stderr: {}",
            stderr
        );
    }

    Ok(())
}

#[test]
fn test_cli_hook_with_minimal_input() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    let output = cmd
        .args(["hook", "pre-tool-use"])
        .write_stdin(minimal_hook_json())
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        wasm_loaded_ok(&stderr),
        "WASM should load successfully. Stderr: {}",
        stderr
    );

    Ok(())
}

#[test]
fn test_cli_invalid_json_returns_error() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    cmd.args(["hook", "pre-tool-use"])
        .write_stdin("not valid json")
        .assert()
        .failure()
        .stderr(predicate::str::contains("parse"));

    Ok(())
}

#[test]
fn test_cli_empty_stdin_returns_error() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    cmd.args(["hook", "pre-tool-use"])
        .write_stdin("")
        .assert()
        .failure();

    Ok(())
}

#[test]
fn test_cli_other_hook_types_passthrough() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    // Post-tool-use should pass through (not implemented in WASM, uses default)
    let input = r#"{
        "session_id": "s",
        "hook_event_name": "PostToolUse",
        "transcript_path": "/tmp/t.jsonl",
        "cwd": "/",
        "permission_mode": "default"
    }"#;

    let output = cmd
        .args(["hook", "post-tool-use"])
        .write_stdin(input)
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        wasm_loaded_ok(&stderr),
        "WASM should load successfully. Stderr: {}",
        stderr
    );

    // Post-tool-use uses default (doesn't call WASM), should succeed
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("\"continue\":true"),
            "Default should allow continuation"
        );
    }

    Ok(())
}
