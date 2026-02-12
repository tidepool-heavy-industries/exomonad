//! CLI Integration Tests for exomonad
//!
//! Tests CLI behavior for the thin HTTP hook client.
//! `exomonad hook` is now a thin HTTP forwarder to the server. When the server
//! is unreachable, it fails open (prints `{"continue":true}` and exits 0).
//! Full E2E tests require a running server — see `tests/mcp_integration.rs`.

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

/// When the server is not running, hook should fail open: exit 0, print allow JSON.
#[test]
fn test_hook_fails_open_when_server_unreachable() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    cmd.args(["hook", "pre-tool-use"])
        .write_stdin(test_hook_json())
        .assert()
        .success()
        .stdout(predicate::str::contains(r#"{"continue":true}"#))
        .stderr(predicate::str::contains("server"));

    Ok(())
}

/// Minimal input also fails open when server is unreachable.
#[test]
fn test_hook_minimal_input_fails_open() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    cmd.args(["hook", "pre-tool-use"])
        .write_stdin(minimal_hook_json())
        .assert()
        .success()
        .stdout(predicate::str::contains(r#"{"continue":true}"#));

    Ok(())
}

/// Empty stdin still fails open (server gets empty body, returns error, client allows).
#[test]
fn test_hook_empty_stdin_fails_open() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    cmd.args(["hook", "pre-tool-use"])
        .write_stdin("")
        .assert()
        .success()
        .stdout(predicate::str::contains(r#"{"continue":true}"#));

    Ok(())
}

/// Invalid JSON still fails open.
#[test]
fn test_hook_invalid_json_fails_open() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    cmd.args(["hook", "pre-tool-use"])
        .write_stdin("not valid json")
        .assert()
        .success()
        .stdout(predicate::str::contains(r#"{"continue":true}"#));

    Ok(())
}

/// Different hook types all work as thin client.
#[test]
fn test_hook_other_event_types_fail_open() -> Result<(), Box<dyn std::error::Error>> {
    for event in &["post-tool-use", "stop", "session-end", "subagent-stop"] {
        let mut cmd = cargo_bin_cmd!("exomonad");
        cmd.args(["hook", event])
            .write_stdin(minimal_hook_json())
            .assert()
            .success()
            .stdout(predicate::str::contains(r#"{"continue":true}"#));
    }

    Ok(())
}

/// Subcommand is required (no more Option<Commands> fallback).
#[test]
fn test_no_subcommand_shows_help() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = cargo_bin_cmd!("exomonad");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Usage"));

    Ok(())
}
