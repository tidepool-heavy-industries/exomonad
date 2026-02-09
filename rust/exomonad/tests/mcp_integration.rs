//! MCP stdio integration tests.
//!
//! Black-box tests that spawn `exomonad mcp-stdio` as a subprocess and drive it
//! via JSON-RPC over stdin/stdout. Catches wire format mismatches (varint128 decode
//! errors) by exercising the full pipeline:
//!   JSON-RPC → WASM (Haskell) → protobuf effects → Rust handlers → protobuf response → WASM decode → JSON-RPC response
//!
//! The default test role is `tl`, which provides: spawn_agents, cleanup_agents,
//! cleanup_merged_agents, list_agents, popup.
//!
//! Each test spawns a full exomonad subprocess with ~4MB WASM loading, so we limit
//! concurrency to avoid resource exhaustion when `cargo test` runs all 38+ tests
//! in parallel. The SUBPROCESS_SEMAPHORE caps concurrent spawns.

#![allow(deprecated)] // cargo_bin function — no macro alternative that returns PathBuf

use assert_cmd::cargo::cargo_bin;
use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::sync::{Mutex, OnceLock};
use std::time::Duration;
use tempfile::TempDir;

/// Limits concurrent subprocess spawns to avoid resource exhaustion.
/// Each exomonad process loads ~4MB of WASM, so 38 simultaneous processes
/// can exceed system limits (file descriptors, memory, CPU for WASM compilation).
const MAX_CONCURRENT_SUBPROCESSES: usize = 8;

fn subprocess_semaphore() -> &'static Semaphore {
    static INSTANCE: OnceLock<Semaphore> = OnceLock::new();
    INSTANCE.get_or_init(|| Semaphore::new(MAX_CONCURRENT_SUBPROCESSES))
}

/// Simple counting semaphore using a mutex + condvar.
struct Semaphore {
    state: Mutex<usize>,
    condvar: std::sync::Condvar,
    max: usize,
}

impl Semaphore {
    fn new(max: usize) -> Self {
        Self {
            state: Mutex::new(0),
            condvar: std::sync::Condvar::new(),
            max,
        }
    }

    fn acquire(&self) -> SemaphoreGuard<'_> {
        let mut count = self.state.lock().unwrap();
        while *count >= self.max {
            count = self.condvar.wait(count).unwrap();
        }
        *count += 1;
        SemaphoreGuard(self)
    }
}

struct SemaphoreGuard<'a>(&'a Semaphore);

impl Drop for SemaphoreGuard<'_> {
    fn drop(&mut self) {
        let mut count = self.0.state.lock().unwrap();
        *count -= 1;
        self.0.condvar.notify_one();
    }
}

// ============================================================================
// McpProcess — subprocess lifecycle helper
// ============================================================================

struct McpProcess {
    child: Child,
    stdin: Option<ChildStdin>,
    response_rx: Receiver<String>,
    stderr_rx: Receiver<String>,
    _semaphore_guard: SemaphoreGuard<'static>,
}

impl McpProcess {
    /// Spawn `exomonad mcp-stdio` in the given working directory.
    /// Blocks until a semaphore slot is available to limit concurrent subprocesses.
    fn spawn(work_dir: &std::path::Path) -> Self {
        let guard = subprocess_semaphore().acquire();

        let mut child = Command::new(cargo_bin("exomonad"))
            .args(["mcp-stdio"])
            .current_dir(work_dir)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to spawn exomonad mcp-stdio");

        let stdin = child.stdin.take().expect("Failed to capture stdin");
        let stdout = child.stdout.take().expect("Failed to capture stdout");
        let stderr = child.stderr.take().expect("Failed to capture stderr");

        // Background reader thread feeds stdout lines into a channel
        let (tx, rx) = mpsc::channel();
        std::thread::spawn(move || {
            let reader = BufReader::new(stdout);
            for line in reader.lines() {
                match line {
                    Ok(l) if !l.trim().is_empty() => {
                        if tx.send(l).is_err() {
                            break;
                        }
                    }
                    Ok(_) => continue,
                    Err(_) => break,
                }
            }
        });

        // Background reader thread captures stderr for diagnostics
        let (err_tx, err_rx) = mpsc::channel();
        std::thread::spawn(move || {
            let reader = BufReader::new(stderr);
            for line in reader.lines() {
                match line {
                    Ok(l) if !l.trim().is_empty() => {
                        if err_tx.send(l).is_err() {
                            break;
                        }
                    }
                    Ok(_) => continue,
                    Err(_) => break,
                }
            }
        });

        Self {
            child,
            stdin: Some(stdin),
            response_rx: rx,
            stderr_rx: err_rx,
            _semaphore_guard: guard,
        }
    }

    fn write_line(&mut self, line: &str) {
        let stdin = self.stdin.as_mut().expect("stdin already closed");
        writeln!(stdin, "{}", line).expect("Failed to write to stdin");
        stdin.flush().expect("Failed to flush stdin");
    }

    /// Collect any stderr output accumulated so far.
    fn drain_stderr(&self) -> String {
        let mut lines = Vec::new();
        while let Ok(line) = self.stderr_rx.try_recv() {
            lines.push(line);
        }
        lines.join("\n")
    }

    /// Send a JSON-RPC request (has `id`) and read the response with a timeout.
    fn call(&mut self, request: Value) -> Value {
        let line = serde_json::to_string(&request).expect("Failed to serialize request");
        self.write_line(&line);

        match self.response_rx.recv_timeout(Duration::from_secs(15)) {
            Ok(response_line) => {
                serde_json::from_str(&response_line).expect("Failed to parse response JSON")
            }
            Err(_) => {
                let stderr = self.drain_stderr();
                panic!(
                    "Timed out waiting for response.\nRequest: {}\nStderr: {}",
                    line, stderr
                );
            }
        }
    }

    /// Send a notification (no `id` field) — server should not respond.
    fn notify(&mut self, request: Value) {
        let line = serde_json::to_string(&request).expect("Failed to serialize notification");
        self.write_line(&line);
    }

    /// Send raw bytes (for malformed JSON tests).
    fn send_raw(&mut self, data: &str) {
        self.write_line(data);
    }

    /// Read a response with timeout (for cases where we expect a response after send_raw).
    fn read_response(&self) -> Value {
        match self.response_rx.recv_timeout(Duration::from_secs(15)) {
            Ok(response_line) => {
                serde_json::from_str(&response_line).expect("Failed to parse response JSON")
            }
            Err(_) => {
                let stderr = self.drain_stderr();
                panic!("Timed out waiting for response.\nStderr: {}", stderr);
            }
        }
    }

    /// Assert no response arrives within the given duration.
    fn assert_no_response(&self, wait: Duration) {
        match self.response_rx.recv_timeout(wait) {
            Err(mpsc::RecvTimeoutError::Timeout) => {} // Expected
            Ok(line) => panic!("Expected no response, got: {}", line),
            Err(e) => panic!("Channel error: {}", e),
        }
    }

    /// Close stdin and wait for the child to exit.
    fn shutdown(&mut self) -> std::process::ExitStatus {
        self.stdin.take(); // Close stdin to signal EOF
        self.child.wait().expect("Failed to wait for child")
    }
}

impl Drop for McpProcess {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

// ============================================================================
// Test directory setup
// ============================================================================

/// Create an isolated temp directory with git repo and .exomonad config.
fn setup_test_dir() -> TempDir {
    let dir = TempDir::new().expect("Failed to create temp dir");

    // Create .exomonad/config.toml
    let config_dir = dir.path().join(".exomonad");
    std::fs::create_dir_all(&config_dir).expect("Failed to create .exomonad dir");
    std::fs::write(
        config_dir.join("config.toml"),
        "default_role = \"tl\"\nproject_dir = \".\"\nzellij_session = \"test\"\n",
    )
    .expect("Failed to write config.toml");

    // Initialize git repo (many tools require one)
    let status = Command::new("git")
        .args(["init"])
        .current_dir(dir.path())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("Failed to run git init");
    assert!(status.success(), "git init failed");

    let status = Command::new("git")
        .args([
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@test.com",
            "commit",
            "--allow-empty",
            "-m",
            "initial",
        ])
        .current_dir(dir.path())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("Failed to run git commit");
    assert!(status.success(), "git commit failed");

    dir
}

// ============================================================================
// JSON-RPC request builders
// ============================================================================

fn initialize_request(id: u64) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test", "version": "0.0.1"}
        }
    })
}

fn list_tools_request(id: u64) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": "tools/list",
        "params": {}
    })
}

fn tool_call_request(id: u64, name: &str, arguments: Value) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": "tools/call",
        "params": {
            "name": name,
            "arguments": arguments
        }
    })
}

fn notification(method: &str) -> Value {
    json!({
        "jsonrpc": "2.0",
        "method": method,
        "params": {}
    })
}

// ============================================================================
// Assertion helpers
// ============================================================================

/// Check that a tool call response succeeded (no JSON-RPC error, no isError).
fn assert_tool_success(resp: &Value) -> Value {
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Expected no JSON-RPC error, got: {}",
        resp
    );

    let result = &resp["result"];
    let is_error = result.get("isError");
    assert!(
        is_error.is_none() || is_error == Some(&Value::Null) || is_error == Some(&json!(false)),
        "Expected tool success (isError absent or false), got: {}",
        resp
    );

    // Validate content structure
    let content = &result["content"];
    assert!(content.is_array(), "Expected content array, got: {}", resp);
    assert!(
        !content.as_array().unwrap().is_empty(),
        "Expected non-empty content"
    );
    assert_eq!(
        content[0]["type"], "text",
        "Expected content type 'text', got: {}",
        content[0]["type"]
    );

    // Parse the text field as JSON — proto decode errors surface here
    let text = content[0]["text"]
        .as_str()
        .expect("content[0].text should be a string");
    serde_json::from_str(text).unwrap_or_else(|e| {
        panic!(
            "content[0].text should be valid JSON. Error: {}. Text: {}",
            e, text
        )
    })
}

/// Assert a tool call returned isError: true.
fn assert_tool_error(resp: &Value) {
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    let has_tool_error = resp
        .pointer("/result/isError")
        .map(|v| v == &json!(true))
        .unwrap_or(false);

    assert!(
        has_jsonrpc_error || has_tool_error,
        "Expected error response, got: {}",
        resp
    );
}

/// Spawn process with initialize handshake already done.
fn spawn_initialized(work_dir: &std::path::Path) -> McpProcess {
    let mut proc = McpProcess::spawn(work_dir);

    // Initialize
    let resp = proc.call(initialize_request(0));
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Initialize failed: {}",
        resp
    );

    // Send initialized notification
    proc.notify(notification("notifications/initialized"));

    proc
}

// ============================================================================
// Tests
// ============================================================================

/// Server starts, WASM loads, returns protocolVersion + serverInfo.
#[test]
fn mcp_initialize_handshake() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    let resp = proc.call(initialize_request(1));

    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Initialize returned error: {}",
        resp
    );
    assert_eq!(resp["id"], 1);

    let result = &resp["result"];
    assert_eq!(
        result["protocolVersion"], "2024-11-05",
        "Unexpected protocolVersion"
    );
    assert!(
        result.get("serverInfo").is_some(),
        "Missing serverInfo in response"
    );
    assert_eq!(result["serverInfo"]["name"], "exomonad");
    assert!(
        result.get("capabilities").is_some(),
        "Missing capabilities in response"
    );

    proc.shutdown();
}

/// WASM handle_list_tools works, returns tool definitions for the tl role.
#[test]
fn mcp_list_tools_returns_definitions() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(list_tools_request(2));

    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "tools/list returned error: {}",
        resp
    );

    let tools = &resp["result"]["tools"];
    assert!(tools.is_array(), "Expected tools array, got: {}", resp);

    let tools_arr = tools.as_array().unwrap();
    assert!(
        !tools_arr.is_empty(),
        "Expected at least one tool definition"
    );

    // Collect tool names
    let tool_names: Vec<&str> = tools_arr
        .iter()
        .filter_map(|t| t["name"].as_str())
        .collect();

    // tl role tools
    for expected in &["spawn_agents", "cleanup_agents", "list_agents"] {
        assert!(
            tool_names.contains(expected),
            "Missing expected tool '{}'. Found: {:?}",
            expected,
            tool_names
        );
    }

    // Each tool should have name, description, inputSchema
    for tool in tools_arr {
        assert!(tool.get("name").is_some(), "Tool missing name: {}", tool);
        assert!(
            tool.get("description").is_some(),
            "Tool missing description: {}",
            tool
        );
        assert!(
            tool.get("inputSchema").is_some(),
            "Tool missing inputSchema: {}",
            tool
        );
    }

    proc.shutdown();
}

/// Full proto roundtrip via agent.list effect.
/// Haskell encodes ListAgentsRequest → Rust decodes → AgentHandler → Rust encodes
/// ListAgentsResponse → Haskell decodes. Varint128 bug surfaces here.
#[test]
fn mcp_tool_call_list_agents() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(3, "list_agents", json!({})));
    let inner = assert_tool_success(&resp);

    // Fresh directory has no agents — should return empty list or object
    assert!(
        inner.is_object() || inner.is_array(),
        "Expected structured list_agents response: {}",
        inner
    );

    proc.shutdown();
}

/// cleanup_agents with empty issues list — proto roundtrip through agent.cleanup namespace.
#[test]
fn mcp_tool_call_cleanup_agents() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(
        4,
        "cleanup_agents",
        json!({"issues": []}),
    ));

    // Empty issues list should succeed (no-op) or return an error about missing issues.
    // Either way, the proto roundtrip completed without crash.
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    if !has_jsonrpc_error {
        let result = &resp["result"];
        let content = &result["content"];
        assert!(content.is_array(), "Expected content array, got: {}", resp);
    }

    proc.shutdown();
}

/// cleanup_merged_agents — proto roundtrip through agent.cleanup_merged namespace.
#[test]
fn mcp_tool_call_cleanup_merged_agents() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(5, "cleanup_merged_agents", json!({})));

    // Fresh repo has no merged branches — should succeed with empty result
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    if !has_jsonrpc_error {
        let result = &resp["result"];
        let content = &result["content"];
        assert!(content.is_array(), "Expected content array, got: {}", resp);
    }

    proc.shutdown();
}

/// Nonexistent tool → isError: true (not a crash).
#[test]
fn mcp_invalid_tool_returns_error() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(7, "nonexistent_tool_xyz", json!({})));

    assert_tool_error(&resp);

    proc.shutdown();
}

/// Raw garbage → JSON-RPC parse error code -32700.
#[test]
fn mcp_malformed_json_returns_parse_error() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    proc.send_raw("this is not valid json at all!!!");
    let resp = proc.read_response();

    assert!(
        resp.get("error").is_some() && !resp["error"].is_null(),
        "Expected JSON-RPC error for malformed input, got: {}",
        resp
    );
    assert_eq!(
        resp["error"]["code"], -32700,
        "Expected parse error code -32700, got: {}",
        resp["error"]["code"]
    );

    proc.shutdown();
}

/// Unknown method → JSON-RPC error code -32601.
#[test]
fn mcp_unknown_method_returns_method_not_found() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    let resp = proc.call(json!({
        "jsonrpc": "2.0",
        "id": 9,
        "method": "completely/unknown",
        "params": {}
    }));

    assert!(
        resp.get("error").is_some() && !resp["error"].is_null(),
        "Expected JSON-RPC error for unknown method, got: {}",
        resp
    );
    assert_eq!(
        resp["error"]["code"], -32601,
        "Expected method not found code -32601, got: {}",
        resp["error"]["code"]
    );

    proc.shutdown();
}

/// notifications/initialized (no id) produces no stdout line.
#[test]
fn mcp_notification_no_response() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    // Send a notification (no id field)
    proc.notify(notification("notifications/initialized"));

    // Should not receive any response within a reasonable window
    proc.assert_no_response(Duration::from_secs(2));

    proc.shutdown();
}

/// 10 sequential list_agents calls — WASM state doesn't corrupt across calls.
#[test]
fn mcp_multiple_sequential_calls() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    for i in 0..10u64 {
        let resp = proc.call(tool_call_request(100 + i, "list_agents", json!({})));
        let inner = assert_tool_success(&resp);
        assert!(
            inner.is_object() || inner.is_array(),
            "Call {} failed: expected structured response, got: {}",
            i,
            inner
        );
    }

    proc.shutdown();
}

/// Multiple different operations interleaved — validates server handles mixed traffic.
#[test]
fn mcp_interleaved_operations() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    // list_agents
    let resp = proc.call(tool_call_request(20, "list_agents", json!({})));
    assert_tool_success(&resp);

    // unknown tool (should error, not crash)
    let resp = proc.call(tool_call_request(21, "no_such_tool", json!({})));
    assert_tool_error(&resp);

    // list_agents again (server should still be healthy)
    let resp = proc.call(tool_call_request(22, "list_agents", json!({})));
    assert_tool_success(&resp);

    // tools/list still works
    let resp = proc.call(list_tools_request(23));
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "tools/list failed after error recovery: {}",
        resp
    );

    proc.shutdown();
}

// ============================================================================
// Argument validation — WASM-level JSON parsing
// ============================================================================

/// spawn_agents missing required `issues` field → tool-level parse error.
#[test]
fn mcp_spawn_agents_missing_required_field() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    // Missing `issues` (required)
    let resp = proc.call(tool_call_request(
        30,
        "spawn_agents",
        json!({"owner": "test", "repo": "test"}),
    ));
    assert_tool_error(&resp);

    // Server still healthy after parse error
    let resp = proc.call(tool_call_request(31, "list_agents", json!({})));
    assert_tool_success(&resp);

    proc.shutdown();
}

/// spawn_agents with `issues` as string instead of array → parse error.
#[test]
fn mcp_spawn_agents_wrong_type_issues() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(
        32,
        "spawn_agents",
        json!({"issues": "123", "owner": "test", "repo": "test"}),
    ));
    assert_tool_error(&resp);

    proc.shutdown();
}

/// spawn_agents with empty issues array — valid parse, empty batch.
#[test]
fn mcp_spawn_agents_empty_issues() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(
        33,
        "spawn_agents",
        json!({"issues": [], "owner": "test", "repo": "test"}),
    ));

    // Empty issues is valid at the schema level — handler may succeed with empty result
    // or fail at the service level. Either way, no crash.
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);

    proc.shutdown();
}

/// cleanup_agents missing required `issues` field.
#[test]
fn mcp_cleanup_agents_missing_issues() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(34, "cleanup_agents", json!({})));
    assert_tool_error(&resp);

    proc.shutdown();
}

/// cleanup_agents with nonexistent issue IDs — exercises batch error response path.
#[test]
fn mcp_cleanup_agents_nonexistent_issues() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(
        35,
        "cleanup_agents",
        json!({"issues": ["99999", "88888"]}),
    ));

    // These issues don't exist, so cleanup should fail for each.
    // The response should still be well-formed (not a crash).
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);

    let result = &resp["result"];
    let content = &result["content"];
    assert!(content.is_array(), "Expected content array, got: {}", resp);

    proc.shutdown();
}

/// cleanup_agents with force=true — optional boolean arg exercises proto encoding.
#[test]
fn mcp_cleanup_agents_with_force() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(
        36,
        "cleanup_agents",
        json!({"issues": ["12345"], "force": true}),
    ));

    // Issue doesn't exist, but the force flag should be accepted and encoded correctly.
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);

    proc.shutdown();
}

/// spawn_agents with all optional fields — exercises full proto message encoding.
#[test]
fn mcp_spawn_agents_all_optional_fields() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(
        37,
        "spawn_agents",
        json!({
            "issues": ["42"],
            "owner": "testowner",
            "repo": "testrepo",
            "worktree_dir": "/tmp/test-worktrees",
            "agent_type": "claude"
        }),
    ));

    // Will fail at the service level (no Zellij, no GitHub token in test), but
    // the proto encode/decode and argument parsing should complete.
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);

    proc.shutdown();
}

/// Extra unknown fields in tool arguments are silently ignored (Aeson default).
#[test]
fn mcp_tool_call_extra_args_ignored() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(tool_call_request(
        38,
        "list_agents",
        json!({"unknown_field": "should_be_ignored", "another": 42}),
    ));

    // list_agents takes no args — extra fields should be ignored, not cause errors
    assert_tool_success(&resp);

    proc.shutdown();
}

// ============================================================================
// JSON-RPC protocol edge cases
// ============================================================================

/// String request ID preserved in response.
#[test]
fn mcp_string_request_id() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    let resp = proc.call(json!({
        "jsonrpc": "2.0",
        "id": "abc-def-123",
        "method": "initialize",
        "params": {}
    }));

    assert_eq!(
        resp["id"], "abc-def-123",
        "Response should echo back string ID"
    );

    proc.shutdown();
}

/// Null request ID is treated as a notification (serde deserializes null → None).
#[test]
fn mcp_null_request_id_is_notification() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    // serde_json deserializes `"id": null` as Option::None for Option<Value>,
    // so the server treats this as a notification — no response expected.
    proc.notify(json!({
        "jsonrpc": "2.0",
        "id": null,
        "method": "initialize",
        "params": {}
    }));
    proc.assert_no_response(Duration::from_secs(2));

    // Server still healthy
    let resp = proc.call(initialize_request(45));
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Server should work after null-id request: {}",
        resp
    );

    proc.shutdown();
}

/// Request with no params field — params defaults to empty.
#[test]
fn mcp_missing_params() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    let resp = proc.call(json!({
        "jsonrpc": "2.0",
        "id": 40,
        "method": "initialize"
    }));

    // Should succeed — params defaults to Value::Null/empty
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Initialize without params should succeed: {}",
        resp
    );

    proc.shutdown();
}

/// Large numeric request ID.
#[test]
fn mcp_large_numeric_id() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    let resp = proc.call(json!({
        "jsonrpc": "2.0",
        "id": 999999999,
        "method": "initialize",
        "params": {}
    }));

    assert_eq!(
        resp["id"], 999999999,
        "Response should echo large numeric ID"
    );

    proc.shutdown();
}

/// tools/call with missing name field.
#[test]
fn mcp_tool_call_missing_name() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(json!({
        "jsonrpc": "2.0",
        "id": 41,
        "method": "tools/call",
        "params": {
            "arguments": {}
        }
    }));

    // Missing tool name — should result in an error (empty tool name → unknown tool)
    assert_tool_error(&resp);

    proc.shutdown();
}

/// tools/call with missing arguments field — defaults to empty object.
#[test]
fn mcp_tool_call_missing_arguments() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(json!({
        "jsonrpc": "2.0",
        "id": 42,
        "method": "tools/call",
        "params": {
            "name": "list_agents"
        }
    }));

    // list_agents doesn't need arguments, so this should work
    assert_tool_success(&resp);

    proc.shutdown();
}

// ============================================================================
// Error recovery sequences
// ============================================================================

/// Server recovers from malformed JSON → processes valid request afterward.
#[test]
fn mcp_recovery_after_malformed_json() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    // Send garbage
    proc.send_raw("{{{invalid json garbage!!!}}}");
    let resp = proc.read_response();
    assert_eq!(resp["error"]["code"], -32700);

    // Now send a valid request — server should still work
    let resp = proc.call(initialize_request(50));
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Server should recover after malformed JSON: {}",
        resp
    );

    proc.shutdown();
}

/// Server recovers from multiple consecutive parse errors.
#[test]
fn mcp_recovery_after_multiple_parse_errors() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    for i in 0..5 {
        proc.send_raw(&format!("garbage line {}", i));
        let resp = proc.read_response();
        assert_eq!(
            resp["error"]["code"], -32700,
            "Parse error {} should return -32700",
            i
        );
    }

    // Server still healthy
    let resp = proc.call(initialize_request(51));
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Server should recover after multiple parse errors: {}",
        resp
    );

    proc.shutdown();
}

/// Alternating errors and successes — stress tests error recovery.
#[test]
fn mcp_alternating_errors_and_successes() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    for i in 0..5u64 {
        // Success
        let resp = proc.call(tool_call_request(60 + i * 2, "list_agents", json!({})));
        assert_tool_success(&resp);

        // Error (unknown tool)
        let resp = proc.call(tool_call_request(61 + i * 2, "nonexistent", json!({})));
        assert_tool_error(&resp);
    }

    // Final success — server still healthy
    let resp = proc.call(tool_call_request(70, "list_agents", json!({})));
    assert_tool_success(&resp);

    proc.shutdown();
}

/// Mix of parse errors, method errors, and tool errors in sequence.
#[test]
fn mcp_mixed_error_types() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    // 1. Parse error (malformed JSON)
    proc.send_raw("not json");
    let resp = proc.read_response();
    assert_eq!(resp["error"]["code"], -32700);

    // 2. Method not found
    let resp = proc.call(json!({
        "jsonrpc": "2.0",
        "id": 80,
        "method": "bogus/method",
        "params": {}
    }));
    assert_eq!(resp["error"]["code"], -32601);

    // 3. Initialize (success)
    let resp = proc.call(initialize_request(81));
    assert!(resp.get("error").is_none() || resp["error"].is_null());

    // 4. Tool error (unknown tool)
    let resp = proc.call(tool_call_request(82, "fake_tool", json!({})));
    assert_tool_error(&resp);

    // 5. Tool success
    let resp = proc.call(tool_call_request(83, "list_agents", json!({})));
    assert_tool_success(&resp);

    proc.shutdown();
}

// ============================================================================
// Notifications
// ============================================================================

/// Unknown notifications produce no response and don't crash.
#[test]
fn mcp_unknown_notification_silent() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    proc.notify(notification("custom/unknown_notification"));
    proc.assert_no_response(Duration::from_secs(2));

    // Server still works
    let resp = proc.call(tool_call_request(90, "list_agents", json!({})));
    assert_tool_success(&resp);

    proc.shutdown();
}

/// Notifications interleaved with requests don't disrupt responses.
#[test]
fn mcp_notifications_interleaved_with_requests() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    proc.notify(notification("notifications/initialized"));
    let resp = proc.call(tool_call_request(91, "list_agents", json!({})));
    assert_tool_success(&resp);

    proc.notify(notification("notifications/initialized"));
    proc.notify(notification("notifications/initialized"));
    let resp = proc.call(tool_call_request(92, "list_agents", json!({})));
    assert_tool_success(&resp);

    proc.shutdown();
}

// ============================================================================
// Tool schema validation
// ============================================================================

/// tools/list returns consistent results across repeated calls.
#[test]
fn mcp_list_tools_idempotent() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp1 = proc.call(list_tools_request(200));
    let resp2 = proc.call(list_tools_request(201));

    // Same tools returned both times
    assert_eq!(
        resp1["result"]["tools"], resp2["result"]["tools"],
        "tools/list should return identical results across calls"
    );

    proc.shutdown();
}

/// spawn_agents inputSchema declares required fields.
#[test]
fn mcp_spawn_agents_schema_has_required_fields() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(list_tools_request(202));
    let tools = resp["result"]["tools"].as_array().unwrap();

    let spawn = tools
        .iter()
        .find(|t| t["name"] == "spawn_agents")
        .expect("spawn_agents should be in tool list");

    let schema = &spawn["inputSchema"];
    assert_eq!(
        schema["type"], "object",
        "inputSchema type should be object"
    );

    // Check required fields
    let required = schema["required"]
        .as_array()
        .expect("spawn_agents should have required fields");
    let required_names: Vec<&str> = required.iter().filter_map(|v| v.as_str()).collect();

    for field in &["issues", "owner", "repo"] {
        assert!(
            required_names.contains(field),
            "spawn_agents should require '{}'. Found: {:?}",
            field,
            required_names
        );
    }

    proc.shutdown();
}

/// All tools have non-empty descriptions.
#[test]
fn mcp_all_tools_have_descriptions() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let resp = proc.call(list_tools_request(203));
    let tools = resp["result"]["tools"].as_array().unwrap();

    for tool in tools {
        let name = tool["name"].as_str().unwrap_or("<unnamed>");
        let desc = tool["description"]
            .as_str()
            .unwrap_or_else(|| panic!("Tool '{}' should have string description", name));
        assert!(!desc.is_empty(), "Tool '{}' has empty description", name);
    }

    proc.shutdown();
}

// ============================================================================
// Payload size variations
// ============================================================================

/// cleanup_agents with many issue IDs — larger proto repeated field payload.
#[test]
fn mcp_cleanup_agents_many_issues() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    let issues: Vec<String> = (1..=50).map(|i| format!("{}", i)).collect();
    let resp = proc.call(tool_call_request(
        210,
        "cleanup_agents",
        json!({"issues": issues}),
    ));

    // All 50 issues are nonexistent — expect structured error response, not a crash
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(
        !has_jsonrpc_error,
        "Unexpected JSON-RPC error for many-issue cleanup: {}",
        resp
    );

    proc.shutdown();
}

// ============================================================================
// Shutdown behavior
// ============================================================================

/// Closing stdin causes clean exit.
#[test]
fn mcp_clean_shutdown() {
    let dir = setup_test_dir();
    let mut proc = spawn_initialized(dir.path());

    // Do some work first
    let resp = proc.call(tool_call_request(220, "list_agents", json!({})));
    assert_tool_success(&resp);

    // Close stdin → server should exit cleanly
    let status = proc.shutdown();
    assert!(
        status.success(),
        "Server should exit with status 0 on stdin close"
    );
}

/// Double initialize doesn't crash (idempotent server startup).
#[test]
fn mcp_double_initialize() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    let resp1 = proc.call(initialize_request(230));
    assert!(resp1.get("error").is_none() || resp1["error"].is_null());

    let resp2 = proc.call(initialize_request(231));
    assert!(resp2.get("error").is_none() || resp2["error"].is_null());

    // Both should return same server info
    assert_eq!(
        resp1["result"]["serverInfo"], resp2["result"]["serverInfo"],
        "Double initialize should return same serverInfo"
    );

    proc.shutdown();
}

/// tools/call before initialize — server should still handle it (no hard init requirement).
#[test]
fn mcp_tool_call_before_initialize() {
    let dir = setup_test_dir();
    let mut proc = McpProcess::spawn(dir.path());

    // Call a tool without initializing first
    let resp = proc.call(tool_call_request(240, "list_agents", json!({})));

    // The server doesn't enforce initialization order — this should work or error gracefully
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    if has_jsonrpc_error {
        // If it errors, it should be a structured error, not a crash
        assert!(
            resp["error"]["code"].is_number(),
            "Error should have numeric code"
        );
    } else {
        // If it succeeds, should be normal tool response
        assert!(resp["result"].is_object(), "Expected result object");
    }

    proc.shutdown();
}
