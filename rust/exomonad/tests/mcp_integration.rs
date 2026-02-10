//! MCP HTTP integration tests.
//!
//! Black-box tests that spawn `exomonad serve --port <port>` as a subprocess and
//! drive it via JSON-RPC over HTTP (StreamableHttp transport). Catches wire format
//! mismatches by exercising the full pipeline:
//!   HTTP POST → axum → rmcp StreamableHttp → WASM (Haskell) → protobuf effects
//!     → Rust handlers → protobuf response → WASM decode → JSON-RPC response
//!
//! The default test role is `tl`, which provides: spawn_agents, cleanup_agents,
//! cleanup_merged_agents, list_agents, popup.
//!
//! Each test spawns a full exomonad serve subprocess with ~4MB WASM loading, so we
//! limit concurrency to avoid resource exhaustion. The SUBPROCESS_SEMAPHORE caps
//! concurrent spawns.
//!
//! **Run with limited threads:** `cargo test -p exomonad --test mcp_integration -- --test-threads=2`
//! Default thread count can cause timeouts due to WASM compilation overhead.

#![allow(deprecated)] // cargo_bin function — no macro alternative that returns PathBuf

use assert_cmd::cargo::cargo_bin;
use reqwest::blocking::Client;
use reqwest::header::{HeaderMap, HeaderValue, ACCEPT, CONTENT_TYPE};
use serde_json::{json, Value};
use std::net::TcpListener;
use std::process::{Child, Command, Stdio};
use std::sync::{Mutex, OnceLock};
use std::time::Duration;
use tempfile::TempDir;

/// Limits concurrent subprocess spawns to avoid resource exhaustion.
/// Each exomonad process loads ~4MB of WASM, so many simultaneous processes
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
// McpServer — HTTP subprocess lifecycle helper
// ============================================================================

struct McpServer {
    child: Child,
    port: u16,
    client: Client,
    session_id: Option<String>,
    _semaphore_guard: SemaphoreGuard<'static>,
    _work_dir: TempDir,
}

/// Find a free TCP port by binding to port 0 and reading the assigned port.
fn find_free_port() -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind to port 0");
    listener.local_addr().unwrap().port()
}

/// Path to the workspace root's WASM directory.
fn wasm_source_dir() -> std::path::PathBuf {
    let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // rust/exomonad/ → exomonad/ (workspace root)
    manifest_dir
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join(".exomonad/wasm")
}

/// Parse a JSON-RPC response from an SSE body.
/// SSE format: lines starting with "data: " contain the payload.
/// We want the last non-empty data line (which has the actual response).
fn parse_sse_json(body: &str) -> Value {
    let mut last_json = None;
    for line in body.lines() {
        if let Some(data) = line.strip_prefix("data: ") {
            let trimmed = data.trim();
            if !trimmed.is_empty() {
                if let Ok(val) = serde_json::from_str::<Value>(trimmed) {
                    last_json = Some(val);
                }
            }
        }
    }
    last_json.unwrap_or_else(|| panic!("No JSON found in SSE body: {}", body))
}

impl McpServer {
    /// Spawn `exomonad serve --port <port>` in an isolated temp directory.
    /// Blocks until the server's health endpoint responds.
    fn spawn(work_dir: TempDir) -> Self {
        let guard = subprocess_semaphore().acquire();
        let port = find_free_port();

        let mut child = Command::new(cargo_bin("exomonad"))
            .args(["serve", "--port", &port.to_string()])
            .current_dir(work_dir.path())
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to spawn exomonad serve");

        let client = Client::builder()
            .timeout(Duration::from_secs(15))
            .build()
            .unwrap();

        // Wait for health endpoint to respond
        let health_url = format!("http://127.0.0.1:{}/health", port);
        let mut ready = false;
        for i in 0..100 {
            std::thread::sleep(Duration::from_millis(100));
            // Check if process has exited early
            if i % 10 == 5 {
                if let Ok(Some(status)) = child.try_wait() {
                    let mut stderr_buf = String::new();
                    if let Some(ref mut stderr) = child.stderr {
                        use std::io::Read;
                        let _ = stderr.read_to_string(&mut stderr_buf);
                    }
                    panic!(
                        "Server exited early with status: {}. Port: {}. Stderr: {}",
                        status, port, stderr_buf
                    );
                }
            }
            if let Ok(resp) = client.get(&health_url).send() {
                if resp.status().is_success() {
                    ready = true;
                    break;
                }
            }
        }
        if !ready {
            let mut stderr_buf = String::new();
            if let Some(ref mut stderr) = child.stderr {
                use std::io::Read;
                let _ = stderr.read_to_string(&mut stderr_buf);
            }
            panic!(
                "Server failed to start on port {} within 10s. Stderr: {}",
                port, stderr_buf
            );
        }

        Self {
            child,
            port,
            client,
            session_id: None,
            _semaphore_guard: guard,
            _work_dir: work_dir,
        }
    }

    fn mcp_url(&self) -> String {
        format!("http://127.0.0.1:{}/tl/mcp", self.port)
    }

    /// Send a JSON-RPC request via HTTP POST and return the parsed response.
    /// Automatically includes session ID header if one was captured from initialize.
    fn call(&mut self, request: Value) -> Value {
        let url = self.mcp_url();
        let mut headers = HeaderMap::new();
        headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
        headers.insert(
            ACCEPT,
            HeaderValue::from_static("application/json, text/event-stream"),
        );
        if let Some(ref sid) = self.session_id {
            headers.insert("Mcp-Session-Id", HeaderValue::from_str(sid).unwrap());
        }

        let resp = self
            .client
            .post(&url)
            .headers(headers)
            .json(&request)
            .send()
            .expect("HTTP request failed");

        // Capture session ID from response headers (set by initialize)
        if let Some(sid) = resp.headers().get("Mcp-Session-Id") {
            self.session_id = Some(sid.to_str().unwrap().to_string());
        }

        let status = resp.status();
        let content_type = resp
            .headers()
            .get(CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("")
            .to_string();
        let body = resp.text().expect("Failed to read response body");

        if body.is_empty() {
            // Some protocol responses (e.g., notifications) return empty body
            json!({"_empty": true, "_status": status.as_u16()})
        } else if content_type.contains("text/event-stream") {
            // SSE response — extract JSON from last "data:" line
            parse_sse_json(&body)
        } else {
            serde_json::from_str(&body).unwrap_or_else(|e| {
                panic!(
                    "Failed to parse response JSON. Status: {}. Error: {}. Body: {}",
                    status, e, body
                )
            })
        }
    }

    /// Send a notification (JSON-RPC request with no `id` field).
    /// In StreamableHttp, notifications get 202 Accepted with no body.
    fn notify(&mut self, request: Value) {
        let url = self.mcp_url();
        let mut headers = HeaderMap::new();
        headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
        headers.insert(
            ACCEPT,
            HeaderValue::from_static("application/json, text/event-stream"),
        );
        if let Some(ref sid) = self.session_id {
            headers.insert("Mcp-Session-Id", HeaderValue::from_str(sid).unwrap());
        }

        let resp = self
            .client
            .post(&url)
            .headers(headers)
            .json(&request)
            .send()
            .expect("Notification HTTP request failed");

        // Notifications should get 2xx with no meaningful body
        let status = resp.status();
        assert!(
            status.is_success(),
            "Notification should succeed, got status: {}",
            status
        );
    }

    /// Send raw string body (for malformed JSON tests).
    fn send_raw(&mut self, data: &str) -> reqwest::blocking::Response {
        let url = self.mcp_url();
        let mut headers = HeaderMap::new();
        headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
        headers.insert(
            ACCEPT,
            HeaderValue::from_static("application/json, text/event-stream"),
        );
        if let Some(ref sid) = self.session_id {
            headers.insert("Mcp-Session-Id", HeaderValue::from_str(sid).unwrap());
        }

        self.client
            .post(&url)
            .headers(headers)
            .body(data.to_string())
            .send()
            .expect("Raw HTTP request failed")
    }

    /// Kill the server process.
    fn shutdown(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

impl Drop for McpServer {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

// ============================================================================
// Test directory setup
// ============================================================================

/// Create an isolated temp directory with git repo, .exomonad config, and WASM symlink.
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

    // Symlink WASM directory so serve can find wasm-guest-tl.wasm
    let wasm_dir = dir.path().join(".exomonad/wasm");
    let source = wasm_source_dir();
    assert!(
        source.exists(),
        "WASM source dir not found: {}. Run `just wasm-all` first.",
        source.display()
    );
    #[cfg(unix)]
    std::os::unix::fs::symlink(&source, &wasm_dir).expect("Failed to symlink WASM dir");
    #[cfg(not(unix))]
    {
        std::fs::create_dir_all(&wasm_dir).unwrap();
        for entry in std::fs::read_dir(&source).unwrap() {
            let entry = entry.unwrap();
            std::fs::copy(entry.path(), wasm_dir.join(entry.file_name())).unwrap();
        }
    }

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

/// Spawn server and perform initialize handshake.
fn spawn_initialized() -> McpServer {
    let dir = setup_test_dir();
    let mut server = McpServer::spawn(dir);

    // Initialize
    let resp = server.call(initialize_request(0));
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Initialize failed: {}",
        resp
    );

    // Send initialized notification
    server.notify(notification("notifications/initialized"));

    server
}

// ============================================================================
// Tests
// ============================================================================

/// Server starts, WASM loads, returns protocolVersion + serverInfo.
#[test]
fn mcp_initialize_handshake() {
    let dir = setup_test_dir();
    let mut server = McpServer::spawn(dir);

    let resp = server.call(initialize_request(1));

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

    server.shutdown();
}

/// WASM handle_list_tools works, returns tool definitions for the tl role.
#[test]
fn mcp_list_tools_returns_definitions() {
    let mut server = spawn_initialized();

    let resp = server.call(list_tools_request(2));

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

    server.shutdown();
}

/// Full proto roundtrip via agent.list effect.
/// Haskell encodes ListAgentsRequest → Rust decodes → AgentHandler → Rust encodes
/// ListAgentsResponse → Haskell decodes. Varint128 bug surfaces here.
#[test]
fn mcp_tool_call_list_agents() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(3, "list_agents", json!({})));
    let inner = assert_tool_success(&resp);

    // Fresh directory has no agents — should return empty list or object
    assert!(
        inner.is_object() || inner.is_array(),
        "Expected structured list_agents response: {}",
        inner
    );

    server.shutdown();
}

/// cleanup_agents with empty issues list — proto roundtrip through agent.cleanup namespace.
#[test]
fn mcp_tool_call_cleanup_agents() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(
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

    server.shutdown();
}

/// cleanup_merged_agents — proto roundtrip through agent.cleanup_merged namespace.
#[test]
fn mcp_tool_call_cleanup_merged_agents() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(5, "cleanup_merged_agents", json!({})));

    // Fresh repo has no merged branches — should succeed with empty result
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    if !has_jsonrpc_error {
        let result = &resp["result"];
        let content = &result["content"];
        assert!(content.is_array(), "Expected content array, got: {}", resp);
    }

    server.shutdown();
}

/// Nonexistent tool → isError: true (not a crash).
#[test]
fn mcp_invalid_tool_returns_error() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(7, "nonexistent_tool_xyz", json!({})));

    assert_tool_error(&resp);

    server.shutdown();
}

/// Malformed JSON body → HTTP error (not a server crash).
/// In HTTP mode, each request is independent, so malformed requests don't affect
/// subsequent valid requests.
#[test]
fn mcp_malformed_json_returns_error() {
    let mut server = spawn_initialized();

    let resp = server.send_raw("this is not valid json at all!!!");
    // Server should return an HTTP error status (4xx)
    assert!(
        !resp.status().is_success() || {
            // Some servers return 200 with a JSON-RPC error
            let body: Value = resp.json().unwrap_or(json!({}));
            body.get("error").is_some() && !body["error"].is_null()
        },
        "Expected error for malformed JSON"
    );

    // Server still works after malformed request (HTTP requests are independent)
    let resp = server.call(tool_call_request(50, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

/// Unknown method → JSON-RPC error code.
#[test]
fn mcp_unknown_method_returns_method_not_found() {
    let mut server = spawn_initialized();

    let resp = server.call(json!({
        "jsonrpc": "2.0",
        "id": 9,
        "method": "completely/unknown",
        "params": {}
    }));

    // rmcp may return method-not-found or invalid-request for unknown methods
    assert!(
        resp.get("error").is_some() && !resp["error"].is_null(),
        "Expected JSON-RPC error for unknown method, got: {}",
        resp
    );

    server.shutdown();
}

/// notifications/initialized (no id) produces no JSON-RPC response body.
#[test]
fn mcp_notification_no_response() {
    let mut server = spawn_initialized();

    // Send a notification — in HTTP, this should return 202 with no body
    server.notify(notification("notifications/initialized"));

    // Server still healthy after notification
    let resp = server.call(tool_call_request(10, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

/// 10 sequential list_agents calls — WASM state doesn't corrupt across calls.
#[test]
fn mcp_multiple_sequential_calls() {
    let mut server = spawn_initialized();

    for i in 0..10u64 {
        let resp = server.call(tool_call_request(100 + i, "list_agents", json!({})));
        let inner = assert_tool_success(&resp);
        assert!(
            inner.is_object() || inner.is_array(),
            "Call {} failed: expected structured response, got: {}",
            i,
            inner
        );
    }

    server.shutdown();
}

/// Multiple different operations interleaved — validates server handles mixed traffic.
#[test]
fn mcp_interleaved_operations() {
    let mut server = spawn_initialized();

    // list_agents
    let resp = server.call(tool_call_request(20, "list_agents", json!({})));
    assert_tool_success(&resp);

    // unknown tool (should error, not crash)
    let resp = server.call(tool_call_request(21, "no_such_tool", json!({})));
    assert_tool_error(&resp);

    // list_agents again (server should still be healthy)
    let resp = server.call(tool_call_request(22, "list_agents", json!({})));
    assert_tool_success(&resp);

    // tools/list still works
    let resp = server.call(list_tools_request(23));
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "tools/list failed after error recovery: {}",
        resp
    );

    server.shutdown();
}

// ============================================================================
// Argument validation — WASM-level JSON parsing
// ============================================================================

/// spawn_agents missing required `issues` field → tool-level parse error.
#[test]
fn mcp_spawn_agents_missing_required_field() {
    let mut server = spawn_initialized();

    // Missing `issues` (required)
    let resp = server.call(tool_call_request(
        30,
        "spawn_agents",
        json!({"owner": "test", "repo": "test"}),
    ));
    assert_tool_error(&resp);

    // Server still healthy after parse error
    let resp = server.call(tool_call_request(31, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

/// spawn_agents with `issues` as string instead of array → parse error.
#[test]
fn mcp_spawn_agents_wrong_type_issues() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(
        32,
        "spawn_agents",
        json!({"issues": "123", "owner": "test", "repo": "test"}),
    ));
    assert_tool_error(&resp);

    server.shutdown();
}

/// spawn_agents with empty issues array — valid parse, empty batch.
#[test]
fn mcp_spawn_agents_empty_issues() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(
        33,
        "spawn_agents",
        json!({"issues": [], "owner": "test", "repo": "test"}),
    ));

    // Empty issues is valid at the schema level — handler may succeed with empty result
    // or fail at the service level. Either way, no crash.
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);

    server.shutdown();
}

/// cleanup_agents missing required `issues` field.
#[test]
fn mcp_cleanup_agents_missing_issues() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(34, "cleanup_agents", json!({})));
    assert_tool_error(&resp);

    server.shutdown();
}

/// cleanup_agents with nonexistent issue IDs — exercises batch error response path.
#[test]
fn mcp_cleanup_agents_nonexistent_issues() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(
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

    server.shutdown();
}

/// cleanup_agents with force=true — optional boolean arg exercises proto encoding.
#[test]
fn mcp_cleanup_agents_with_force() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(
        36,
        "cleanup_agents",
        json!({"issues": ["12345"], "force": true}),
    ));

    // Issue doesn't exist, but the force flag should be accepted and encoded correctly.
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);

    server.shutdown();
}

/// spawn_agents with all optional fields — exercises full proto message encoding.
#[test]
fn mcp_spawn_agents_all_optional_fields() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(
        37,
        "spawn_agents",
        json!({
            "issues": ["42"],
            "owner": "testowner",
            "repo": "testrepo",
            "agent_type": "claude"
        }),
    ));

    // Will fail at the service level (no Zellij, no GitHub token in test), but
    // the proto encode/decode and argument parsing should complete.
    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);

    server.shutdown();
}

/// Extra unknown fields in tool arguments are silently ignored (Aeson default).
#[test]
fn mcp_tool_call_extra_args_ignored() {
    let mut server = spawn_initialized();

    let resp = server.call(tool_call_request(
        38,
        "list_agents",
        json!({"unknown_field": "should_be_ignored", "another": 42}),
    ));

    // list_agents takes no args — extra fields should be ignored, not cause errors
    assert_tool_success(&resp);

    server.shutdown();
}

// ============================================================================
// JSON-RPC protocol edge cases
// ============================================================================

/// String request ID preserved in response.
#[test]
fn mcp_string_request_id() {
    let dir = setup_test_dir();
    let mut server = McpServer::spawn(dir);

    // Initialize with string ID
    let resp = server.call(json!({
        "jsonrpc": "2.0",
        "id": "abc-def-123",
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test", "version": "0.0.1"}
        }
    }));

    assert_eq!(
        resp["id"], "abc-def-123",
        "Response should echo back string ID"
    );

    server.shutdown();
}

/// Null request ID handling after initialize.
#[test]
fn mcp_null_request_id_is_notification() {
    let mut server = spawn_initialized();

    // Send a request with null id — treated as notification in HTTP (no response body)
    let resp = server.send_raw(
        &serde_json::to_string(&json!({
            "jsonrpc": "2.0",
            "id": null,
            "method": "tools/list",
            "params": {}
        }))
        .unwrap(),
    );

    // Server should handle gracefully — either 2xx or error, not crash
    assert!(
        resp.status().is_success() || resp.status().is_client_error(),
        "Unexpected status for null-id request: {}",
        resp.status()
    );

    // Server still healthy
    let resp = server.call(tool_call_request(45, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

/// Request with no params field — params defaults to empty.
#[test]
fn mcp_missing_params() {
    let dir = setup_test_dir();
    let mut server = McpServer::spawn(dir);

    // Initialize with proper params
    let resp = server.call(json!({
        "jsonrpc": "2.0",
        "id": 40,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test", "version": "0.0.1"}
        }
    }));

    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "Initialize should succeed: {}",
        resp
    );

    // Now test missing params on a post-initialize request
    server.notify(notification("notifications/initialized"));
    let resp = server.call(json!({
        "jsonrpc": "2.0",
        "id": 41,
        "method": "tools/list"
    }));

    // tools/list with no params should succeed (defaults to empty)
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "tools/list without params should succeed: {}",
        resp
    );

    server.shutdown();
}

/// Large numeric request ID.
#[test]
fn mcp_large_numeric_id() {
    let dir = setup_test_dir();
    let mut server = McpServer::spawn(dir);

    let resp = server.call(json!({
        "jsonrpc": "2.0",
        "id": 999999999,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test", "version": "0.0.1"}
        }
    }));

    assert_eq!(
        resp["id"], 999999999,
        "Response should echo large numeric ID"
    );

    server.shutdown();
}

/// tools/call with missing name field.
#[test]
fn mcp_tool_call_missing_name() {
    let mut server = spawn_initialized();

    let resp = server.call(json!({
        "jsonrpc": "2.0",
        "id": 41,
        "method": "tools/call",
        "params": {
            "arguments": {}
        }
    }));

    // Missing tool name — should result in an error (empty tool name → unknown tool)
    assert_tool_error(&resp);

    server.shutdown();
}

/// tools/call with missing arguments field — defaults to empty object.
#[test]
fn mcp_tool_call_missing_arguments() {
    let mut server = spawn_initialized();

    let resp = server.call(json!({
        "jsonrpc": "2.0",
        "id": 42,
        "method": "tools/call",
        "params": {
            "name": "list_agents"
        }
    }));

    // list_agents doesn't need arguments, so this should work
    assert_tool_success(&resp);

    server.shutdown();
}

// ============================================================================
// Error recovery sequences
// ============================================================================

/// Server recovers after malformed JSON — HTTP requests are independent.
#[test]
fn mcp_recovery_after_malformed_json() {
    let mut server = spawn_initialized();

    // Send malformed JSON — should get error response
    let resp = server.send_raw("{{{invalid json garbage!!!}}}");
    assert!(
        !resp.status().is_success() || {
            let body: Value = resp.json().unwrap_or(json!({}));
            body.get("error").is_some()
        },
        "Expected error for malformed JSON"
    );

    // Server still healthy — HTTP requests are independent
    let resp = server.call(tool_call_request(50, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

/// Multiple malformed requests — server stays healthy (HTTP is stateless per-request).
#[test]
fn mcp_recovery_after_multiple_parse_errors() {
    let mut server = spawn_initialized();

    for i in 0..5 {
        let _resp = server.send_raw(&format!("garbage line {}", i));
    }

    // Server still healthy
    let resp = server.call(tool_call_request(51, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

/// Alternating errors and successes — stress tests error recovery.
#[test]
fn mcp_alternating_errors_and_successes() {
    let mut server = spawn_initialized();

    for i in 0..5u64 {
        // Success
        let resp = server.call(tool_call_request(60 + i * 2, "list_agents", json!({})));
        assert_tool_success(&resp);

        // Error (unknown tool)
        let resp = server.call(tool_call_request(61 + i * 2, "nonexistent", json!({})));
        assert_tool_error(&resp);
    }

    // Final success — server still healthy
    let resp = server.call(tool_call_request(70, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

/// Mix of method errors and tool errors in sequence after initialize.
#[test]
fn mcp_mixed_error_types() {
    let mut server = spawn_initialized();

    // 1. Method not found (valid JSON, unknown method)
    let resp = server.call(json!({
        "jsonrpc": "2.0",
        "id": 80,
        "method": "bogus/method",
        "params": {}
    }));
    assert!(
        resp.get("error").is_some() && !resp["error"].is_null(),
        "Expected error for unknown method, got: {}",
        resp
    );

    // 2. Tool error (unknown tool)
    let resp = server.call(tool_call_request(82, "fake_tool", json!({})));
    assert_tool_error(&resp);

    // 3. Tool success — server still healthy after errors
    let resp = server.call(tool_call_request(83, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

// ============================================================================
// Notifications
// ============================================================================

/// Unknown notifications produce no response body and don't crash.
#[test]
fn mcp_unknown_notification_silent() {
    let mut server = spawn_initialized();

    server.notify(notification("custom/unknown_notification"));

    // Server still works
    let resp = server.call(tool_call_request(90, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

/// Notifications interleaved with requests don't disrupt responses.
#[test]
fn mcp_notifications_interleaved_with_requests() {
    let mut server = spawn_initialized();

    server.notify(notification("notifications/initialized"));
    let resp = server.call(tool_call_request(91, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.notify(notification("notifications/initialized"));
    server.notify(notification("notifications/initialized"));
    let resp = server.call(tool_call_request(92, "list_agents", json!({})));
    assert_tool_success(&resp);

    server.shutdown();
}

// ============================================================================
// Tool schema validation
// ============================================================================

/// tools/list returns consistent results across repeated calls.
#[test]
fn mcp_list_tools_idempotent() {
    let mut server = spawn_initialized();

    let resp1 = server.call(list_tools_request(200));
    let resp2 = server.call(list_tools_request(201));

    // Same tools returned both times
    assert_eq!(
        resp1["result"]["tools"], resp2["result"]["tools"],
        "tools/list should return identical results across calls"
    );

    server.shutdown();
}

/// spawn_agents inputSchema declares required fields.
#[test]
fn mcp_spawn_agents_schema_has_required_fields() {
    let mut server = spawn_initialized();

    let resp = server.call(list_tools_request(202));
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

    server.shutdown();
}

/// All tools have non-empty descriptions.
#[test]
fn mcp_all_tools_have_descriptions() {
    let mut server = spawn_initialized();

    let resp = server.call(list_tools_request(203));
    let tools = resp["result"]["tools"].as_array().unwrap();

    for tool in tools {
        let name = tool["name"].as_str().unwrap_or("<unnamed>");
        let desc = tool["description"]
            .as_str()
            .unwrap_or_else(|| panic!("Tool '{}' should have string description", name));
        assert!(!desc.is_empty(), "Tool '{}' has empty description", name);
    }

    server.shutdown();
}

// ============================================================================
// Payload size variations
// ============================================================================

/// cleanup_agents with many issue IDs — larger proto repeated field payload.
#[test]
fn mcp_cleanup_agents_many_issues() {
    let mut server = spawn_initialized();

    let issues: Vec<String> = (1..=50).map(|i| format!("{}", i)).collect();
    let resp = server.call(tool_call_request(
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

    server.shutdown();
}

// ============================================================================
// Shutdown behavior
// ============================================================================

/// Server responds to health check after sustained traffic.
#[test]
fn mcp_clean_shutdown() {
    let mut server = spawn_initialized();

    // Do some work first
    let resp = server.call(tool_call_request(220, "list_agents", json!({})));
    assert_tool_success(&resp);

    // Verify health endpoint still works
    let health_url = format!("http://127.0.0.1:{}/health", server.port);
    let resp = server
        .client
        .get(&health_url)
        .send()
        .expect("Health check failed");
    assert!(
        resp.status().is_success(),
        "Health check should succeed after traffic"
    );

    server.shutdown();
}

/// Multiple sessions — server can handle multiple initialize handshakes.
/// In HTTP mode, each initialize creates a new session.
#[test]
fn mcp_double_initialize() {
    let dir = setup_test_dir();
    let mut server = McpServer::spawn(dir);

    let resp1 = server.call(initialize_request(230));
    assert!(
        resp1.get("error").is_none() || resp1["error"].is_null(),
        "First initialize should succeed: {}",
        resp1
    );
    let session1 = server.session_id.clone();

    // Second initialize — in HTTP, this creates a new session
    server.session_id = None; // Clear to simulate new client
    let resp2 = server.call(initialize_request(231));
    assert!(
        resp2.get("error").is_none() || resp2["error"].is_null(),
        "Second initialize should succeed (new session): {}",
        resp2
    );
    let session2 = server.session_id.clone();

    // Sessions should be different
    if let (Some(s1), Some(s2)) = (session1, session2) {
        assert_ne!(s1, s2, "Each initialize should create a new session");
    }

    server.shutdown();
}

/// Request without session ID before initialize — server should reject.
#[test]
fn mcp_tool_call_before_initialize() {
    let dir = setup_test_dir();
    let mut server = McpServer::spawn(dir);

    // Try tool call without initializing first (no session ID)
    let resp = server.send_raw(
        &serde_json::to_string(&tool_call_request(240, "list_agents", json!({}))).unwrap(),
    );

    // Server should reject — either 4xx HTTP or JSON-RPC error
    let status = resp.status();
    if status.is_success() {
        let body: Value = resp.json().unwrap_or(json!({}));
        assert!(
            body.get("error").is_some() && !body["error"].is_null(),
            "Pre-initialize tool call should error: {}",
            body
        );
    }
    // 4xx is also acceptable — server rejected the request

    server.shutdown();
}
