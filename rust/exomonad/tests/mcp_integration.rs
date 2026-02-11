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
//! All tests share a single `exomonad serve` process (via OnceLock). Most tests
//! share a single MCP session to avoid exhausting the server's session pool.
//! Tests that exercise protocol edge cases (initialization, ID formats) create
//! their own lightweight sessions.
//!
//! **Run with:** `cargo test -p exomonad --test mcp_integration`

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

// ============================================================================
// SharedServer — singleton subprocess, one per test run
// ============================================================================

struct SharedServer {
    _child: Child,
    port: u16,
    _work_dir: TempDir,
}

// Safety: Child + TempDir are Send. The shared server is only written once via OnceLock.
unsafe impl Sync for SharedServer {}

fn shared_server() -> &'static SharedServer {
    static INSTANCE: OnceLock<SharedServer> = OnceLock::new();
    INSTANCE.get_or_init(|| {
        let dir = setup_test_dir();
        let port = find_free_port();

        let mut child = Command::new(cargo_bin("exomonad"))
            .args(["serve", "--port", &port.to_string()])
            .current_dir(dir.path())
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to spawn exomonad serve");

        let client = Client::builder()
            .timeout(Duration::from_secs(15))
            .build()
            .unwrap();

        // Wait for health endpoint
        let health_url = format!("http://127.0.0.1:{}/health", port);
        let mut ready = false;
        for i in 0..100 {
            std::thread::sleep(Duration::from_millis(100));
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

        SharedServer {
            _child: child,
            port,
            _work_dir: dir,
        }
    })
}

// ============================================================================
// McpClient — lightweight HTTP session
// ============================================================================

struct McpClient {
    port: u16,
    client: Client,
    session_id: Option<String>,
}

/// Find a free TCP port by binding to port 0 and reading the assigned port.
fn find_free_port() -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind to port 0");
    listener.local_addr().unwrap().port()
}

/// Path to the workspace root's WASM directory.
fn wasm_source_dir() -> std::path::PathBuf {
    let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join(".exomonad/wasm")
}

/// Parse a JSON-RPC response from an SSE body.
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

impl McpClient {
    fn new(port: u16) -> Self {
        let client = Client::builder()
            .timeout(Duration::from_secs(15))
            .build()
            .unwrap();
        Self {
            port,
            client,
            session_id: None,
        }
    }

    fn mcp_url(&self) -> String {
        format!("http://127.0.0.1:{}/tl/mcp", self.port)
    }

    /// Send a JSON-RPC request via HTTP POST and return the parsed response.
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
            json!({"_empty": true, "_status": status.as_u16()})
        } else if content_type.contains("text/event-stream") {
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

        let status = resp.status();
        assert!(
            status.is_success(),
            "Notification should succeed, got status: {}",
            status
        );
    }

    /// Send raw string body (for malformed JSON tests).
    fn send_raw(&self, data: &str) -> reqwest::blocking::Response {
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
}

// ============================================================================
// Test directory setup
// ============================================================================

/// Create an isolated temp directory with git repo, .exomonad config, and WASM symlink.
fn setup_test_dir() -> TempDir {
    let dir = TempDir::new().expect("Failed to create temp dir");

    let config_dir = dir.path().join(".exomonad");
    std::fs::create_dir_all(&config_dir).expect("Failed to create .exomonad dir");
    std::fs::write(
        config_dir.join("config.toml"),
        "default_role = \"tl\"\nproject_dir = \".\"\nzellij_session = \"test\"\n",
    )
    .expect("Failed to write config.toml");

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

// ============================================================================
// Shared session — most tests use this to avoid exhausting server session pool
// ============================================================================

/// Returns a mutex-guarded initialized session shared across tests.
/// The server has a limited session pool (~15), so most tests share one session.
fn shared_session() -> &'static Mutex<McpClient> {
    static INSTANCE: OnceLock<Mutex<McpClient>> = OnceLock::new();
    INSTANCE.get_or_init(|| {
        let server = shared_server();
        let mut client = McpClient::new(server.port);

        let resp = client.call(initialize_request(0));
        assert!(
            resp.get("error").is_none() || resp["error"].is_null(),
            "Initialize failed: {}",
            resp
        );
        client.notify(notification("notifications/initialized"));

        Mutex::new(client)
    })
}

/// Create a new uninitialized client pointing at the shared server.
/// Use sparingly — each initialize consumes a session slot.
fn new_raw_client() -> McpClient {
    let server = shared_server();
    McpClient::new(server.port)
}

// ============================================================================
// Tests — Protocol handshake (use dedicated sessions)
// ============================================================================

/// Server starts, WASM loads, returns protocolVersion + serverInfo.
#[test]
fn mcp_initialize_handshake() {
    let mut client = new_raw_client();

    let resp = client.call(initialize_request(1));

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
}

/// String request ID preserved in response.
#[test]
fn mcp_string_request_id() {
    let mut client = new_raw_client();

    let resp = client.call(json!({
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
}

/// Large numeric request ID.
#[test]
fn mcp_large_numeric_id() {
    let mut client = new_raw_client();

    let resp = client.call(json!({
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
}

/// Multiple sessions — server can handle multiple initialize handshakes.
#[test]
fn mcp_double_initialize() {
    let mut client = new_raw_client();

    let resp1 = client.call(initialize_request(230));
    assert!(
        resp1.get("error").is_none() || resp1["error"].is_null(),
        "First initialize should succeed: {}",
        resp1
    );
    let session1 = client.session_id.clone();

    // Second initialize — creates a new session
    client.session_id = None;
    let resp2 = client.call(initialize_request(231));
    assert!(
        resp2.get("error").is_none() || resp2["error"].is_null(),
        "Second initialize should succeed (new session): {}",
        resp2
    );
    let session2 = client.session_id.clone();

    if let (Some(s1), Some(s2)) = (session1, session2) {
        assert_ne!(s1, s2, "Each initialize should create a new session");
    }
}

/// Request without session ID before initialize — server should reject.
#[test]
fn mcp_tool_call_before_initialize() {
    let client = new_raw_client();

    let resp = client.send_raw(
        &serde_json::to_string(&tool_call_request(240, "list_agents", json!({}))).unwrap(),
    );

    let status = resp.status();
    if status.is_success() {
        let body: Value = resp.json().unwrap_or(json!({}));
        assert!(
            body.get("error").is_some() && !body["error"].is_null(),
            "Pre-initialize tool call should error: {}",
            body
        );
    }
}

/// Request with no params field — params defaults to empty.
#[test]
fn mcp_missing_params() {
    let mut client = new_raw_client();

    let resp = client.call(json!({
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

    client.notify(notification("notifications/initialized"));
    let resp = client.call(json!({
        "jsonrpc": "2.0",
        "id": 41,
        "method": "tools/list"
    }));

    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "tools/list without params should succeed: {}",
        resp
    );
}

// ============================================================================
// Tests — Tool listing (shared session)
// ============================================================================

/// WASM handle_list_tools works, returns tool definitions for the tl role.
#[test]
fn mcp_list_tools_returns_definitions() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(list_tools_request(2));

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

    let tool_names: Vec<&str> = tools_arr
        .iter()
        .filter_map(|t| t["name"].as_str())
        .collect();

    for expected in &["spawn_agents", "cleanup_agents", "list_agents"] {
        assert!(
            tool_names.contains(expected),
            "Missing expected tool '{}'. Found: {:?}",
            expected,
            tool_names
        );
    }

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
}

/// tools/list returns consistent results across repeated calls.
#[test]
fn mcp_list_tools_idempotent() {
    let mut client = shared_session().lock().unwrap();

    let resp1 = client.call(list_tools_request(200));
    let resp2 = client.call(list_tools_request(201));

    assert_eq!(
        resp1["result"]["tools"], resp2["result"]["tools"],
        "tools/list should return identical results across calls"
    );
}

/// spawn_agents inputSchema declares required fields.
#[test]
fn mcp_spawn_agents_schema_has_required_fields() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(list_tools_request(202));
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
}

/// All tools have non-empty descriptions.
#[test]
fn mcp_all_tools_have_descriptions() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(list_tools_request(203));
    let tools = resp["result"]["tools"].as_array().unwrap();

    for tool in tools {
        let name = tool["name"].as_str().unwrap_or("<unnamed>");
        let desc = tool["description"]
            .as_str()
            .unwrap_or_else(|| panic!("Tool '{}' should have string description", name));
        assert!(!desc.is_empty(), "Tool '{}' has empty description", name);
    }
}

// ============================================================================
// Tests — Tool calls (shared session)
// ============================================================================

/// Full proto roundtrip via agent.list effect.
#[test]
fn mcp_tool_call_list_agents() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(3, "list_agents", json!({})));
    let inner = assert_tool_success(&resp);

    assert!(
        inner.is_object() || inner.is_array(),
        "Expected structured list_agents response: {}",
        inner
    );
}

/// cleanup_agents with empty issues list — proto roundtrip through agent.cleanup namespace.
#[test]
fn mcp_tool_call_cleanup_agents() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        4,
        "cleanup_agents",
        json!({"issues": []}),
    ));

    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    if !has_jsonrpc_error {
        let result = &resp["result"];
        let content = &result["content"];
        assert!(content.is_array(), "Expected content array, got: {}", resp);
    }
}

/// cleanup_merged_agents — proto roundtrip through agent.cleanup_merged namespace.
#[test]
fn mcp_tool_call_cleanup_merged_agents() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(5, "cleanup_merged_agents", json!({})));

    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    if !has_jsonrpc_error {
        let result = &resp["result"];
        let content = &result["content"];
        assert!(content.is_array(), "Expected content array, got: {}", resp);
    }
}

/// Nonexistent tool → isError: true (not a crash).
#[test]
fn mcp_invalid_tool_returns_error() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(7, "nonexistent_tool_xyz", json!({})));

    assert_tool_error(&resp);
}

/// Malformed JSON body → HTTP error (not a server crash).
#[test]
fn mcp_malformed_json_returns_error() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.send_raw("this is not valid json at all!!!");
    assert!(
        !resp.status().is_success() || {
            let body: Value = resp.json().unwrap_or(json!({}));
            body.get("error").is_some() && !body["error"].is_null()
        },
        "Expected error for malformed JSON"
    );

    // Server still works after malformed request
    let resp = client.call(tool_call_request(50, "list_agents", json!({})));
    assert_tool_success(&resp);
}

/// Unknown method → JSON-RPC error code.
#[test]
fn mcp_unknown_method_returns_method_not_found() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(json!({
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
}

/// notifications/initialized (no id) produces no JSON-RPC response body.
#[test]
fn mcp_notification_no_response() {
    let mut client = shared_session().lock().unwrap();

    client.notify(notification("notifications/initialized"));

    let resp = client.call(tool_call_request(10, "list_agents", json!({})));
    assert_tool_success(&resp);
}

/// 10 sequential list_agents calls — WASM state doesn't corrupt across calls.
#[test]
fn mcp_multiple_sequential_calls() {
    let mut client = shared_session().lock().unwrap();

    for i in 0..10u64 {
        let resp = client.call(tool_call_request(100 + i, "list_agents", json!({})));
        let inner = assert_tool_success(&resp);
        assert!(
            inner.is_object() || inner.is_array(),
            "Call {} failed: expected structured response, got: {}",
            i,
            inner
        );
    }
}

/// Multiple different operations interleaved — validates server handles mixed traffic.
#[test]
fn mcp_interleaved_operations() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(20, "list_agents", json!({})));
    assert_tool_success(&resp);

    let resp = client.call(tool_call_request(21, "no_such_tool", json!({})));
    assert_tool_error(&resp);

    let resp = client.call(tool_call_request(22, "list_agents", json!({})));
    assert_tool_success(&resp);

    let resp = client.call(list_tools_request(23));
    assert!(
        resp.get("error").is_none() || resp["error"].is_null(),
        "tools/list failed after error recovery: {}",
        resp
    );
}

// ============================================================================
// Argument validation — WASM-level JSON parsing (shared session)
// ============================================================================

/// spawn_agents missing required `issues` field → tool-level parse error.
#[test]
fn mcp_spawn_agents_missing_required_field() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        30,
        "spawn_agents",
        json!({"owner": "test", "repo": "test"}),
    ));
    assert_tool_error(&resp);

    let resp = client.call(tool_call_request(31, "list_agents", json!({})));
    assert_tool_success(&resp);
}

/// spawn_agents with `issues` as string instead of array → parse error.
#[test]
fn mcp_spawn_agents_wrong_type_issues() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        32,
        "spawn_agents",
        json!({"issues": "123", "owner": "test", "repo": "test"}),
    ));
    assert_tool_error(&resp);
}

/// spawn_agents with empty issues array — valid parse, empty batch.
#[test]
fn mcp_spawn_agents_empty_issues() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        33,
        "spawn_agents",
        json!({"issues": [], "owner": "test", "repo": "test"}),
    ));

    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);
}

/// cleanup_agents missing required `issues` field.
#[test]
fn mcp_cleanup_agents_missing_issues() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(34, "cleanup_agents", json!({})));
    assert_tool_error(&resp);
}

/// cleanup_agents with nonexistent issue IDs — exercises batch error response path.
#[test]
fn mcp_cleanup_agents_nonexistent_issues() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        35,
        "cleanup_agents",
        json!({"issues": ["99999", "88888"]}),
    ));

    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);

    let result = &resp["result"];
    let content = &result["content"];
    assert!(content.is_array(), "Expected content array, got: {}", resp);
}

/// cleanup_agents with force=true — optional boolean arg exercises proto encoding.
#[test]
fn mcp_cleanup_agents_with_force() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        36,
        "cleanup_agents",
        json!({"issues": ["12345"], "force": true}),
    ));

    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);
}

/// spawn_agents with all optional fields — exercises full proto message encoding.
#[test]
fn mcp_spawn_agents_all_optional_fields() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        37,
        "spawn_agents",
        json!({
            "issues": ["42"],
            "owner": "testowner",
            "repo": "testrepo",
            "agent_type": "claude"
        }),
    ));

    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);
}

/// Extra unknown fields in tool arguments are silently ignored (Aeson default).
#[test]
fn mcp_tool_call_extra_args_ignored() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        38,
        "list_agents",
        json!({"unknown_field": "should_be_ignored", "another": 42}),
    ));

    assert_tool_success(&resp);
}

// ============================================================================
// JSON-RPC protocol edge cases (shared session)
// ============================================================================

/// Null request ID handling after initialize.
#[test]
fn mcp_null_request_id_is_notification() {
    let client = shared_session().lock().unwrap();

    let resp = client.send_raw(
        &serde_json::to_string(&json!({
            "jsonrpc": "2.0",
            "id": null,
            "method": "tools/list",
            "params": {}
        }))
        .unwrap(),
    );

    assert!(
        resp.status().is_success() || resp.status().is_client_error(),
        "Unexpected status for null-id request: {}",
        resp.status()
    );
}

/// tools/call with missing name field.
#[test]
fn mcp_tool_call_missing_name() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(json!({
        "jsonrpc": "2.0",
        "id": 41,
        "method": "tools/call",
        "params": {
            "arguments": {}
        }
    }));

    assert_tool_error(&resp);
}

/// tools/call with missing arguments field — defaults to empty object.
#[test]
fn mcp_tool_call_missing_arguments() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(json!({
        "jsonrpc": "2.0",
        "id": 42,
        "method": "tools/call",
        "params": {
            "name": "list_agents"
        }
    }));

    assert_tool_success(&resp);
}

// ============================================================================
// Error recovery sequences (shared session)
// ============================================================================

/// Server recovers after malformed JSON — HTTP requests are independent.
#[test]
fn mcp_recovery_after_malformed_json() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.send_raw("{{{invalid json garbage!!!}}}");
    assert!(
        !resp.status().is_success() || {
            let body: Value = resp.json().unwrap_or(json!({}));
            body.get("error").is_some()
        },
        "Expected error for malformed JSON"
    );

    let resp = client.call(tool_call_request(50, "list_agents", json!({})));
    assert_tool_success(&resp);
}

/// Multiple malformed requests — server stays healthy.
#[test]
fn mcp_recovery_after_multiple_parse_errors() {
    let mut client = shared_session().lock().unwrap();

    for i in 0..5 {
        let _resp = client.send_raw(&format!("garbage line {}", i));
    }

    let resp = client.call(tool_call_request(51, "list_agents", json!({})));
    assert_tool_success(&resp);
}

/// Alternating errors and successes — stress tests error recovery.
#[test]
fn mcp_alternating_errors_and_successes() {
    let mut client = shared_session().lock().unwrap();

    for i in 0..5u64 {
        let resp = client.call(tool_call_request(60 + i * 2, "list_agents", json!({})));
        assert_tool_success(&resp);

        let resp = client.call(tool_call_request(61 + i * 2, "nonexistent", json!({})));
        assert_tool_error(&resp);
    }

    let resp = client.call(tool_call_request(70, "list_agents", json!({})));
    assert_tool_success(&resp);
}

/// Mix of method errors and tool errors in sequence after initialize.
#[test]
fn mcp_mixed_error_types() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(json!({
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

    let resp = client.call(tool_call_request(82, "fake_tool", json!({})));
    assert_tool_error(&resp);

    let resp = client.call(tool_call_request(83, "list_agents", json!({})));
    assert_tool_success(&resp);
}

// ============================================================================
// Notifications (shared session)
// ============================================================================

/// Unknown notifications produce no response body and don't crash.
#[test]
fn mcp_unknown_notification_silent() {
    let mut client = shared_session().lock().unwrap();

    client.notify(notification("custom/unknown_notification"));

    let resp = client.call(tool_call_request(90, "list_agents", json!({})));
    assert_tool_success(&resp);
}

/// Notifications interleaved with requests don't disrupt responses.
#[test]
fn mcp_notifications_interleaved_with_requests() {
    let mut client = shared_session().lock().unwrap();

    client.notify(notification("notifications/initialized"));
    let resp = client.call(tool_call_request(91, "list_agents", json!({})));
    assert_tool_success(&resp);

    client.notify(notification("notifications/initialized"));
    client.notify(notification("notifications/initialized"));
    let resp = client.call(tool_call_request(92, "list_agents", json!({})));
    assert_tool_success(&resp);
}

// ============================================================================
// Payload size variations (shared session)
// ============================================================================

/// cleanup_agents with many issue IDs — larger proto repeated field payload.
#[test]
fn mcp_cleanup_agents_many_issues() {
    let mut client = shared_session().lock().unwrap();

    let issues: Vec<String> = (1..=50).map(|i| format!("{}", i)).collect();
    let resp = client.call(tool_call_request(
        210,
        "cleanup_agents",
        json!({"issues": issues}),
    ));

    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(
        !has_jsonrpc_error,
        "Unexpected JSON-RPC error for many-issue cleanup: {}",
        resp
    );
}

// ============================================================================
// Health check (shared session)
// ============================================================================

/// Server responds to health check after sustained traffic.
#[test]
fn mcp_health_after_traffic() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(220, "list_agents", json!({})));
    assert_tool_success(&resp);

    let health_url = format!("http://127.0.0.1:{}/health", client.port);
    let resp = client
        .client
        .get(&health_url)
        .send()
        .expect("Health check failed");
    assert!(
        resp.status().is_success(),
        "Health check should succeed after traffic"
    );
}
