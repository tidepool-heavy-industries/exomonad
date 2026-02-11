//! MCP HTTP integration tests.
//!
//! Black-box tests that drive `exomonad serve` via JSON-RPC over HTTP
//! (StreamableHttp transport). Catches wire format mismatches by exercising
//! the full pipeline:
//!   HTTP POST → axum → rmcp StreamableHttp → WASM (Haskell) → protobuf effects
//!     → Rust handlers → protobuf response → WASM decode → JSON-RPC response
//!
//! The unified WASM provides: spawn_subtree, spawn_worker, popup,
//! get_agent_messages, answer_question, get_agent_messages.
//!
//! Tests expect a running server on the port specified by MCP_TEST_PORT.
//! Most tests share a single MCP session to avoid exhausting the server's
//! session pool. Tests that exercise protocol edge cases (initialization,
//! ID formats) create their own lightweight sessions.
//!
//! **Run with:** `just test-mcp`

use reqwest::blocking::Client;
use reqwest::header::{HeaderMap, HeaderValue, ACCEPT, CONTENT_TYPE};
use serde_json::{json, Value};
use std::sync::{Mutex, OnceLock};
use std::time::Duration;

// ============================================================================
// Server port — provided by wrapper script via env var
// ============================================================================

fn server_port() -> u16 {
    std::env::var("MCP_TEST_PORT")
        .expect("MCP_TEST_PORT not set — run via: just test-mcp")
        .parse()
        .expect("MCP_TEST_PORT must be a valid port number")
}

// ============================================================================
// McpClient — lightweight HTTP session
// ============================================================================

struct McpClient {
    port: u16,
    client: Client,
    session_id: Option<String>,
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
        let mut client = McpClient::new(server_port());

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

/// Create a new uninitialized client pointing at the server.
/// Use sparingly — each initialize consumes a session slot.
fn new_raw_client() -> McpClient {
    McpClient::new(server_port())
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
        &serde_json::to_string(&tool_call_request(240, "get_agent_messages", json!({}))).unwrap(),
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

    for expected in &["spawn_subtree", "spawn_worker", "get_agent_messages"] {
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

/// spawn_subtree inputSchema declares required fields.
#[test]
fn mcp_spawn_subtree_schema_has_required_fields() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(list_tools_request(202));
    let tools = resp["result"]["tools"].as_array().unwrap();

    let spawn = tools
        .iter()
        .find(|t| t["name"] == "spawn_subtree")
        .expect("spawn_subtree should be in tool list");

    let schema = &spawn["inputSchema"];
    assert_eq!(
        schema["type"], "object",
        "inputSchema type should be object"
    );

    let required = schema["required"]
        .as_array()
        .expect("spawn_subtree should have required fields");
    let required_names: Vec<&str> = required.iter().filter_map(|v| v.as_str()).collect();

    for field in &["task", "branch_name"] {
        assert!(
            required_names.contains(field),
            "spawn_subtree should require '{}'. Found: {:?}",
            field,
            required_names
        );
    }
}

/// Verify all tool schemas match expected structure.
#[test]
fn mcp_tool_schemas_valid() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(list_tools_request(204));
    let tools = resp["result"]["tools"].as_array().unwrap();

    let tool_map: std::collections::HashMap<&str, &Value> = tools
        .iter()
        .filter_map(|t| t["name"].as_str().map(|n| (n, t)))
        .collect();

    // 1. spawn_subtree
    let subtree = tool_map
        .get("spawn_subtree")
        .expect("spawn_subtree missing");
    let props = &subtree["inputSchema"]["properties"];
    assert!(props.get("task").is_some());
    assert!(props.get("branch_name").is_some());
    assert!(props.get("context").is_some());
    assert!(props.get("agent_type").is_some());

    // 2. spawn_worker
    let worker = tool_map.get("spawn_worker").expect("spawn_worker missing");
    let props = &worker["inputSchema"]["properties"];
    assert!(props.get("name").is_some());
    assert!(props.get("prompt").is_some());
    assert!(props.get("agent_type").is_none()); // NO agent_type for worker

    // 3. file_pr
    let file_pr = tool_map.get("file_pr").expect("file_pr missing");
    let props = &file_pr["inputSchema"]["properties"];
    assert!(props.get("title").is_some());
    assert!(props.get("body").is_some());
    assert!(props.get("base_branch").is_some());

    // 4. get_agent_messages
    let msg = tool_map
        .get("get_agent_messages")
        .expect("get_agent_messages missing");
    assert!(msg["inputSchema"]["properties"]
        .get("timeout_secs")
        .is_some());


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

/// Full proto roundtrip via agent.cleanup_merged effect.
#[test]
fn mcp_tool_call_get_agent_messages() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(5, "get_agent_messages", json!({})));

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
    let resp = client.call(tool_call_request(50, "get_agent_messages", json!({})));
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

    let resp = client.call(tool_call_request(10, "get_agent_messages", json!({})));
    assert_tool_success(&resp);
}

/// 10 sequential get_agent_messages calls — WASM state doesn't corrupt across calls.
#[test]
fn mcp_multiple_sequential_calls() {
    let mut client = shared_session().lock().unwrap();

    for i in 0..10u64 {
        let resp = client.call(tool_call_request(100 + i, "get_agent_messages", json!({})));
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

    let resp = client.call(tool_call_request(20, "get_agent_messages", json!({})));
    assert_tool_success(&resp);

    let resp = client.call(tool_call_request(21, "no_such_tool", json!({})));
    assert_tool_error(&resp);

    let resp = client.call(tool_call_request(22, "get_agent_messages", json!({})));
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

/// spawn_subtree missing required `task` field → tool-level parse error.
#[test]
fn mcp_spawn_subtree_missing_required_field() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        30,
        "spawn_subtree",
        json!({"branch_name": "test-branch"}),
    ));
    assert_tool_error(&resp);

    // Server recovers
    let resp = client.call(tool_call_request(31, "get_agent_messages", json!({})));
    assert_tool_success(&resp);
}

/// spawn_subtree with `task` as number instead of string → parse error.
#[test]
fn mcp_spawn_subtree_wrong_type_task() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        32,
        "spawn_subtree",
        json!({"task": 123, "branch_name": "test-branch"}),
    ));
    assert_tool_error(&resp);
}

/// spawn_subtree with all required fields — valid parse, exercises proto encoding.
#[test]
fn mcp_spawn_subtree_valid_args() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        33,
        "spawn_subtree",
        json!({"task": "test task", "branch_name": "test-branch"}),
    ));

    let has_jsonrpc_error = resp.get("error").is_some() && !resp["error"].is_null();
    assert!(!has_jsonrpc_error, "Unexpected JSON-RPC error: {}", resp);
}

/// spawn_subtree with all optional fields — exercises full proto message encoding.
#[test]
fn mcp_spawn_subtree_all_optional_fields() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(
        37,
        "spawn_subtree",
        json!({
            "task": "test task",
            "branch_name": "test-branch",
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
        "get_agent_messages",
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
            "name": "get_agent_messages"
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

    let resp = client.call(tool_call_request(50, "get_agent_messages", json!({})));
    assert_tool_success(&resp);
}

/// Multiple malformed requests — server stays healthy.
#[test]
fn mcp_recovery_after_multiple_parse_errors() {
    let mut client = shared_session().lock().unwrap();

    for i in 0..5 {
        let _resp = client.send_raw(&format!("garbage line {}", i));
    }

    let resp = client.call(tool_call_request(51, "get_agent_messages", json!({})));
    assert_tool_success(&resp);
}

/// Alternating errors and successes — stress tests error recovery.
#[test]
fn mcp_alternating_errors_and_successes() {
    let mut client = shared_session().lock().unwrap();

    for i in 0..5u64 {
        let resp = client.call(tool_call_request(
            60 + i * 2,
            "get_agent_messages",
            json!({}),
        ));
        assert_tool_success(&resp);

        let resp = client.call(tool_call_request(61 + i * 2, "nonexistent", json!({})));
        assert_tool_error(&resp);
    }

    let resp = client.call(tool_call_request(70, "get_agent_messages", json!({})));
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

    let resp = client.call(tool_call_request(83, "get_agent_messages", json!({})));
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

    let resp = client.call(tool_call_request(90, "get_agent_messages", json!({})));
    assert_tool_success(&resp);
}

/// Notifications interleaved with requests don't disrupt responses.
#[test]
fn mcp_notifications_interleaved_with_requests() {
    let mut client = shared_session().lock().unwrap();

    client.notify(notification("notifications/initialized"));
    let resp = client.call(tool_call_request(91, "get_agent_messages", json!({})));
    assert_tool_success(&resp);

    client.notify(notification("notifications/initialized"));
    client.notify(notification("notifications/initialized"));
    let resp = client.call(tool_call_request(92, "get_agent_messages", json!({})));
    assert_tool_success(&resp);
}

// ============================================================================
// Health check (shared session)
// ============================================================================

/// Server responds to health check after sustained traffic.
#[test]
fn mcp_health_after_traffic() {
    let mut client = shared_session().lock().unwrap();

    let resp = client.call(tool_call_request(220, "get_agent_messages", json!({})));
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
