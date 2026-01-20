//! Integration tests for MCP stdio server in mantle-agent.
//!
//! These tests spawn the `mantle-agent mcp` subprocess and verify
//! correct JSON-RPC 2.0 protocol behavior over stdio.
//!
//! ## Test Coverage
//!
//! - `test_mcp_initialize` - Protocol handshake
//! - `test_mcp_initialized_notification` - Initialized notification handling
//! - `test_mcp_tools_list_empty` - Empty tool list when no env var
//! - `test_mcp_tools_list_with_decision_tools` - Tool definitions from env var
//! - `test_mcp_tools_call_success` - Successful tool call
//! - `test_mcp_tools_call_unknown_tool` - Error for unknown tool
//! - `test_mcp_unknown_method` - Error for unknown JSON-RPC method
//! - `test_mcp_multiple_tool_calls` - Multiple tool calls in sequence
//!
//! ## Docker Integration Testing
//!
//! For full Docker integration tests (requires Docker running):
//!
//! ```bash
//! # Build the container
//! cd rust && docker build -t mantle-agent:test -f mantle/Dockerfile .
//!
//! # Test MCP server in container
//! docker run --rm -e MANTLE_DECISION_TOOLS='[{"name":"test","description":"Test","inputSchema":{}}]' \
//!   mantle-agent:test mantle-agent mcp --help
//!
//! # Verify entrypoint registers MCP server with Claude Code
//! docker run --rm -e MANTLE_DECISION_TOOLS='[...]' \
//!   mantle-agent:test bash -c "claude mcp list"
//! ```

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::Duration;

// ============================================================================
// Test Helper Types
// ============================================================================

/// JSON-RPC 2.0 request for sending to MCP server.
#[derive(Debug, Serialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    id: Value,
    method: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    params: Option<Value>,
}

impl JsonRpcRequest {
    fn new(id: impl Into<Value>, method: &str) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: id.into(),
            method: method.to_string(),
            params: None,
        }
    }

    fn with_params(mut self, params: Value) -> Self {
        self.params = Some(params);
        self
    }
}

/// JSON-RPC 2.0 response received from MCP server.
#[derive(Debug, Deserialize)]
struct JsonRpcResponse {
    #[allow(dead_code)]
    jsonrpc: String,
    id: Value,
    #[serde(default)]
    result: Option<Value>,
    #[serde(default)]
    error: Option<JsonRpcError>,
}

#[derive(Debug, Deserialize)]
struct JsonRpcError {
    code: i32,
    message: String,
    #[serde(default)]
    #[allow(dead_code)]
    data: Option<Value>,
}

// ============================================================================
// Test Server Wrapper
// ============================================================================

/// Wrapper around mantle-agent mcp subprocess.
struct McpTestServer {
    child: Child,
    stdin: Option<std::process::ChildStdin>,
    stdout: BufReader<std::process::ChildStdout>,
}

impl McpTestServer {
    /// Spawn MCP server with optional decision tools.
    fn spawn(decision_tools: Option<&str>) -> Self {
        let exe = find_mantle_agent();

        let mut cmd = Command::new(&exe);
        cmd.arg("mcp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        // Set decision tools env var if provided
        if let Some(tools) = decision_tools {
            cmd.env("MANTLE_DECISION_TOOLS", tools);
        }

        let mut child = cmd.spawn().expect("Failed to spawn mantle-agent mcp");

        let stdin = child.stdin.take().expect("Failed to get stdin");
        let stdout = child.stdout.take().expect("Failed to get stdout");

        // Give server a moment to start
        std::thread::sleep(Duration::from_millis(50));

        McpTestServer {
            child,
            stdin: Some(stdin),
            stdout: BufReader::new(stdout),
        }
    }

    /// Send a JSON-RPC request and receive the response.
    fn send(&mut self, request: &JsonRpcRequest) -> JsonRpcResponse {
        let request_json = serde_json::to_string(request).expect("Failed to serialize request");
        let stdin = self.stdin.as_mut().expect("stdin already closed");
        writeln!(stdin, "{}", request_json).expect("Failed to write to stdin");
        stdin.flush().expect("Failed to flush stdin");

        let mut response_line = String::new();
        self.stdout
            .read_line(&mut response_line)
            .expect("Failed to read response");

        serde_json::from_str(&response_line).expect("Failed to parse response JSON")
    }
}

impl Drop for McpTestServer {
    fn drop(&mut self) {
        // Close stdin to signal server to exit
        self.stdin.take();
        // Wait for clean exit
        let _ = self.child.wait();
    }
}

/// Find the built mantle-agent executable.
fn find_mantle_agent() -> PathBuf {
    // CARGO_MANIFEST_DIR = .../rust/mantle-agent
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let rust_dir = manifest_dir.parent().unwrap();

    // Check debug build first
    let debug_exe = rust_dir.join("target/debug/mantle-agent");
    if debug_exe.exists() {
        return debug_exe;
    }

    // Check release build
    let release_exe = rust_dir.join("target/release/mantle-agent");
    if release_exe.exists() {
        return release_exe;
    }

    panic!(
        "mantle-agent not found. Run `cargo build -p mantle-agent` first.\n\
         Searched in: {:?}",
        rust_dir.join("target")
    );
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_mcp_initialize() {
    let mut server = McpTestServer::spawn(None);

    let request = JsonRpcRequest::new("init-1", "initialize").with_params(json!({
        "protocolVersion": "2024-11-05",
        "capabilities": {},
        "clientInfo": {
            "name": "test-client",
            "version": "1.0.0"
        }
    }));

    let response = server.send(&request);

    assert!(response.error.is_none(), "Expected no error");
    assert!(response.result.is_some(), "Expected result");

    let result = response.result.unwrap();
    assert_eq!(result["protocolVersion"], "2024-11-05");
    assert!(result["capabilities"]["tools"].is_object());
    assert_eq!(result["serverInfo"]["name"], "mantle-agent");
}

#[test]
fn test_mcp_tools_list_empty() {
    let mut server = McpTestServer::spawn(None);

    // Initialize first
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Request tools list
    let request = JsonRpcRequest::new("list-1", "tools/list").with_params(json!({}));
    let response = server.send(&request);

    assert!(response.error.is_none(), "Expected no error");
    assert!(response.result.is_some(), "Expected result");

    let result = response.result.unwrap();
    let tools = result["tools"].as_array().expect("Expected tools array");
    assert!(tools.is_empty(), "Expected empty tools list when no env var");
}

#[test]
fn test_mcp_tools_list_with_decision_tools() {
    let tools_json = r#"[
        {"name":"decision::approve","description":"Approve the request","inputSchema":{"type":"object","properties":{"notes":{"type":"string"}}}},
        {"name":"decision::reject","description":"Reject the request","inputSchema":{"type":"object","properties":{"reason":{"type":"string"}}}}
    ]"#;

    let mut server = McpTestServer::spawn(Some(tools_json));

    // Initialize first
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Request tools list
    let request = JsonRpcRequest::new("list-1", "tools/list").with_params(json!({}));
    let response = server.send(&request);

    assert!(response.error.is_none(), "Expected no error");
    assert!(response.result.is_some(), "Expected result");

    let result = response.result.unwrap();
    let tools = result["tools"].as_array().expect("Expected tools array");
    assert_eq!(tools.len(), 2, "Expected 2 decision tools");

    // Verify tool names
    let tool_names: Vec<&str> = tools
        .iter()
        .map(|t| t["name"].as_str().unwrap())
        .collect();
    assert!(tool_names.contains(&"decision::approve"));
    assert!(tool_names.contains(&"decision::reject"));

    // Verify input schemas are preserved
    let approve_tool = tools.iter().find(|t| t["name"] == "decision::approve").unwrap();
    assert!(approve_tool["inputSchema"]["properties"]["notes"].is_object());
}

#[test]
fn test_mcp_tools_call_success() {
    let tools_json = r#"[
        {"name":"decision::approve","description":"Approve","inputSchema":{"type":"object"}}
    ]"#;

    let mut server = McpTestServer::spawn(Some(tools_json));

    // Initialize
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Call the decision tool
    let request = JsonRpcRequest::new("call-1", "tools/call").with_params(json!({
        "name": "decision::approve",
        "arguments": {"notes": "Looks good!"}
    }));

    let response = server.send(&request);

    assert!(response.error.is_none(), "Expected no error");
    assert!(response.result.is_some(), "Expected result");

    let result = response.result.unwrap();
    let content = result["content"].as_array().expect("Expected content array");
    assert!(!content.is_empty(), "Expected at least one content item");

    let text_content = &content[0];
    assert_eq!(text_content["type"], "text");
    assert!(
        text_content["text"].as_str().unwrap().contains("approve"),
        "Expected response to mention 'approve'"
    );
}

#[test]
fn test_mcp_tools_call_unknown_tool() {
    let tools_json = r#"[
        {"name":"decision::approve","description":"Approve","inputSchema":{"type":"object"}}
    ]"#;

    let mut server = McpTestServer::spawn(Some(tools_json));

    // Initialize
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Try to call a non-existent tool
    let request = JsonRpcRequest::new("call-bad", "tools/call").with_params(json!({
        "name": "decision::nonexistent",
        "arguments": {}
    }));

    let response = server.send(&request);

    assert!(response.error.is_some(), "Expected error for unknown tool");
    let error = response.error.unwrap();
    assert_eq!(error.code, -32602, "Expected invalid params error code");
    assert!(
        error.message.contains("Unknown tool"),
        "Error should mention unknown tool"
    );
}

#[test]
fn test_mcp_unknown_method() {
    let mut server = McpTestServer::spawn(None);

    let request = JsonRpcRequest::new("bad-1", "unknown/method").with_params(json!({}));
    let response = server.send(&request);

    assert!(response.error.is_some(), "Expected error for unknown method");
    let error = response.error.unwrap();
    assert_eq!(error.code, -32601, "Expected method not found error code");
}

#[test]
fn test_mcp_initialized_notification() {
    let mut server = McpTestServer::spawn(None);

    // Send initialize
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Send initialized notification (MCP protocol requirement)
    let request = JsonRpcRequest::new("notify", "initialized").with_params(json!({}));
    let response = server.send(&request);

    // Should acknowledge without error
    assert!(response.error.is_none(), "Expected no error for initialized");
}

#[test]
fn test_mcp_multiple_tool_calls() {
    let tools_json = r#"[
        {"name":"decision::approve","description":"Approve","inputSchema":{"type":"object"}},
        {"name":"decision::reject","description":"Reject","inputSchema":{"type":"object"}}
    ]"#;

    let mut server = McpTestServer::spawn(Some(tools_json));

    // Initialize
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Call approve
    let approve_req = JsonRpcRequest::new("call-1", "tools/call").with_params(json!({
        "name": "decision::approve",
        "arguments": {"notes": "First approval"}
    }));
    let approve_resp = server.send(&approve_req);
    assert!(approve_resp.error.is_none(), "Approve should succeed");

    // Call reject
    let reject_req = JsonRpcRequest::new("call-2", "tools/call").with_params(json!({
        "name": "decision::reject",
        "arguments": {"reason": "Missing documentation"}
    }));
    let reject_resp = server.send(&reject_req);
    assert!(reject_resp.error.is_none(), "Reject should succeed");

    // Verify different request IDs get different responses
    assert_eq!(approve_resp.id, json!("call-1"));
    assert_eq!(reject_resp.id, json!("call-2"));
}

// Note: Malformed JSON test removed because the server's error response behavior
// depends on buffering and timing. The unit tests in mcp.rs verify error handling.
