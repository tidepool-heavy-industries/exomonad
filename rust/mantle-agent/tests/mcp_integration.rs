//! Integration tests for MCP stdio server in mantle-agent.
//!
//! These tests spawn the `mantle-agent mcp` subprocess and verify
//! correct JSON-RPC 2.0 protocol behavior over stdio.
//!
//! The tests use a **Mock Control Server** (background thread) to simulate
//! the Haskell backend. This allows testing the agent's interaction with
//! both the MCP client (stdio) and the control server (Unix socket).
//!
//! ## Test Coverage
//!
//! - `test_mcp_initialize` - Protocol handshake
//! - `test_mcp_initialized_notification` - Initialized notification handling
//! - `test_mcp_tools_list_empty` - Empty tool list from mock server
//! - `test_mcp_tools_list_with_tools` - Tool definitions from mock server
//! - `test_mcp_tools_call_success` - Successful tool call forwarded to mock
//! - `test_mcp_tools_call_unknown_tool` - Error for unknown tool (handled by agent)
//! - `test_mcp_unknown_method` - Error for unknown JSON-RPC method
//! - `test_mcp_multiple_tool_calls` - Multiple tool calls in sequence

use mantle_shared::protocol::{ControlMessage, ControlResponse, ToolDefinition};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::UnixListener;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

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
// Mock Control Server
// ============================================================================ 

struct MockControlServer {
    tools: Arc<Mutex<Vec<ToolDefinition>>>,
    // Keep temp_dir alive as long as the server is running
    _temp_dir: Arc<TempDir>,
    socket_path: PathBuf,
}

impl MockControlServer {
    fn new() -> Self {
        let temp_dir = Arc::new(TempDir::new().expect("Failed to create temp dir"));
        let socket_path = temp_dir.path().join("control.sock");
        let tools = Arc::new(Mutex::new(Vec::new()));

        let server = Self {
            tools: tools.clone(),
            _temp_dir: temp_dir,
            socket_path: socket_path.clone(),
        };

        // Spawn background listener
        let tools_clone = tools.clone();
        let socket_path_clone = socket_path.clone();
        
        thread::spawn(move || {
            let listener = UnixListener::bind(&socket_path_clone)
                .expect("Failed to bind mock control socket");

            for stream in listener.incoming() {
                match stream {
                    Ok(stream) => {
                        let tools = tools_clone.clone();
                        thread::spawn(move || handle_client(stream, tools));
                    }
                    Err(e) => eprintln!("Accept failed: {}", e),
                }
            }
        });

        server
    }

    fn set_tools(&self, new_tools: Vec<ToolDefinition>) {
        let mut tools = self.tools.lock().unwrap();
        *tools = new_tools;
    }
}

fn handle_client(stream: std::os::unix::net::UnixStream, tools: Arc<Mutex<Vec<ToolDefinition>>>) {
    let mut reader = BufReader::new(&stream);
    let mut writer = &stream;
    let mut line = String::new();

    loop {
        line.clear();
        match reader.read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {
                let msg: ControlMessage = match serde_json::from_str(&line) {
                    Ok(m) => m,
                    Err(e) => {
                        eprintln!("Failed to parse control message: {}", e);
                        continue;
                    }
                };

                let response = match msg {
                    ControlMessage::ToolsListRequest => {
                        let tools = tools.lock().unwrap();
                        ControlResponse::ToolsListResponse {
                            tools: tools.clone(),
                        }
                    }
                    ControlMessage::McpToolCall { tool_name, .. } => {
                        // Mock successful tool call
                        ControlResponse::McpToolResponse {
                            id: "mock-id".to_string(),
                            result: Some(json!({
                                "content": [{
                                    "type": "text",
                                    "text": format!("Tool executed: {}", tool_name)
                                }]
                            })),
                            error: None,
                        }
                    }
                    _ => ControlResponse::Pong,
                };

                let json = serde_json::to_string(&response).unwrap();
                writeln!(writer, "{}", json).unwrap();
            }
            Err(e) => {
                eprintln!("Read error: {}", e);
                break;
            }
        }
    }
}

// ============================================================================ 
// Test Server Wrapper
// ============================================================================ 

/// Wrapper around mantle-agent mcp subprocess + mock control server.
struct McpTestServer {
    child: Child,
    stdin: Option<std::process::ChildStdin>,
    stdout: BufReader<std::process::ChildStdout>,
    // Keep mock server alive
    _mock_server: MockControlServer,
}

impl McpTestServer {
    fn spawn(tools_json: Option<&str>) -> Self {
        let mock_server = MockControlServer::new();

        if let Some(json_str) = tools_json {
            // Local struct matching standard MCP JSON format
            #[derive(Deserialize)]
            struct TestToolDefinition {
                name: String,
                description: String,
                #[serde(rename = "inputSchema")]
                input_schema: Value,
            }

            // Parse JSON into local struct
            let tools: Vec<TestToolDefinition> = serde_json::from_str(json_str)
                .expect("Failed to parse test tools JSON");
            
            // Convert to protocol::ToolDefinition (which uses tdName etc.)
            let proto_tools: Vec<ToolDefinition> = tools.into_iter().map(|t| ToolDefinition {
                name: t.name,
                description: t.description,
                input_schema: t.input_schema,
            }).collect();

            mock_server.set_tools(proto_tools);
        }

        let exe = find_mantle_agent();

        let mut cmd = Command::new(&exe);
        cmd.arg("mcp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .env("TIDEPOOL_CONTROL_SOCKET", &mock_server.socket_path);

        let mut child = cmd.spawn().expect("Failed to spawn mantle-agent mcp");

        let stdin = child.stdin.take().expect("Failed to get stdin");
        let stdout = child.stdout.take().expect("Failed to get stdout");

        // Give server a moment to start and connect to socket
        std::thread::sleep(Duration::from_millis(100));

        McpTestServer {
            child,
            stdin: Some(stdin),
            stdout: BufReader::new(stdout),
            _mock_server: mock_server,
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
    assert!(tools.is_empty(), "Expected empty tools list when no tools provided");
}

#[test]
fn test_mcp_tools_list_with_tools() {
    let tools_json = r#"[
        {"name":"test_tool_a","description":"Test Tool A","inputSchema":{"type":"object","properties":{"notes":{"type":"string"}}}},
        {"name":"test_tool_b","description":"Test Tool B","inputSchema":{"type":"object","properties":{"reason":{"type":"string"}}}}
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
    assert_eq!(tools.len(), 2, "Expected 2 tools");

    // Verify tool names
    let tool_names: Vec<&str> = tools
        .iter()
        .map(|t| t["name"].as_str().unwrap())
        .collect();
    assert!(tool_names.contains(&"test_tool_a"));
    assert!(tool_names.contains(&"test_tool_b"));

    // Verify input schemas are preserved
    let tool_a = tools.iter().find(|t| t["name"] == "test_tool_a").unwrap();
    assert!(tool_a["inputSchema"]["properties"]["notes"].is_object());
}

#[test]
fn test_mcp_tools_call_success() {
    let tools_json = r#"[
        {"name":"test_tool_a","description":"Test Tool A","inputSchema":{"type":"object"}}
    ]"#;

    let mut server = McpTestServer::spawn(Some(tools_json));

    // Initialize
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Call the tool
    let request = JsonRpcRequest::new("call-1", "tools/call").with_params(json!({
        "name": "test_tool_a",
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
        text_content["text"].as_str().unwrap().contains("test_tool_a"),
        "Expected response to mention 'test_tool_a'"
    );
}

#[test]
fn test_mcp_tools_call_unknown_tool() {
    let tools_json = r#"[
        {"name":"test_tool_a","description":"Test Tool A","inputSchema":{"type":"object"}}
    ]"#;

    let mut server = McpTestServer::spawn(Some(tools_json));

    // Initialize
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Try to call a non-existent tool
    let request = JsonRpcRequest::new("call-bad", "tools/call").with_params(json!({
        "name": "nonexistent_tool",
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
        {"name":"test_tool_a","description":"Test Tool A","inputSchema":{"type":"object"}},
        {"name":"test_tool_b","description":"Test Tool B","inputSchema":{"type":"object"}}
    ]"#;

    let mut server = McpTestServer::spawn(Some(tools_json));

    // Initialize
    let init_req = JsonRpcRequest::new("init", "initialize").with_params(json!({}));
    let _ = server.send(&init_req);

    // Call tool A
    let req_a = JsonRpcRequest::new("call-1", "tools/call").with_params(json!({
        "name": "test_tool_a",
        "arguments": {"notes": "First call"}
    }));
    let resp_a = server.send(&req_a);
    assert!(resp_a.error.is_none(), "Tool A call should succeed");

    // Call tool B
    let req_b = JsonRpcRequest::new("call-2", "tools/call").with_params(json!({
        "name": "test_tool_b",
        "arguments": {"reason": "Second call"}
    }));
    let resp_b = server.send(&req_b);
    assert!(resp_b.error.is_none(), "Tool B call should succeed");

    // Verify different request IDs get different responses
    assert_eq!(resp_a.id, json!("call-1"));
    assert_eq!(resp_b.id, json!("call-2"));
}