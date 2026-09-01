//! **Outbound.** Serves the node's role tools (from the injected roster) over a hand-written
//! minimal MCP/JSON-RPC stdio server (`initialize` / `tools/list` / `tools/call`, over raw
//! `serde_json::Value`) and routes communication through the node's ingestion system. This is
//! the server that Claude Code or other agents connect to for tool execution. Known limits: no
//! capability negotiation beyond the three methods above, and malformed JSON is logged and
//! dropped rather than answered with a JSON-RPC parse-error reply.
//!
//! It exposes the tools from the injected `RoleRegistry` for the node's specific role.
//! Tools like `send_message` and `notify_parent` are routed through the `Bus::deliver`
//! mechanism, which appends `IngestionEntry` objects to target inboxes (e.g., the
//! parent's ingestion inbox), maintaining runtime-agnostic communication where the
//! policy layer doesn't need to know about Teams or tmux.

use std::path::Path;
use std::sync::Arc;

use exo_framework::{ErasedTool, Exomonad};
use exo_runtime::Runtime;
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tracing::warn;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Newest MCP revision implemented by this deliberately small stdio server.
///
/// MCP clients may propose an older revision during `initialize`; the methods used here have the
/// same wire shape in 2024-11-05, so retain that revision for Claude compatibility as well.
const MCP_PROTOCOL_VERSION: &str = "2025-06-18";
const LEGACY_MCP_PROTOCOL_VERSION: &str = "2024-11-05";

/// Serve the policy toolset over the hand-written MCP/JSON-RPC stdio server until the stream closes.
pub async fn serve<D: Exomonad<Caps = Runtime>>(ctx: Arc<NodeContext<D>>) -> NodeResult<()> {
    let tools = D::role_def(ctx.kind).tools;
    let stdin = tokio::io::stdin();
    let mut reader = BufReader::new(stdin).lines();
    let mut stdout = tokio::io::stdout();

    while let Ok(Some(line)) = reader.next_line().await {
        if line.trim().is_empty() {
            continue;
        }

        let msg: Value = match serde_json::from_str(&line) {
            Ok(v) => v,
            // Malformed JSON-RPC: the body won't parse, so there's no recoverable `id` to reply
            // to (a JSON-RPC parse-error reply would carry `id: null`). Log rather than drop silently.
            Err(e) => {
                warn!("dropping unparseable JSON-RPC line: {e}");
                continue;
            }
        };

        if let Some(response) =
            handle_rpc(&tools, &ctx.runtime, &ctx.own_inbox, &ctx.inbox_wake, msg).await
        {
            let mut bytes = serde_json::to_vec(&response).map_err(std::io::Error::other)?;
            bytes.push(b'\n');
            stdout.write_all(&bytes).await?;
            stdout.flush().await?;
        }
    }

    Ok(())
}

async fn handle_rpc(
    tools: &[Box<dyn ErasedTool<Runtime>>],
    runtime: &Runtime,
    own_inbox: &exo_caps::InboxPath,
    inbox_wake: &tokio::sync::Notify,
    msg: Value,
) -> Option<Value> {
    let req = Request::parse(&msg);
    let id = req.id.clone();
    let is_notification = id.is_none() || id.as_ref().map(|v| v.is_null()).unwrap_or(false);

    let result: Option<Result<Value, (i32, String)>> = match req.method {
        "initialize" => {
            let requested_version = req
                .params
                .and_then(|params| params.get("protocolVersion"))
                .and_then(Value::as_str);
            let negotiated_version = match requested_version {
                Some(LEGACY_MCP_PROTOCOL_VERSION) => LEGACY_MCP_PROTOCOL_VERSION,
                _ => MCP_PROTOCOL_VERSION,
            };
            Some(Ok(json!({
                "protocolVersion": negotiated_version,
                "capabilities": { "tools": {} },
                "serverInfo": { "name": "exomonad-node", "version": env!("CARGO_PKG_VERSION") }
            })))
        }
        "notifications/initialized" => None,
        "tools/list" => {
            let tool_list: Vec<_> = tools
                .iter()
                .map(|t| {
                    json!({
                        "name": t.name(),
                        "description": t.description(),
                        "inputSchema": t.schema(),
                    })
                })
                .collect();
            Some(Ok(json!({ "tools": tool_list })))
        }
        "tools/call" => Some(call_tool(tools, runtime, own_inbox, inbox_wake, req.params).await),
        _ => {
            if is_notification {
                None
            } else {
                Some(Err((-32601, format!("Method not found: {}", req.method))))
            }
        }
    };

    if let (Some(res), Some(id)) = (result, id) {
        match res {
            Ok(val) => Some(ok_response(id, val)),
            Err((code, message)) => Some(error_response(id, code, message)),
        }
    } else {
        None
    }
}

async fn call_tool(
    tools: &[Box<dyn ErasedTool<Runtime>>],
    runtime: &Runtime,
    own_inbox: &exo_caps::InboxPath,
    inbox_wake: &tokio::sync::Notify,
    params: Option<&Value>,
) -> Result<Value, (i32, String)> {
    let name = params.and_then(|p| p.get("name")).and_then(|n| n.as_str());
    let arguments = params
        .and_then(|p| p.get("arguments"))
        .cloned()
        .unwrap_or(json!({}));

    let Some(name) = name else {
        return Err((-32602, "Missing tool name".to_string()));
    };

    let Some(tool) = tools.iter().find(|t| t.name() == name) else {
        return Err((-32601, format!("Tool not found: {name}")));
    };

    capture_codex_binding(runtime, own_inbox, inbox_wake, params).await?;

    match tool.call(runtime, arguments).await {
        Ok(output) => {
            // Map ToolOutput (text, data) to MCP CallToolResult
            let mut content = vec![json!({
                "type": "text",
                "text": output.get("text").and_then(|t| t.as_str()).unwrap_or(""),
            })];

            if let Some(data) = output.get("data") {
                if !data.is_null() {
                    content.push(json!({
                        "type": "text",
                        "text": format!("Data: {}", serde_json::to_string_pretty(data).unwrap_or_default()),
                    }));
                }
            }

            Ok(json!({ "content": content }))
        }
        Err(e) => Err((-32603, e.to_string())),
    }
}

/// Learn the real Codex thread identity from metadata attached to every Codex MCP tool call.
///
/// Recording it before executing the tool is important: a node's first Exomonad action may be a
/// spawn or send, and those operations must already be able to fork or queue against the caller's
/// actual thread. Claude does not provide this metadata and does not need a Codex binding.
async fn capture_codex_binding(
    runtime: &Runtime,
    own_inbox: &exo_caps::InboxPath,
    inbox_wake: &tokio::sync::Notify,
    params: Option<&Value>,
) -> Result<(), (i32, String)> {
    if runtime.agent_type() != exo_caps::AgentType::Codex {
        return Ok(());
    }

    let thread_id = params
        .and_then(|params| params.get("_meta"))
        .and_then(|meta| meta.get("threadId"))
        .and_then(Value::as_str)
        .ok_or_else(|| {
            (
                -32602,
                "Codex tools/call metadata is missing _meta.threadId".to_string(),
            )
        })?;
    let codex = runtime
        .codex_node()
        .ok_or_else(|| (-32603, "Codex node is missing its binding path".to_string()))?;

    if exo_runtime::codex::read_binding(&codex.binding)
        .await
        .is_ok_and(|binding| binding.thread_id == thread_id)
    {
        return Ok(());
    }

    exo_runtime::codex::write_binding(&codex.binding, thread_id)
        .await
        .map_err(|e| {
            (
                -32603,
                format!("failed to persist Codex thread binding: {e}"),
            )
        })?;
    tracing::info!(thread_id, "bound Exomonad node to Codex thread");
    write_binding_wake_marker(own_inbox.as_path(), thread_id)
        .await
        .map_err(|e| (-32603, format!("failed to wake Codex inbox owner: {e}")))?;
    inbox_wake.notify_one();
    Ok(())
}

async fn write_binding_wake_marker(inbox_path: &Path, thread_id: &str) -> std::io::Result<()> {
    let inbox = exo_caps::InboxPath::new(inbox_path.to_path_buf());
    let marker = exo_caps::paths::binding_wake_path(&inbox);
    if let Some(parent) = marker.parent() {
        tokio::fs::create_dir_all(parent).await?;
    }
    tokio::fs::write(marker, thread_id.as_bytes()).await
}

struct Request<'a> {
    id: Option<Value>,
    method: &'a str,
    params: Option<&'a Value>,
}

impl<'a> Request<'a> {
    fn parse(msg: &'a Value) -> Self {
        Self {
            id: msg.get("id").cloned(),
            method: msg.get("method").and_then(|m| m.as_str()).unwrap_or(""),
            params: msg.get("params"),
        }
    }
}

fn ok_response(id: Value, result: Value) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": result
    })
}

fn error_response(id: Value, code: i32, message: String) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "error": { "code": code, "message": message }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, AgentType, Branch, CodexNode, NodePath, PaneId};

    const THREAD: &str = "01a05a16-97f5-7722-aa8d-467e01e2e5b4";

    fn test_runtime(temp_path: std::path::PathBuf) -> Runtime {
        Runtime::new(
            NodePath::new(vec![AgentName::new("root".into()).unwrap()]).unwrap(),
            Branch::new("main".into()).unwrap(),
            temp_path,
            None,
            "test-run".into(),
            "test-session".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        )
    }

    fn test_inbox(temp: &tempfile::TempDir) -> exo_caps::InboxPath {
        exo_caps::InboxPath::new(temp.path().join("inbox.jsonl"))
    }

    #[tokio::test]
    async fn test_handle_rpc_tools_list() {
        use exo_caps::CapResult;
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());

        // A node serves whatever tools its injected roster provides; the concrete roster is the
        // domain's concern (tested in `exo`). Here a single named tool stands in for any roster.
        struct ListTestTool;
        #[async_trait::async_trait]
        impl ErasedTool<Runtime> for ListTestTool {
            fn name(&self) -> &str {
                "list_test_tool"
            }
            fn description(&self) -> &str {
                "desc"
            }
            fn schema(&self) -> Value {
                json!({})
            }
            async fn call(&self, _runtime: &Runtime, _args: Value) -> CapResult<Value> {
                Ok(json!({ "text": "" }))
            }
        }
        let tools: Vec<Box<dyn ErasedTool<Runtime>>> = vec![Box::new(ListTestTool)];

        let request = json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/list"
        });

        let response = handle_rpc(
            &tools,
            &runtime,
            &test_inbox(&temp),
            &tokio::sync::Notify::new(),
            request,
        )
        .await
        .unwrap();
        assert_eq!(response["id"], 1);
        let tools_out = response["result"]["tools"].as_array().unwrap();
        let names: Vec<_> = tools_out
            .iter()
            .map(|t| t["name"].as_str().unwrap())
            .collect();
        assert!(names.contains(&"list_test_tool"));
    }

    #[tokio::test]
    async fn test_handle_rpc_initialize() {
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());
        let tools = vec![];

        let request = json!({
            "jsonrpc": "2.0",
            "id": "init-1",
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-06-18",
                "capabilities": {},
                "clientInfo": { "name": "codex-mcp-client", "version": "test" }
            }
        });

        let response = handle_rpc(
            &tools,
            &runtime,
            &test_inbox(&temp),
            &tokio::sync::Notify::new(),
            request,
        )
        .await
        .unwrap();
        assert_eq!(response["id"], "init-1");
        assert_eq!(response["result"]["protocolVersion"], "2025-06-18");
        assert_eq!(response["result"]["serverInfo"]["name"], "exomonad-node");
    }

    #[tokio::test]
    async fn test_handle_rpc_initialize_retains_legacy_compatibility() {
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());

        let request = json!({
            "jsonrpc": "2.0",
            "id": "legacy-init",
            "method": "initialize",
            "params": { "protocolVersion": "2024-11-05" }
        });

        let response = handle_rpc(
            &[],
            &runtime,
            &test_inbox(&temp),
            &tokio::sync::Notify::new(),
            request,
        )
        .await
        .unwrap();
        assert_eq!(response["result"]["protocolVersion"], "2024-11-05");
    }

    #[tokio::test]
    async fn test_handle_rpc_tools_call_happy() {
        use exo_caps::CapResult;
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());

        // Create a dummy tool that returns a predictable result
        struct TestTool;
        #[async_trait::async_trait]
        impl ErasedTool<Runtime> for TestTool {
            fn name(&self) -> &str {
                "test_tool"
            }
            fn description(&self) -> &str {
                "desc"
            }
            fn schema(&self) -> Value {
                json!({})
            }
            async fn call(&self, _runtime: &Runtime, _args: Value) -> CapResult<Value> {
                Ok(json!({ "text": "hello world", "data": { "foo": "bar" } }))
            }
        }

        let tools: Vec<Box<dyn ErasedTool<Runtime>>> = vec![Box::new(TestTool)];

        let request = json!({
            "jsonrpc": "2.0",
            "id": "call-1",
            "method": "tools/call",
            "params": {
                "name": "test_tool",
                "arguments": {}
            }
        });

        let response = handle_rpc(
            &tools,
            &runtime,
            &test_inbox(&temp),
            &tokio::sync::Notify::new(),
            request,
        )
        .await
        .unwrap();
        assert_eq!(response["id"], "call-1");
        let content = response["result"]["content"].as_array().unwrap();
        assert_eq!(content[0]["text"], "hello world");
        assert!(content[1]["text"]
            .as_str()
            .unwrap()
            .contains("\"foo\": \"bar\""));
    }

    #[tokio::test]
    async fn test_handle_rpc_unknown_method() {
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf());
        let tools = vec![];

        let request = json!({
            "jsonrpc": "2.0",
            "id": "err-1",
            "method": "unknown/method"
        });

        let response = handle_rpc(
            &tools,
            &runtime,
            &test_inbox(&temp),
            &tokio::sync::Notify::new(),
            request,
        )
        .await
        .unwrap();
        assert_eq!(response["id"], "err-1");
        assert_eq!(response["error"]["code"], -32601);
        assert!(response["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Method not found"));
    }

    #[tokio::test]
    async fn codex_tool_call_persists_real_thread_id_before_execution() {
        use exo_caps::CapResult;

        let temp = tempfile::tempdir().unwrap();
        let binding = temp.path().join("codex-binding.json");
        let runtime = test_runtime(temp.path().to_path_buf()).with_agent_backend(
            AgentType::Codex,
            Some(CodexNode {
                binding: binding.clone(),
            }),
        );

        struct TestTool;
        #[async_trait::async_trait]
        impl ErasedTool<Runtime> for TestTool {
            fn name(&self) -> &str {
                "test_tool"
            }
            fn description(&self) -> &str {
                "desc"
            }
            fn schema(&self) -> Value {
                json!({})
            }
            async fn call(&self, runtime: &Runtime, _args: Value) -> CapResult<Value> {
                let codex = runtime.codex_node().unwrap();
                let persisted = exo_runtime::codex::read_binding(&codex.binding)
                    .await
                    .unwrap();
                assert_eq!(persisted.thread_id, THREAD);
                Ok(json!({ "text": "bound" }))
            }
        }

        let request = json!({
            "jsonrpc": "2.0",
            "id": "call-1",
            "method": "tools/call",
            "params": {
                "name": "test_tool",
                "arguments": {},
                "_meta": { "threadId": THREAD }
            }
        });
        let response = handle_rpc(
            &[Box::new(TestTool)],
            &runtime,
            &test_inbox(&temp),
            &tokio::sync::Notify::new(),
            request,
        )
        .await
        .unwrap();

        assert_eq!(response["result"]["content"][0]["text"], "bound");
        assert_eq!(
            exo_runtime::codex::read_binding(&binding)
                .await
                .unwrap()
                .thread_id,
            THREAD
        );
        assert_eq!(
            tokio::fs::read_to_string(exo_caps::paths::binding_wake_path(&test_inbox(&temp)))
                .await
                .unwrap(),
            THREAD
        );
    }

    #[tokio::test]
    async fn codex_tool_call_rejects_missing_thread_metadata() {
        let temp = tempfile::tempdir().unwrap();
        let runtime = test_runtime(temp.path().to_path_buf()).with_agent_backend(
            AgentType::Codex,
            Some(CodexNode {
                binding: temp.path().join("codex-binding.json"),
            }),
        );

        struct TestTool;
        #[async_trait::async_trait]
        impl ErasedTool<Runtime> for TestTool {
            fn name(&self) -> &str {
                "test_tool"
            }
            fn description(&self) -> &str {
                "desc"
            }
            fn schema(&self) -> Value {
                json!({})
            }
            async fn call(&self, _runtime: &Runtime, _args: Value) -> exo_caps::CapResult<Value> {
                panic!("tool must not run without a Codex thread binding")
            }
        }

        let response = handle_rpc(
            &[Box::new(TestTool)],
            &runtime,
            &test_inbox(&temp),
            &tokio::sync::Notify::new(),
            json!({
                "jsonrpc": "2.0",
                "id": "call-1",
                "method": "tools/call",
                "params": { "name": "test_tool", "arguments": {} }
            }),
        )
        .await
        .unwrap();

        assert_eq!(response["error"]["code"], -32602);
        assert!(response["error"]["message"]
            .as_str()
            .unwrap()
            .contains("_meta.threadId"));
    }
}
