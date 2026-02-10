//! MCP tool definitions and WASM execution.
//!
//! Most tool logic is in Haskell WASM. This module provides:
//! - Tool schema discovery via WASM (handle_list_tools)
//! - WASM routing for tool execution (handle_mcp_call)
//! - Direct Rust tools for TL-side messaging (get_agent_messages, answer_question)

use super::{McpState, ToolDefinition};
use crate::services::messaging;
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tracing::{debug, error, info};

// ============================================================================
// WASM MCP Types (matches Haskell Main.hs MCPCallInput/MCPCallOutput)
// ============================================================================

/// Input for WASM handle_mcp_call function.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MCPCallInput {
    tool_name: String,
    tool_args: Value,
}

impl MCPCallInput {
    pub fn new(tool_name: String, tool_args: Value) -> Self {
        Self {
            tool_name,
            tool_args,
        }
    }
}

/// Output from WASM handle_mcp_call function.
#[derive(Debug, Deserialize)]
pub struct MCPCallOutput {
    pub success: bool,
    pub result: Option<Value>,
    pub error: Option<String>,
}

#[cfg(feature = "runtime")]
impl MCPCallOutput {
    /// Convert to rmcp `CallToolResult`.
    pub fn into_call_tool_result(self) -> Result<rmcp::model::CallToolResult, rmcp::ErrorData> {
        if self.success {
            let text = self
                .result
                .map(|v| serde_json::to_string_pretty(&v).unwrap_or_default())
                .unwrap_or_default();
            Ok(rmcp::model::CallToolResult::success(vec![
                rmcp::model::Content::text(text),
            ]))
        } else {
            let error_text = self
                .error
                .unwrap_or_else(|| "Unknown WASM error".to_string());
            Ok(rmcp::model::CallToolResult::error(vec![
                rmcp::model::Content::text(error_text),
            ]))
        }
    }
}

// ============================================================================
// Tool Discovery (via WASM)
// ============================================================================

/// Get all available tool definitions from WASM plugin.
///
/// Calls handle_list_tools in Haskell WASM, which returns the tool schemas
/// defined in ExoMonad.Guest.Tools. This is the single source of truth.
pub async fn get_tool_definitions(state: &McpState) -> Result<Vec<ToolDefinition>> {
    debug!("Fetching tool definitions from WASM");

    // handle_list_tools takes no input (empty object)
    let tools: Vec<ToolDefinition> = state
        .plugin
        .call("handle_list_tools", &())
        .await
        .context("WASM handle_list_tools failed")?;

    debug!(count = tools.len(), "Got tool definitions from WASM");
    Ok(tools)
}

// ============================================================================
// WASM Execution
// ============================================================================

/// Execute a tool by name via WASM.
///
/// All tool calls are routed through handle_mcp_call in Haskell WASM.
/// Haskell handles the pure logic, Rust executes I/O via host functions.
pub async fn execute_tool(state: &McpState, name: &str, args: Value) -> Result<Value> {
    debug!(tool = %name, "Routing tool call through WASM");

    let input = MCPCallInput {
        tool_name: name.to_string(),
        tool_args: args,
    };

    // Log before calling WASM
    debug!(tool = %name, "Calling WASM handle_mcp_call");

    let call_result = state.plugin.call("handle_mcp_call", &input).await;

    // Log raw result before deserialization
    match &call_result {
        Ok(_) => debug!(tool = %name, "WASM call succeeded"),
        Err(e) => {
            error!(
                tool = %name,
                error = %e,
                "WASM call failed before deserialization"
            );
        }
    }

    let output: MCPCallOutput = call_result.map_err(|e| {
        // Preserve full error chain with tool name
        anyhow!("WASM call failed for tool '{}': {}", name, e)
    })?;

    // Log the parsed output
    debug!(
        tool = %name,
        success = output.success,
        has_result = output.result.is_some(),
        has_error = output.error.is_some(),
        "Parsed MCPCallOutput"
    );

    if let Some(ref err) = output.error {
        error!(tool = %name, mcp_error = %err, "Tool returned error in MCPCallOutput");
    }

    if output.success {
        output
            .result
            .ok_or_else(|| anyhow!("WASM returned success but no result"))
    } else {
        Err(anyhow!(output
            .error
            .unwrap_or_else(|| "Unknown WASM error".to_string())))
    }
}

// ============================================================================
// Direct Rust Tools (TL-side messaging, bypass WASM)
// ============================================================================

/// Tool definitions for TL-side messaging tools (registered alongside WASM tools).
pub fn tl_messaging_tool_definitions() -> Vec<ToolDefinition> {
    vec![
        ToolDefinition {
            name: "get_agent_messages".to_string(),
            description: "Read notes and pending questions from agent outboxes. Scans all agents (or a specific agent) for messages.".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "agent_id": {
                        "type": "string",
                        "description": "Filter to a specific agent directory name. If omitted, reads from all agents."
                    },
                    "subrepo": {
                        "type": "string",
                        "description": "Subrepo path (e.g. 'egregore/') to scope agent scanning."
                    }
                }
            }),
        },
        ToolDefinition {
            name: "answer_question".to_string(),
            description: "Answer a pending question from an agent. Writes the answer to the agent's inbox, unblocking their send_question call.".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "agent_id": {
                        "type": "string",
                        "description": "The agent directory name (e.g. 'gh-42-fix-bug-claude')."
                    },
                    "question_id": {
                        "type": "string",
                        "description": "The question ID to answer (e.g. 'q-abc123')."
                    },
                    "answer": {
                        "type": "string",
                        "description": "The answer text."
                    },
                    "subrepo": {
                        "type": "string",
                        "description": "Subrepo path (e.g. 'egregore/') if agent is in a subrepo."
                    }
                },
                "required": ["agent_id", "question_id", "answer"]
            }),
        },
    ]
}

/// Check if a tool name is a direct Rust tool (not WASM).
pub fn is_direct_rust_tool(name: &str) -> bool {
    matches!(name, "get_agent_messages" | "answer_question")
}

/// Execute a direct Rust tool by name.
pub async fn execute_direct_tool(state: &McpState, name: &str, args: Value) -> Result<Value> {
    match name {
        "get_agent_messages" => execute_get_agent_messages(state, args).await,
        "answer_question" => execute_answer_question(state, args).await,
        _ => Err(anyhow!("Unknown direct tool: {}", name)),
    }
}

async fn execute_get_agent_messages(_state: &McpState, args: Value) -> Result<Value> {
    let agent_id = args.get("agent_id").and_then(|v| v.as_str());

    let team_name = get_team_name_from_env()
        .ok_or_else(|| anyhow!("No EXOMONAD_TEAM_NAME or CLAUDE_TEAM_NAME set"))?;

    info!(
        agent_id = ?agent_id,
        team = %team_name,
        "Getting agent messages from Teams inbox"
    );

    if let Some(agent_id) = agent_id {
        let messages = messaging::read_agent_inbox(&team_name, agent_id)
            .await
            .map_err(|e| anyhow!("Failed to read agent inbox: {}", e))?;

        info!(agent = %agent_id, count = messages.len(), "Read agent messages from Teams inbox");
        Ok(json!({
            "agent_id": agent_id,
            "messages": messages,
        }))
    } else {
        let results = messaging::scan_all_agent_messages_teams(&team_name)
            .await
            .map_err(|e| anyhow!("Failed to scan agent messages: {}", e))?;

        let agents: Vec<Value> = results
            .into_iter()
            .map(|(id, msgs)| {
                json!({
                    "agent_id": id,
                    "messages": msgs,
                })
            })
            .collect();

        info!(agent_count = agents.len(), "Scanned all agent messages from Teams inboxes");
        Ok(json!({ "agents": agents }))
    }
}

async fn execute_answer_question(_state: &McpState, args: Value) -> Result<Value> {
    let agent_id = args
        .get("agent_id")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("agent_id is required"))?;
    let question_id = args
        .get("question_id")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("question_id is required"))?;
    let answer = args
        .get("answer")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("answer is required"))?;

    let team_name = get_team_name_from_env()
        .ok_or_else(|| anyhow!("No EXOMONAD_TEAM_NAME or CLAUDE_TEAM_NAME set"))?;

    info!(
        agent = %agent_id,
        question_id = %question_id,
        team = %team_name,
        "Answering question via Teams inbox"
    );

    messaging::write_to_agent_inbox(&team_name, agent_id, answer)
        .await
        .map_err(|e| anyhow!("Failed to write answer to Teams inbox: {}", e))?;

    info!(agent = %agent_id, question_id = %question_id, "Answer written to Teams inbox");
    Ok(json!({
        "status": "answered",
        "agent_id": agent_id,
        "question_id": question_id,
    }))
}

/// Get team name from environment variables.
fn get_team_name_from_env() -> Option<String> {
    std::env::var("EXOMONAD_TEAM_NAME")
        .or_else(|_| std::env::var("CLAUDE_TEAM_NAME"))
        .ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn mcp_call_input_serializes_camel_case() {
        let input = MCPCallInput {
            tool_name: "git_branch".to_string(),
            tool_args: json!({"path": "/tmp"}),
        };
        let json = serde_json::to_string(&input).unwrap();

        // Verify camelCase serialization
        assert!(json.contains("toolName"), "Expected toolName in JSON");
        assert!(json.contains("toolArgs"), "Expected toolArgs in JSON");

        // Verify snake_case is NOT present
        assert!(
            !json.contains("tool_name"),
            "Unexpected snake_case tool_name"
        );
        assert!(
            !json.contains("tool_args"),
            "Unexpected snake_case tool_args"
        );
    }

    #[test]
    fn mcp_call_input_preserves_tool_args() {
        let args = json!({
            "owner": "exomonad",
            "repo": "exomonad",
            "number": 42
        });

        let input = MCPCallInput {
            tool_name: "github_get_issue".to_string(),
            tool_args: args.clone(),
        };

        let serialized = serde_json::to_value(&input).unwrap();
        assert_eq!(serialized["toolArgs"], args);
    }

    #[test]
    fn mcp_call_output_deserializes_success() {
        let json = r#"{"success": true, "result": {"branch": "main"}, "error": null}"#;
        let output: MCPCallOutput = serde_json::from_str(json).unwrap();

        assert!(output.success);
        assert!(output.result.is_some());
        assert_eq!(output.result.unwrap()["branch"], "main");
        assert!(output.error.is_none());
    }

    #[test]
    fn mcp_call_output_deserializes_error() {
        let json = r#"{"success": false, "result": null, "error": "something failed"}"#;
        let output: MCPCallOutput = serde_json::from_str(json).unwrap();

        assert!(!output.success);
        assert!(output.result.is_none());
        assert_eq!(output.error, Some("something failed".to_string()));
    }

    #[test]
    fn mcp_call_output_deserializes_with_complex_result() {
        let json = r#"{
            "success": true,
            "result": {
                "issues": [
                    {"number": 1, "title": "Bug", "state": "open"},
                    {"number": 2, "title": "Feature", "state": "closed"}
                ]
            },
            "error": null
        }"#;
        let output: MCPCallOutput = serde_json::from_str(json).unwrap();

        assert!(output.success);
        let result = output.result.unwrap();
        let issues = result["issues"].as_array().unwrap();
        assert_eq!(issues.len(), 2);
        assert_eq!(issues[0]["number"], 1);
        assert_eq!(issues[1]["title"], "Feature");
    }

    #[test]
    fn mcp_call_output_requires_success_field() {
        // Missing success field should fail
        let json = r#"{"result": {"ok": true}, "error": null}"#;
        let result: Result<MCPCallOutput, _> = serde_json::from_str(json);
        assert!(result.is_err());
    }

    #[test]
    fn mcp_call_output_handles_missing_optional_fields() {
        // Only success is required; result and error can be missing
        let json = r#"{"success": true}"#;
        let result: Result<MCPCallOutput, _> = serde_json::from_str(json);

        // Document actual behavior - if this fails, optional fields need defaults
        if let Ok(output) = result {
            // If parsing succeeds, fields are truly optional
            assert!(output.success);
            assert!(output.result.is_none());
            assert!(output.error.is_none());
        } else {
            // If parsing fails, result/error are required (not truly optional)
            let json_with_nulls = r#"{"success": true, "result": null, "error": null}"#;
            let output: MCPCallOutput = serde_json::from_str(json_with_nulls).unwrap();
            assert!(output.success);
        }
    }

    #[test]
    fn mcp_call_input_empty_args() {
        let input = MCPCallInput {
            tool_name: "list_agents".to_string(),
            tool_args: json!({}),
        };

        let serialized = serde_json::to_value(&input).unwrap();
        assert_eq!(serialized["toolName"], "list_agents");
        assert_eq!(serialized["toolArgs"], json!({}));
    }

    #[test]
    fn mcp_call_input_nested_args() {
        let input = MCPCallInput {
            tool_name: "spawn_agents".to_string(),
            tool_args: json!({
                "issues": ["1", "2", "3"],
                "owner": "test",
                "repo": "repo",
                "worktree_dir": "/path/to/worktrees"
            }),
        };

        let serialized = serde_json::to_value(&input).unwrap();
        let args = &serialized["toolArgs"];
        assert_eq!(args["issues"].as_array().unwrap().len(), 3);
        assert_eq!(args["owner"], "test");
    }
}
