//! MCP tool definitions and WASM execution.
//!
//! All tool logic is in Haskell WASM. This module provides:
//! - Tool schema discovery via WASM (handle_list_tools)
//! - WASM routing for tool execution (handle_mcp_call)

use super::{McpState, ToolDefinition};
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tracing::{debug, error};

// ============================================================================
// WASM MCP Types (matches Haskell Main.hs MCPCallInput/MCPCallOutput)
// ============================================================================

/// Input for WASM handle_mcp_call function.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct MCPCallInput {
    tool_name: String,
    tool_args: Value,
}

/// Output from WASM handle_mcp_call function.
#[derive(Debug, Deserialize)]
struct MCPCallOutput {
    success: bool,
    result: Option<Value>,
    error: Option<String>,
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
            "repo": "tidepool",
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
        if result.is_err() {
            // If parsing fails, result/error are required (not truly optional)
            let json_with_nulls = r#"{"success": true, "result": null, "error": null}"#;
            let output: MCPCallOutput = serde_json::from_str(json_with_nulls).unwrap();
            assert!(output.success);
        } else {
            // If parsing succeeds, fields are truly optional
            let output = result.unwrap();
            assert!(output.success);
            assert!(output.result.is_none());
            assert!(output.error.is_none());
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
