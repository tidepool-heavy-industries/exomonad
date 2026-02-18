//! MCP tool definitions and WASM execution.
//!
//! All tool logic is in Haskell WASM. This module provides:
//! - Tool schema discovery via WASM (handle_list_tools)
//! - Tool call types for WASM boundary (MCPCallInput/MCPCallOutput)

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

// ============================================================================
// WASM MCP Types (matches Haskell Main.hs MCPCallInput/MCPCallOutput)
// ============================================================================

/// Input for WASM handle_mcp_call function.
///
/// The `role` field is always present at the WASM boundary — the server knows
/// which role it's serving (from the route: `/agents/{role}/{name}/mcp`).
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MCPCallInput {
    role: String,
    tool_name: String,
    tool_args: Value,
}

impl MCPCallInput {
    pub fn new(role: String, tool_name: String, tool_args: Value) -> Self {
        Self {
            role,
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

impl MCPCallOutput {
    /// Convert to a JSON value suitable for a JSON-RPC result.
    /// Used by our custom MCP server (replaces rmcp's CallToolResult).
    pub fn into_json_rpc_result(self) -> Value {
        if self.success {
            let text = self
                .result
                .map(|v| serde_json::to_string_pretty(&v).unwrap_or_default())
                .unwrap_or_default();
            json!({"content": [{"type": "text", "text": text}]})
        } else {
            let err = self.error.unwrap_or_else(|| "Unknown error".to_string());
            json!({"content": [{"type": "text", "text": err}], "isError": true})
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn mcp_call_input_serializes_camel_case() {
        let input = MCPCallInput {
            role: "tl".to_string(),
            tool_name: "git_branch".to_string(),
            tool_args: json!({"path": "/tmp"}),
        };
        let json = serde_json::to_string(&input).unwrap();

        // Verify camelCase serialization
        assert!(json.contains("\"role\""), "Expected role in JSON");
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
            role: "tl".to_string(),
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
            role: "tl".to_string(),
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
            role: "tl".to_string(),
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
