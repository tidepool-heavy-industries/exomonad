//! MCP (Model Context Protocol) types.

use crate::domain::ToolName;
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Tool definition for MCP discovery (must match Haskell ToolDefinition).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDefinition {
    pub name: ToolName,
    pub description: String,
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}

/// MCP error response.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpError {
    /// Error code.
    pub code: i32,
    /// Error message.
    pub message: String,
    /// Structured details for debugging.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<Value>,
    /// Actionable guidance to fix the issue.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suggestion: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tool_definition_serde_roundtrip() {
        let td = ToolDefinition {
            name: ToolName::from("spawn_subtree"),
            description: "Fork a worktree".into(),
            input_schema: serde_json::json!({"type": "object"}),
        };
        let json = serde_json::to_value(&td).unwrap();
        assert_eq!(json["name"], "spawn_subtree");
        assert_eq!(json["inputSchema"]["type"], "object");
        assert!(!json.as_object().unwrap().contains_key("input_schema"));

        let back: ToolDefinition = serde_json::from_value(json).unwrap();
        assert_eq!(back.name.as_str(), "spawn_subtree");
    }
}
