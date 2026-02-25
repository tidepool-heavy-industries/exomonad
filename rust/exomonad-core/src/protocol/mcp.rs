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

#[cfg(test)]
mod proptest_tests {
    use super::*;
    use proptest::prelude::*;

    fn arb_tool_name() -> impl Strategy<Value = ToolName> {
        "[a-zA-Z0-9_]+".prop_map(|s| ToolName::try_from(s).unwrap())
    }

    fn arb_json_value() -> impl Strategy<Value = Value> {
        prop_oneof![
            any::<bool>().prop_map(Value::Bool),
            any::<String>().prop_map(Value::String),
        ]
    }

    proptest! {
        #[test]
        fn test_tool_definition_roundtrip(
            name in arb_tool_name(),
            description in any::<String>(),
            input_schema in arb_json_value()
        ) {
            let td = ToolDefinition { name, description, input_schema };
            let json = serde_json::to_string(&td).unwrap();
            let back: ToolDefinition = serde_json::from_str(&json).unwrap();
            prop_assert_eq!(back.name, td.name);
            prop_assert_eq!(back.description, td.description);
            prop_assert_eq!(back.input_schema, td.input_schema);
        }

        #[test]
        fn test_mcp_error_roundtrip(
            code in any::<i32>(),
            message in any::<String>(),
            details in prop::option::weighted(0.5, arb_json_value()),
            suggestion in prop::option::weighted(0.5, any::<String>())
        ) {
            let err = McpError { code, message, details, suggestion };
            let json = serde_json::to_string(&err).unwrap();
            let back: McpError = serde_json::from_str(&json).unwrap();
            prop_assert_eq!(back.code, err.code);
            prop_assert_eq!(back.message, err.message);
            prop_assert_eq!(back.details, err.details);
            prop_assert_eq!(back.suggestion, err.suggestion);
        }
    }
}
