//! Popup service for WASM host functions.
//!
//! Shows interactive popup forms and wizards via direct Zellij IPC and returns user response.
//!
//! Uses `ZellijIpc::pipe_to_plugin_blocking` (direct Unix socket, no subprocess):
//! 1. Sends `Action::CliPipe` with `floating: false` for a tiled pane
//! 2. Plugin receives via `pipe()` with `PipeSource::Cli(pipe_id)` and payload
//! 3. Plugin renders form UI in a tiled pane
//! 4. On submit, plugin calls `cli_pipe_output(&pipe_id, "{JSON response}")`
//! 5. IPC reads `ServerToClientMsg::CliPipeOutput` from the socket

use crate::services::zellij_ipc::ZellijIpc;
use anyhow::Result;
use serde::{Deserialize, Serialize};

// ============================================================================
// Input/Output types
// ============================================================================

/// Input for show_popup.
///
/// raw_json is either:
/// - A JSON array of component objects (form mode)
/// - A JSON object with "panes" key (wizard mode)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PopupInput {
    /// Title displayed at the top of the popup.
    pub title: String,
    /// Raw JSON payload: array = form components, object with "panes" = wizard.
    pub raw_json: serde_json::Value,
    /// Target Zellij tab name for routing. Only the plugin instance in this tab renders.
    pub target_tab: Option<String>,
}

/// Output from show_popup.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PopupOutput {
    /// Button clicked: "submit" or "cancelled"
    pub button: String,
    /// Component values as JSON object
    pub values: serde_json::Value,
}

// ============================================================================
// Popup Service
// ============================================================================

/// Popup service for showing interactive forms and wizards.
pub struct PopupService {
    ipc: ZellijIpc,
    plugin_path: String,
}

impl PopupService {
    /// Create a new popup service with the specified Zellij IPC and plugin path.
    pub fn new(ipc: ZellijIpc, plugin_path: String) -> Self {
        Self { ipc, plugin_path }
    }

    /// Show a popup and wait for user response.
    ///
    /// Synchronous, blocks until the popup plugin sends a response via the pipe.
    pub fn show_popup(&self, payload: &str) -> Result<String> {
        self.ipc
            .pipe_to_plugin_blocking(&self.plugin_path, "exomonad-popup", payload, false, Some("Popup"))
    }
}

/// Sanitize a string for use in the legacy payload protocol.
pub fn sanitize_payload_field(s: &str) -> String {
    s.replace('|', "-").replace(',', ";")
}

/// Extract choice items from popup components (legacy fallback).
pub fn extract_choice_items(components: &[serde_json::Value]) -> Vec<String> {
    for component in components {
        if component.get("type").and_then(|t| t.as_str()) == Some("choice") {
            if let Some(options) = component.get("options").and_then(|o| o.as_array()) {
                return options
                    .iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect();
            }
        }
    }

    components
        .iter()
        .filter_map(|c| {
            if c.get("type").and_then(|t| t.as_str()) == Some("text") {
                c.get("content").and_then(|v| v.as_str().map(String::from))
            } else {
                None
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_popup_input_serialization() {
        let input = PopupInput {
            title: "Test".to_string(),
            raw_json: serde_json::json!([{
                "type": "text",
                "id": "msg",
                "content": "Hello"
            }]),
            target_tab: None,
        };

        let json = serde_json::to_string(&input).unwrap();
        let parsed: PopupInput = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.title, "Test");
        assert!(parsed.raw_json.is_array());
    }

    #[test]
    fn test_popup_output_serialization() {
        let output = PopupOutput {
            button: "submit".to_string(),
            values: serde_json::json!({"choice": "option1"}),
        };

        let json = serde_json::to_string(&output).unwrap();
        let parsed: PopupOutput = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.button, "submit");
    }

    #[test]
    fn test_extract_choice_items_from_choice_component() {
        let components = vec![serde_json::json!({
            "type": "choice",
            "id": "action",
            "label": "Select action",
            "options": ["Option A", "Option B", "Option C"]
        })];

        let items = extract_choice_items(&components);
        assert_eq!(items, vec!["Option A", "Option B", "Option C"]);
    }

    #[test]
    fn test_extract_choice_items_fallback_to_text() {
        let components = vec![
            serde_json::json!({
                "type": "text",
                "id": "t1",
                "content": "First Item"
            }),
            serde_json::json!({
                "type": "text",
                "id": "t2",
                "content": "Second Item"
            }),
        ];

        let items = extract_choice_items(&components);
        assert_eq!(items, vec!["First Item", "Second Item"]);
    }

    #[test]
    fn test_extract_choice_items_empty() {
        let components: Vec<serde_json::Value> = vec![];
        let items = extract_choice_items(&components);
        assert!(items.is_empty());
    }

    #[test]
    fn test_extract_choice_items_prefers_choice_over_text() {
        let components = vec![
            serde_json::json!({
                "type": "text",
                "id": "header",
                "content": "Please select:"
            }),
            serde_json::json!({
                "type": "choice",
                "id": "action",
                "label": "Select action",
                "options": ["A", "B"]
            }),
        ];

        let items = extract_choice_items(&components);
        assert_eq!(items, vec!["A", "B"]);
    }

    #[test]
    fn test_sanitize_payload_field() {
        assert_eq!(sanitize_payload_field("Select | Insert"), "Select - Insert");
        assert_eq!(sanitize_payload_field("A, B, C"), "A; B; C");
        assert_eq!(sanitize_payload_field("X|Y,Z"), "X-Y;Z");
        assert_eq!(sanitize_payload_field("Normal Text"), "Normal Text");
    }
}
