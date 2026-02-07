//! Popup service for WASM host functions.
//!
//! Shows interactive popup choice lists via Zellij CLI pipes and returns user selection.
//!
//! Uses the Zellij CLI pipe mechanism:
//! 1. `zellij pipe --plugin file:plugin.wasm --name exomonad:popup -- "request_id|title|item1,item2,item3"`
//! 2. Plugin receives via `pipe()` with `PipeSource::Cli(pipe_id)` and payload
//! 3. Plugin blocks CLI, shows popup UI
//! 4. On submit, plugin calls `cli_pipe_output(&pipe_id, "request_id:selected_item")`
//! 5. CLI receives response on stdout

use anyhow::{Context, Result};
use exomonad_ui_protocol::transport;
use serde::{Deserialize, Serialize};
use std::io::Read;
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

/// Timeout for waiting for popup response (5 minutes).
const POPUP_TIMEOUT: Duration = Duration::from_secs(300);

/// Default path to the Zellij plugin
const DEFAULT_PLUGIN_PATH: &str = "~/.config/zellij/plugins/exomonad-plugin.wasm";

/// Get the plugin path, checking it exists.
/// Override with EXOMONAD_PLUGIN_PATH env var.
///
/// NOTE: Returns expanded path (file:/Users/...) because Zellij CLI does NOT expand tilde.
/// This creates a separate plugin instance from KDL-loaded plugins (which use file:~/.config/...),
/// but that's intentional for ephemeral popup use case.
fn get_plugin_path() -> Result<String> {
    let path =
        std::env::var("EXOMONAD_PLUGIN_PATH").unwrap_or_else(|_| DEFAULT_PLUGIN_PATH.to_string());

    // Expand ~ to home directory (required for CLI commands - Zellij CLI doesn't expand ~)
    let expanded = if path.starts_with("~/") {
        if let Some(home) = dirs::home_dir() {
            home.join(&path[2..]).to_string_lossy().to_string()
        } else {
            path.clone()
        }
    } else {
        path.clone()
    };

    if !std::path::Path::new(&expanded).exists() {
        anyhow::bail!(
            "Zellij plugin not found at '{}'. Run 'just install-all' to install it.",
            expanded
        );
    }

    Ok(format!("file:{}", expanded))
}

// ============================================================================
// Input/Output types
// ============================================================================

/// Input for show_popup (matches Haskell PopupRequest).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PopupInput {
    /// Title displayed at the top of the popup.
    pub title: String,
    /// List of UI components.
    pub components: Vec<serde_json::Value>,
}

/// Output from show_popup (matches Haskell PopupResponse).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PopupOutput {
    /// Button clicked: "submit" or "cancel"
    pub button: String,
    /// Component values as JSON object
    pub values: serde_json::Value,
}

// ============================================================================
// Popup Service
// ============================================================================

/// Popup service for showing interactive forms.
pub struct PopupService {
    /// Zellij session name (from config, not detected).
    zellij_session: String,
}

impl PopupService {
    /// Create a new popup service with the specified Zellij session.
    pub fn new(zellij_session: String) -> Self {
        Self { zellij_session }
    }

    /// Show a popup and wait for user response.
    ///
    /// Uses Zellij CLI pipe mechanism with simple payload format:
    /// 1. `zellij pipe --plugin file:plugin.wasm --name exomonad:popup -- "request_id|title|item1,item2,item3"`
    /// 2. Plugin receives via `pipe()` with `PipeSource::Cli(pipe_id)` and payload
    /// 3. Plugin blocks CLI, shows popup UI, user interacts
    /// 4. On submit, plugin calls `cli_pipe_output(&pipe_id, "request_id:selected_item")`
    /// 5. This CLI command receives response on stdout and exits
    #[tracing::instrument(skip(self))]
    pub fn show_popup(&self, input: &PopupInput) -> Result<PopupOutput> {
        tracing::info!(title = %input.title, components = input.components.len(), "Showing popup");

        // Generate unique request ID
        let request_id = uuid::Uuid::new_v4().to_string();

        // Extract choice items from components
        let items = extract_choice_items(&input.components);

        if items.is_empty() {
            anyhow::bail!("Popup must have at least one choice item");
        }

        // Verify plugin exists
        let plugin_path = get_plugin_path()?;
        let session = &self.zellij_session;

        // Build payload: "request_id|title|item1,item2,item3"
        // Sanitize delimiters to avoid parsing issues
        let safe_title = sanitize_payload_field(&input.title);
        let safe_items: Vec<String> = items.iter().map(|s| sanitize_payload_field(s)).collect();
        let payload = format!("{}|{}|{}", request_id, safe_title, safe_items.join(","));

        tracing::debug!(
            request_id = %request_id,
            plugin = %plugin_path,
            session = %session,
            items = ?items,
            "Sending popup request via zellij pipe --plugin"
        );

        // Use zellij pipe --plugin to send request and receive response on stdout.
        // The plugin will call cli_pipe_output() to send the response back.
        // We must specify --session explicitly because the MCP server process may not
        // have ZELLIJ_SESSION_NAME in its environment.
        let mut child = Command::new("zellij")
            .arg("--session")
            .arg(&session)
            .arg("pipe")
            .arg("--plugin")
            .arg(&plugin_path)
            .arg("--name")
            .arg(transport::POPUP_PIPE)
            .arg("--")
            .arg(&payload)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to spawn zellij pipe command")?;

        let (tx, rx) = mpsc::channel();
        let mut stdout = child.stdout.take().expect("stdout was piped");
        let mut stderr = child.stderr.take().expect("stderr was piped");

        // Spawn thread to read stdout (blocks until plugin sends response)
        let stderr_handle = std::thread::spawn(move || {
            let mut buffer = Vec::new();
            let _ = stderr.read_to_end(&mut buffer);
            buffer
        });

        std::thread::spawn(move || {
            let mut buffer = Vec::new();
            let result = stdout.read_to_end(&mut buffer);
            let _ = tx.send((result, buffer));
        });

        tracing::debug!(request_id = %request_id, "Waiting for popup response (timeout: {:?})", POPUP_TIMEOUT);

        // Wait for response with timeout
        let response_str = match rx.recv_timeout(POPUP_TIMEOUT) {
            Ok((Ok(_), buffer)) => {
                let _ = child.wait();
                // Capture stderr for debugging
                if let Ok(stderr_buf) = stderr_handle.join() {
                    if !stderr_buf.is_empty() {
                        let stderr_str = String::from_utf8_lossy(&stderr_buf);
                        tracing::warn!(request_id = %request_id, stderr = %stderr_str, "zellij pipe stderr");
                    }
                }
                String::from_utf8(buffer).context("Invalid UTF-8 in popup response")?
            }
            Ok((Err(e), _)) => {
                let _ = child.kill();
                let _ = child.wait();
                // Capture stderr for debugging
                if let Ok(stderr_buf) = stderr_handle.join() {
                    if !stderr_buf.is_empty() {
                        let stderr_str = String::from_utf8_lossy(&stderr_buf);
                        tracing::error!(request_id = %request_id, stderr = %stderr_str, "zellij pipe failed");
                    }
                }
                return Err(anyhow::Error::from(e).context("Failed to read popup response"));
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {
                tracing::warn!(request_id = %request_id, "Popup timed out after {:?}", POPUP_TIMEOUT);
                let _ = child.kill();
                let _ = child.wait();
                // Join stderr thread to avoid orphaned thread
                let _ = stderr_handle.join();
                anyhow::bail!("Popup timed out after {} seconds", POPUP_TIMEOUT.as_secs());
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                let _ = child.kill();
                let _ = child.wait();
                // Join stderr thread to avoid orphaned thread
                let _ = stderr_handle.join();
                anyhow::bail!("Popup response channel disconnected unexpectedly");
            }
        };

        tracing::info!(
            request_id = %request_id,
            response = %response_str,
            response_len = response_str.len(),
            response_bytes = ?response_str.as_bytes(),
            "Received popup response"
        );

        // Parse response: "{request_id}:{selected_item}" or "{request_id}:CANCELLED"
        let response_str = response_str.trim();

        if response_str.is_empty() {
            anyhow::bail!(
                "Empty response from zellij pipe - plugin may have failed to load or respond"
            );
        }

        let (resp_request_id, selection) = response_str.split_once(':').context(format!(
            "Invalid popup response format: expected 'request_id:selection', got: {:?}",
            response_str
        ))?;

        // Verify request_id matches
        if resp_request_id != request_id {
            tracing::warn!(
                expected = %request_id,
                received = %resp_request_id,
                "Request ID mismatch in popup response"
            );
        }

        let (button, values) = if selection == "CANCELLED" {
            ("cancel".to_string(), serde_json::json!({}))
        } else {
            (
                "submit".to_string(),
                serde_json::json!({"selected": selection}),
            )
        };

        tracing::info!(
            request_id = %request_id,
            button = %button,
            "Popup completed"
        );

        Ok(PopupOutput { button, values })
    }
}

/// Sanitize a string for use in the payload protocol.
///
/// Replaces delimiter characters (`|` and `,`) with safe alternatives
/// to prevent parsing issues in the plugin.
fn sanitize_payload_field(s: &str) -> String {
    s.replace('|', "-").replace(',', ";")
}

/// Extract choice items from popup components.
///
/// Looks for Choice components and extracts their options.
/// Falls back to text content from Text components if no Choice found.
fn extract_choice_items(components: &[serde_json::Value]) -> Vec<String> {
    for component in components {
        // Look for Choice component with options
        if component.get("type").and_then(|t| t.as_str()) == Some("choice") {
            if let Some(options) = component.get("options").and_then(|o| o.as_array()) {
                return options
                    .iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect();
            }
        }
    }

    // Fallback: collect text content from Text components as items
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
            components: vec![serde_json::json!({
                "type": "text",
                "id": "msg",
                "content": "Hello"
            })],
        };

        let json = serde_json::to_string(&input).unwrap();
        let parsed: PopupInput = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.title, "Test");
        assert_eq!(parsed.components.len(), 1);
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
        // Should extract from choice component, not text
        assert_eq!(items, vec!["A", "B"]);
    }

    #[test]
    fn test_sanitize_payload_field() {
        // Pipes replaced with dashes
        assert_eq!(sanitize_payload_field("Select | Insert"), "Select - Insert");
        // Commas replaced with semicolons
        assert_eq!(sanitize_payload_field("A, B, C"), "A; B; C");
        // Both
        assert_eq!(sanitize_payload_field("X|Y,Z"), "X-Y;Z");
        // Clean string unchanged
        assert_eq!(sanitize_payload_field("Normal Text"), "Normal Text");
    }
}
