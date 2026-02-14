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

use crate::layout::resolve_plugin_path;
use crate::ui_protocol::transport;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::io::Read;
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

/// Timeout for waiting for popup response (5 minutes).
const POPUP_TIMEOUT: Duration = Duration::from_secs(300);

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
    /// Zellij session name (optional).
    zellij_session: Option<String>,
}

impl PopupService {
    /// Create a new popup service with the specified Zellij session.
    pub fn new(zellij_session: Option<String>) -> Self {
        Self { zellij_session }
    }

    /// Show a popup and wait for user response.
    ///
    /// Requires a Zellij session — uses the CLI pipe mechanism to communicate
    /// with the pre-loaded exomonad-plugin floating pane.
    #[tracing::instrument(skip(self))]
    pub fn show_popup(&self, input: &PopupInput) -> Result<PopupOutput> {
        tracing::info!(title = %input.title, components = input.components.len(), "Showing popup");

        let zellij_session = self
            .zellij_session
            .clone()
            .or_else(|| std::env::var("ZELLIJ_SESSION_NAME").ok());

        tracing::info!(session = ?zellij_session, configured = ?self.zellij_session, "Popup session resolution");

        let session = zellij_session
            .context("No Zellij session available — popup requires a Zellij session")?;

        self.show_zellij_popup(&session, input)
    }

    /// Show popup via Zellij pipe to exomonad-plugin.
    fn show_zellij_popup(&self, session: &str, input: &PopupInput) -> Result<PopupOutput> {
        let request_id = uuid::Uuid::new_v4().to_string();
        tracing::info!(request_id = %request_id, session = %session, "[Popup] Starting zellij popup flow");

        let components_res: Result<Vec<crate::ui_protocol::Component>, _> =
            serde_json::from_value(serde_json::Value::Array(input.components.clone()));

        let payload = if let Ok(components) = components_res {
            tracing::info!(request_id = %request_id, count = components.len(), "[Popup] Using JSON payload path");
            let request = crate::ui_protocol::PopupRequest {
                request_id: request_id.clone(),
                definition: crate::ui_protocol::PopupDefinition {
                    title: input.title.clone(),
                    components,
                },
            };
            serde_json::to_string(&request).context("Failed to serialize PopupRequest")?
        } else {
            tracing::info!(request_id = %request_id, err = ?components_res.unwrap_err(), "[Popup] JSON parse failed, using legacy payload path");
            let items = extract_choice_items(&input.components);
            if items.is_empty() {
                anyhow::bail!("Popup must have at least one choice item for Zellij display");
            }

            let safe_title = sanitize_payload_field(&input.title);
            let safe_items: Vec<String> = items.iter().map(|s| sanitize_payload_field(s)).collect();
            format!("{}|{}|{}", request_id, safe_title, safe_items.join(","))
        };

        tracing::info!(request_id = %request_id, payload_len = payload.len(), "[Popup] Payload built: {}", &payload[..payload.len().min(200)]);

        let plugin_path = match resolve_plugin_path() {
            Some(p) => {
                tracing::info!(request_id = %request_id, path = %p, "[Popup] Plugin path resolved");
                p
            }
            None => {
                tracing::error!(request_id = %request_id, "[Popup] Plugin WASM not found!");
                anyhow::bail!("Zellij plugin not found. Run 'just install-all' to install it.");
            }
        };

        let cmd_args = vec![
            "--session".to_string(),
            session.to_string(),
            "pipe".to_string(),
            "--plugin".to_string(),
            plugin_path.clone(),
            "--name".to_string(),
            transport::POPUP_PIPE.to_string(),
            "--".to_string(),
            payload.clone(),
        ];
        tracing::info!(request_id = %request_id, "[Popup] Spawning: zellij {}", cmd_args.join(" "));

        let mut child = Command::new("zellij")
            .arg("--session")
            .arg(session)
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

        let child_id = child.id();
        tracing::info!(request_id = %request_id, pid = child_id, "[Popup] zellij pipe spawned successfully");

        let stdout = child.stdout.take().expect("stdout was piped");
        let mut stderr = child.stderr.take().expect("stderr was piped");

        // Read stderr in background (diagnostic only)
        let req_id_stderr = request_id.clone();
        let stderr_handle = std::thread::spawn(move || {
            let mut buffer = Vec::new();
            let _ = stderr.read_to_end(&mut buffer);
            if !buffer.is_empty() {
                tracing::info!(request_id = %req_id_stderr, stderr = %String::from_utf8_lossy(&buffer), "[Popup] zellij pipe stderr");
            }
        });

        // Read exactly one line from stdout. The plugin sends a single response
        // line via cli_pipe_output, so we don't need to wait for process exit.
        // Using read_to_end would block until the zellij pipe process exits,
        // which may not happen promptly after unblock_cli_pipe_input.
        let (tx, rx) = mpsc::channel();
        let req_id_stdout = request_id.clone();
        std::thread::spawn(move || {
            use std::io::BufRead;
            let mut reader = std::io::BufReader::new(stdout);
            let mut line = String::new();
            let result = reader.read_line(&mut line);
            tracing::info!(request_id = %req_id_stdout, bytes = line.len(), ok = result.is_ok(), "[Popup] stdout line read complete");
            let _ = tx.send(result.map(|_| line));
        });

        tracing::info!(request_id = %request_id, timeout_secs = POPUP_TIMEOUT.as_secs(), "[Popup] Waiting for response...");

        // Wait for the single response line with timeout
        let response_str = match rx.recv_timeout(POPUP_TIMEOUT) {
            Ok(Ok(line)) => {
                tracing::info!(request_id = %request_id, bytes = line.len(), "[Popup] Got response");
                // Kill the pipe process — we have what we need
                let _ = child.kill();
                let _ = child.wait();
                let _ = stderr_handle.join();
                line
            }
            Ok(Err(e)) => {
                tracing::error!(request_id = %request_id, err = %e, "[Popup] stdout read failed");
                let _ = child.kill();
                let _ = child.wait();
                return Err(anyhow::Error::from(e).context("Failed to read popup response"));
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {
                tracing::error!(request_id = %request_id, timeout_secs = POPUP_TIMEOUT.as_secs(), pid = child_id, "[Popup] TIMED OUT — killing zellij pipe process");
                let _ = child.kill();
                let _ = child.wait();
                anyhow::bail!("Popup timed out after {} seconds", POPUP_TIMEOUT.as_secs());
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                tracing::error!(request_id = %request_id, "[Popup] Channel disconnected — stdout thread died");
                let _ = child.kill();
                let _ = child.wait();
                anyhow::bail!("Popup response channel disconnected unexpectedly");
            }
        };

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

        if resp_request_id != request_id {
            tracing::warn!(expected = %request_id, received = %resp_request_id, "Request ID mismatch");
        }

        let (button, values) = if selection == "CANCELLED" {
            ("cancel".to_string(), serde_json::json!({}))
        } else {
            (
                "submit".to_string(),
                serde_json::json!({"selected": selection}),
            )
        };

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
