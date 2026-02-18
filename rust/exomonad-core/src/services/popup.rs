//! Popup service for WASM host functions.
//!
//! Shows interactive popup forms and wizards via Zellij CLI pipes and returns user response.
//!
//! Uses the Zellij CLI pipe mechanism:
//! 1. `zellij pipe --plugin file:plugin.wasm --name exomonad:popup -- "{JSON payload}"`
//! 2. Plugin receives via `pipe()` with `PipeSource::Cli(pipe_id)` and payload
//! 3. Plugin blocks CLI, shows popup UI
//! 4. On submit, plugin calls `cli_pipe_output(&pipe_id, "{JSON response}")`
//! 5. CLI receives response on stdout

use crate::layout::resolve_plugin_path;
use crate::ui_protocol::transport;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::io::Read;
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

/// Timeout for waiting for popup response (30 minutes).
/// Generous because the user may be on a different tab/pane when the popup appears.
const POPUP_TIMEOUT: Duration = Duration::from_secs(1800);

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
        tracing::info!(title = %input.title, "Showing popup");

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

        // Determine mode: wizard (object with "panes") or form (array of components)
        let payload = if input.raw_json.get("panes").is_some() {
            // Wizard mode: wrap in WizardRequest envelope
            tracing::info!(request_id = %request_id, "[Popup] Wizard mode detected");
            let wizard_def: crate::ui_protocol::WizardDefinition =
                serde_json::from_value(input.raw_json.clone())
                    .context("Failed to parse wizard definition")?;
            let request = crate::ui_protocol::WizardRequest {
                request_id: request_id.clone(),
                wizard: wizard_def,
            };
            serde_json::to_string(&request).context("Failed to serialize WizardRequest")?
        } else if let Some(components_array) = input.raw_json.as_array() {
            // Form mode: try typed component parsing
            let components_res: Result<Vec<crate::ui_protocol::Component>, _> =
                serde_json::from_value(serde_json::Value::Array(components_array.clone()));

            if let Ok(components) = components_res {
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
                tracing::info!(request_id = %request_id, "[Popup] JSON parse failed, using legacy payload path");
                let items = extract_choice_items(components_array);
                if items.is_empty() {
                    anyhow::bail!("Popup must have at least one choice item for Zellij display");
                }

                let safe_title = sanitize_payload_field(&input.title);
                let safe_items: Vec<String> = items.iter().map(|s| sanitize_payload_field(s)).collect();
                format!("{}|{}|{}", request_id, safe_title, safe_items.join(","))
            }
        } else {
            anyhow::bail!("Invalid popup input: expected array of components or wizard object");
        };

        // Inject target_tab routing into JSON payloads so only the correct plugin instance renders.
        let payload = if let Some(target_tab) = &input.target_tab {
            if payload.starts_with('{') {
                let mut json: serde_json::Value = serde_json::from_str(&payload)
                    .context("Failed to re-parse payload for target_tab injection")?;
                json["target_tab"] = serde_json::Value::String(target_tab.clone());
                serde_json::to_string(&json).context("Failed to re-serialize payload")?
            } else {
                payload
            }
        } else {
            payload
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

        let response_str = response_str.trim();

        if response_str.is_empty() {
            anyhow::bail!(
                "Empty response from zellij pipe - plugin may have failed to load or respond"
            );
        }

        // Parse response: try JSON first (new format), then legacy "request_id:selection"
        if response_str.starts_with('{') {
            let json: serde_json::Value = serde_json::from_str(response_str)
                .context("Failed to parse JSON popup response")?;

            let resp_request_id = json["request_id"].as_str().unwrap_or("");
            if resp_request_id != request_id {
                tracing::warn!(expected = %request_id, received = %resp_request_id, "Request ID mismatch");
            }

            let result = &json["result"];
            let button = result["button"].as_str().unwrap_or("submit").to_string();
            let values = result["values"].clone();

            // For wizard results, include panes_visited in the values
            if let Some(panes_visited) = result.get("panes_visited") {
                let mut combined = serde_json::Map::new();
                combined.insert("values".to_string(), values);
                combined.insert("panes_visited".to_string(), panes_visited.clone());
                return Ok(PopupOutput {
                    button,
                    values: serde_json::Value::Object(combined),
                });
            }

            return Ok(PopupOutput { button, values });
        }

        // Legacy format: "request_id:selection" or "request_id:CANCELLED"
        let (resp_request_id, selection) = response_str.split_once(':').context(format!(
            "Invalid popup response format: expected 'request_id:selection', got: {:?}",
            response_str
        ))?;

        if resp_request_id != request_id {
            tracing::warn!(expected = %request_id, received = %resp_request_id, "Request ID mismatch");
        }

        let (button, values) = if selection == "CANCELLED" {
            ("cancelled".to_string(), serde_json::json!({}))
        } else {
            (
                "submit".to_string(),
                serde_json::json!({"selected": selection}),
            )
        };

        Ok(PopupOutput { button, values })
    }
}

/// Sanitize a string for use in the legacy payload protocol.
fn sanitize_payload_field(s: &str) -> String {
    s.replace('|', "-").replace(',', ";")
}

/// Extract choice items from popup components (legacy fallback).
fn extract_choice_items(components: &[serde_json::Value]) -> Vec<String> {
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
