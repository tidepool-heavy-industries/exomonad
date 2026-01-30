//! Hook command implementation.
//!
//! Handles Claude Code hook events by forwarding them to the control socket
//! and returning the response. This enables ExoMonad orchestration to intercept
//! and control Claude's tool usage.

use crate::error::{ExoMonadError, Result};
use crate::protocol::{
    ControlMessage, ControlResponse, HookInput, HookOutput, HookSpecificOutput, PermissionDecision,
    Role, Runtime,
};
use crate::socket::{control_socket_path, ControlSocket};
use clap::ValueEnum;
use serde_json::{json, Value};
use std::io::Read;
use tracing::{debug, error, warn};

/// Hook event types supported by Claude Code.
///
/// Each variant corresponds to a hook event that Claude Code emits.
/// See: <https://docs.anthropic.com/en/docs/claude-code/hooks>
#[derive(Debug, Clone, Copy, ValueEnum, strum::Display)]
pub enum HookEventType {
    /// Before tool execution (can allow/deny/modify)
    PreToolUse,
    /// After tool completion
    PostToolUse,
    /// When a notification is shown
    Notification,
    /// When Claude Code wants to stop
    Stop,
    /// When a subagent (Task tool) starts
    SubagentStart,
    /// When a subagent (Task tool) finishes
    SubagentStop,
    /// Before a compact operation
    PreCompact,
    /// When a session starts or resumes
    SessionStart,
    /// When a session ends
    SessionEnd,
    /// When permission dialog is shown
    PermissionRequest,
    /// When user submits a prompt
    UserPromptSubmit,
}

/// Normalize Gemini CLI hook payloads to Claude Code format.
///
/// Gemini CLI uses different field names than Claude Code:
/// - `hook_event_name: "BeforeTool"` or `"BeforeToolSelection"` → `"PreToolUse"`
/// - `tool_parameters` → `tool_input`
///
/// This function parses the JSON, applies normalization, and returns the normalized JSON string.
fn normalize_gemini_payload(payload: &str) -> Result<String> {
    let mut value: Value = serde_json::from_str(payload)?;

    if let Some(obj) = value.as_object_mut() {
        // Normalize hook_event_name
        let event_name = obj
            .get("hook_event_name")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());
        if let Some(event_name) = event_name {
            let normalized_name = match event_name.as_str() {
                "BeforeTool" | "BeforeToolSelection" => "PreToolUse",
                // Other event names pass through unchanged
                _ => &event_name,
            };
            if normalized_name != event_name.as_str() {
                obj.insert("hook_event_name".to_string(), json!(normalized_name));
                debug!(
                    from = event_name,
                    to = normalized_name,
                    "Normalized hook_event_name"
                );
            }
        }

        // Normalize tool_parameters to tool_input
        if let Some(tool_params) = obj.remove("tool_parameters") {
            // Only use tool_parameters if tool_input doesn't exist
            if !obj.contains_key("tool_input") {
                obj.insert("tool_input".to_string(), tool_params);
                debug!("Copied tool_parameters to tool_input");
            }
        }
    }

    serde_json::to_string(&value).map_err(Into::into)
}

/// Handle a hook event from Claude Code.
///
/// This function:
/// 1. Reads the hook payload JSON from stdin (provided by Claude Code)
/// 2. Normalizes the payload if needed (e.g., for Gemini CLI)
/// 3. Connects to the control server via Unix socket
/// 4. Sends the hook event and waits for response
/// 5. Outputs the response JSON to stdout
/// 6. Returns the exit code (0=allow, 2=deny/error)
///
/// If no control server is available, we "fail open" - allow the hook
/// to proceed without orchestration. This ensures Claude Code still works
/// when not running under ExoMonad control.
///
/// # Arguments
///
/// * `event_type` - The type of hook event being handled
/// * `runtime` - The runtime environment (Claude or Gemini)
/// * `role` - The role of the agent (dev, tl, pm)
///
/// # Returns
///
/// Returns `Ok(())` on success. The function may call `std::process::exit()`
/// with a non-zero code if the hook should be denied.
pub async fn handle_hook(event_type: HookEventType, runtime: Runtime, role: Role) -> Result<()> {
    // Read hook payload from stdin
    let mut stdin_content = String::new();
    std::io::stdin()
        .read_to_string(&mut stdin_content)
        .map_err(ExoMonadError::Io)?;

    debug!(
        event = ?event_type,
        runtime = ?runtime,
        role = ?role,
        payload_len = stdin_content.len(),
        "Received hook event"
    );

    // Normalize payload if needed (e.g., for Gemini CLI)
    let normalized_content = if runtime == Runtime::Gemini {
        debug!("Normalizing Gemini hook payload");
        normalize_gemini_payload(&stdin_content)?
    } else {
        stdin_content
    };

    // Parse the hook input
    let hook_input: HookInput = serde_json::from_str(&normalized_content)?;

    // Derive container ID from EXOMONAD_ISSUE_ID for remote command execution
    let container_id = std::env::var("EXOMONAD_ISSUE_ID")
        .ok()
        .map(|id| format!("exomonad-agent-{}", id));

    // Verify event type matches what Claude Code sent
    let expected_event = event_type.to_string();
    if hook_input.hook_event_name != expected_event {
        warn!(
            expected = %expected_event,
            got = %hook_input.hook_event_name,
            "Hook event name mismatch"
        );
    }

    // Get control server socket path
    let path = match control_socket_path() {
        Ok(p) => p,
        Err(e) => {
            return handle_server_unavailable(
                event_type,
                &format!("Control socket path not available: {}", e),
            );
        }
    };

    // Connect to control server via Unix socket
    let socket = match ControlSocket::connect(&path) {
        Ok(s) => s,
        Err(e) => {
            return handle_server_unavailable(
                event_type,
                &format!(
                    "Failed to connect to control server at {}: {}",
                    path.display(),
                    e
                ),
            );
        }
    };

    // Send hook event to orchestrator (Haskell)
    let message = ControlMessage::HookEvent {
        input: Box::new(hook_input),
        runtime,
        role,
        container_id,
    };

    // Await the response (async)
    let response = socket.send(&message).await?;

    // Handle response
    match response {
        ControlResponse::HookResponse { output, exit_code } => {
            // Output the response JSON for Claude Code
            println!(
                "{}",
                serde_json::to_string(&output).map_err(ExoMonadError::JsonSerialize)?
            );

            // Exit with the code from orchestrator (0=allow, 2=deny)
            if exit_code != 0 {
                std::process::exit(exit_code);
            }
        }
        ControlResponse::McpToolResponse { .. } => {
            // Unexpected response type
            error!("Received MCP response for hook request");
            std::process::exit(1);
        }
        ControlResponse::ToolsListResponse { .. } => {
            // Unexpected response type
            error!("Received ToolsListResponse for hook request");
            std::process::exit(1);
        }
        ControlResponse::Pong => {
            // Unexpected response type
            error!("Received Pong response for hook request");
            std::process::exit(1);
        }
    }

    Ok(())
}

/// Handle control server unavailable - fail closed.
///
/// During development, we want to fail loudly when the control server is unavailable
/// to catch configuration issues early. This prevents silent failures where hooks
/// appear to work but aren't actually being processed.
///
/// TODO: Add configurable fail-open mode for production deployments where Claude Code
/// should continue working even if the control server is down. This would require:
/// - EXOMONAD_FAIL_MODE environment variable ("closed" vs "open")
/// - Proper monitoring/alerting when falling back to fail-open
/// - Graceful degradation logic (default_allow_response)
fn handle_server_unavailable(_event_type: HookEventType, reason: &str) -> Result<()> {
    // Always fail closed during development
    error!(reason, "Control server unavailable (fail-closed)");
    eprintln!("ERROR: {}", reason);
    eprintln!("Control server required.");
    eprintln!("Ensure control server is running and EXOMONAD_CONTROL_SOCKET is correct.");
    std::process::exit(1);
}

/// Create a default "allow" response for when no control socket is available.
///
/// Different hook types require different response structures:
/// - PreToolUse: needs `decision: "allow"`
/// - PostToolUse: needs `decision: "allow"`
/// - PermissionRequest: needs permission decision
/// - Others: just need `continue: true`
pub fn default_allow_response(event_type: HookEventType) -> HookOutput {
    match event_type {
        HookEventType::PreToolUse => HookOutput::pre_tool_use_allow(None, None),
        HookEventType::PostToolUse => HookOutput::post_tool_use_allow(None),
        HookEventType::PermissionRequest => HookOutput {
            continue_: true,
            hook_specific_output: Some(HookSpecificOutput::PermissionRequest {
                decision: PermissionDecision::Allow {
                    updated_input: None,
                },
            }),
            ..Default::default()
        },
        // Other hooks just need continue: true
        _ => HookOutput {
            continue_: true,
            ..Default::default()
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_gemini_before_tool_to_pre_tool_use() {
        let gemini_payload = r#"{
            "session_id": "abc123",
            "hook_event_name": "BeforeTool",
            "tool_name": "Write",
            "tool_parameters": {"file_path": "/tmp/test.txt", "content": "hello"}
        }"#;

        let normalized = normalize_gemini_payload(gemini_payload).unwrap();
        let value: Value = serde_json::from_str(&normalized).unwrap();

        assert_eq!(value["hook_event_name"], "PreToolUse");
        assert_eq!(value["tool_name"], "Write");
    }

    #[test]
    fn test_normalize_gemini_before_tool_selection_to_pre_tool_use() {
        let gemini_payload = r#"{
            "session_id": "abc123",
            "hook_event_name": "BeforeToolSelection",
            "tool_name": "Execute"
        }"#;

        let normalized = normalize_gemini_payload(gemini_payload).unwrap();
        let value: Value = serde_json::from_str(&normalized).unwrap();

        assert_eq!(value["hook_event_name"], "PreToolUse");
    }

    #[test]
    fn test_normalize_gemini_tool_parameters_to_tool_input() {
        let gemini_payload = r#"{
            "session_id": "abc123",
            "hook_event_name": "BeforeTool",
            "tool_parameters": {"file_path": "/tmp/test.txt", "content": "hello"}
        }"#;

        let normalized = normalize_gemini_payload(gemini_payload).unwrap();
        let value: Value = serde_json::from_str(&normalized).unwrap();

        // tool_input should exist with the content from tool_parameters
        assert!(value["tool_input"].is_object());
        assert_eq!(value["tool_input"]["file_path"], "/tmp/test.txt");
        assert_eq!(value["tool_input"]["content"], "hello");

        // tool_parameters should be removed
        assert!(value["tool_parameters"].is_null());
    }

    #[test]
    fn test_normalize_gemini_preserves_existing_tool_input() {
        let gemini_payload = r#"{
            "session_id": "abc123",
            "hook_event_name": "BeforeTool",
            "tool_input": {"file_path": "/tmp/existing.txt"},
            "tool_parameters": {"file_path": "/tmp/ignored.txt"}
        }"#;

        let normalized = normalize_gemini_payload(gemini_payload).unwrap();
        let value: Value = serde_json::from_str(&normalized).unwrap();

        // Existing tool_input should be preserved
        assert_eq!(value["tool_input"]["file_path"], "/tmp/existing.txt");

        // tool_parameters should be removed even though tool_input existed
        assert!(value["tool_parameters"].is_null());
    }

    #[test]
    fn test_normalize_other_hook_events_pass_through() {
        let gemini_payload = r#"{
            "session_id": "abc123",
            "hook_event_name": "SessionStart"
        }"#;

        let normalized = normalize_gemini_payload(gemini_payload).unwrap();
        let value: Value = serde_json::from_str(&normalized).unwrap();

        // Hook name should remain unchanged
        assert_eq!(value["hook_event_name"], "SessionStart");
    }

    #[test]
    fn test_normalize_empty_payload() {
        let empty_payload = "{}";
        let normalized = normalize_gemini_payload(empty_payload).unwrap();
        let value: Value = serde_json::from_str(&normalized).unwrap();

        // Should be valid but empty object
        assert!(value.is_object());
    }

    #[test]
    fn test_normalize_invalid_json_fails() {
        let invalid_json = "{invalid json";
        let result = normalize_gemini_payload(invalid_json);

        // Should fail to parse
        assert!(result.is_err());
    }
}
