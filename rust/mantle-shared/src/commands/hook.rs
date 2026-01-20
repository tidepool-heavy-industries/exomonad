//! Hook command implementation.
//!
//! Handles Claude Code hook events by forwarding them to the control socket
//! and returning the response. This enables Tidepool orchestration to intercept
//! and control Claude's tool usage.

use crate::error::{MantleError, Result};
use crate::protocol::{
    ControlMessage, ControlResponse, HookInput, HookOutput, HookSpecificOutput, PermissionDecision,
};
use crate::socket::{control_socket_path, ControlSocket};
use clap::ValueEnum;
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

/// Handle a hook event from Claude Code.
///
/// This function:
/// 1. Reads the hook payload JSON from stdin (provided by Claude Code)
/// 2. Connects to the control server via TCP
/// 3. Sends the hook event and waits for response
/// 4. Outputs the response JSON to stdout
/// 5. Returns the exit code (0=allow, 2=deny/error)
///
/// If no control server is available, we "fail open" - allow the hook
/// to proceed without orchestration. This ensures Claude Code still works
/// when not running under Tidepool control.
///
/// # Arguments
///
/// * `event_type` - The type of hook event being handled
///
/// # Returns
///
/// Returns `Ok(())` on success. The function may call `std::process::exit()`
/// with a non-zero code if the hook should be denied.
pub fn handle_hook(event_type: HookEventType) -> Result<()> {
    // Read hook payload from stdin
    let mut stdin_content = String::new();
    std::io::stdin()
        .read_to_string(&mut stdin_content)
        .map_err(MantleError::Io)?;

    debug!(
        event = ?event_type,
        payload_len = stdin_content.len(),
        "Received hook event"
    );

    // Parse the hook input
    let hook_input: HookInput = serde_json::from_str(&stdin_content)?;

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
    let path = control_socket_path();

    // Connect to control server via Unix socket
    let mut socket = match ControlSocket::connect(&path) {
        Ok(s) => s,
        Err(e) => {
            return handle_server_unavailable(
                event_type,
                &format!("Failed to connect to control server at {}: {}", path.display(), e),
            );
        }
    };

    // Send hook event to orchestrator (Haskell)
    let message = ControlMessage::HookEvent {
        input: Box::new(hook_input),
    };
    let response = socket.send(&message)?;

    // Handle response
    match response {
        ControlResponse::HookResponse { output, exit_code } => {
            // Output the response JSON for Claude Code
            println!(
                "{}",
                serde_json::to_string(&output).map_err(MantleError::JsonSerialize)?
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
/// - MANTLE_FAIL_MODE environment variable ("closed" vs "open")
/// - Proper monitoring/alerting when falling back to fail-open
/// - Graceful degradation logic (default_allow_response)
fn handle_server_unavailable(_event_type: HookEventType, reason: &str) -> Result<()> {
    // Always fail closed during development
    error!(reason, "Control server unavailable (fail-closed)");
    eprintln!("ERROR: {}", reason);
    eprintln!("Control server required.");
    eprintln!("Ensure control server is running and TIDEPOOL_CONTROL_SOCKET is correct.");
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
