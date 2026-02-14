//! Zellij event emission service.
//!
//! Broadcasts agent lifecycle events to the Zellij plugin sidebar via pipe.
//! Uses tokio::spawn for non-blocking fire-and-forget with timeout.

use crate::ui_protocol::{transport, AgentEvent};
use anyhow::{Context, Result};
use std::time::Duration;
use tokio::process::Command;
use tracing::{debug, info, warn};

const PIPE_TIMEOUT: Duration = Duration::from_secs(5);

/// Emit an agent event to the Zellij plugin sidebar via pipe.
///
/// Non-blocking: spawns a tokio task that handles the subprocess with timeout.
/// Returns immediately. Errors are logged, not propagated.
///
/// # Arguments
/// * `session` - The Zellij session name to target (from config, not env var)
/// * `event` - The agent event to emit
pub fn emit_event(session: &str, event: &AgentEvent) -> Result<()> {
    let json = serde_json::to_string(event).context("Failed to serialize event")?;
    let plugin_path = crate::layout::resolve_plugin_path()
        .context("Zellij plugin not found. Run 'just install-all' to install it.")?;

    debug!(
        "[ZellijEvents] Spawning async emit to session {}: {}",
        session, json
    );

    // Fire and forget: spawn task, don't await
    let session_owned = session.to_string();
    tokio::spawn(emit_with_timeout(session_owned, plugin_path, json));

    Ok(())
}

async fn emit_with_timeout(session: String, plugin_path: String, json: String) {
    let child_result = Command::new("zellij")
        .arg("--session")
        .arg(&session)
        .args([
            "pipe",
            "--plugin",
            &plugin_path,
            "--name",
            "exomonad-events",
            "--",
            &json,
        ])
        .spawn();

    let mut child = match child_result {
        Ok(c) => c,
        Err(e) => {
            warn!("[ZellijEvents] Failed to spawn zellij pipe: {}", e);
            return;
        }
    };

    match tokio::time::timeout(PIPE_TIMEOUT, child.wait()).await {
        Ok(Ok(status)) => {
            if !status.success() {
                warn!("[ZellijEvents] zellij pipe exited with status: {}", status);
            }
        }
        Ok(Err(e)) => {
            warn!("[ZellijEvents] zellij pipe wait error: {}", e);
        }
        Err(_) => {
            warn!(
                "[ZellijEvents] zellij pipe timed out after {:?}, killing",
                PIPE_TIMEOUT
            );
            let _ = child.kill().await;
        }
    }
}

/// Inject text into a target pane via the Zellij plugin.
///
/// Sends a JSON payload to the plugin via `zellij pipe`, which resolves
/// the target pane from the tab name and writes the text as stdin.
///
/// Fire-and-forget: errors are logged, not propagated.
pub fn inject_input(tab_name: &str, text: &str) {
    let plugin_path = match crate::layout::resolve_plugin_path() {
        Some(p) => p,
        None => {
            warn!("[ZellijEvents] Plugin not found, skipping inject_input");
            return;
        }
    };

    let session = match std::env::var("ZELLIJ_SESSION_NAME") {
        Ok(s) => s,
        Err(_) => {
            warn!("[ZellijEvents] ZELLIJ_SESSION_NAME not set, skipping inject_input");
            return;
        }
    };

    let payload = serde_json::json!({
        "tab_name": tab_name,
        "text": text,
    })
    .to_string();

    info!(
        "[ZellijEvents] Injecting input to tab '{}': {} chars",
        tab_name,
        text.len()
    );

    tokio::spawn(inject_with_timeout(session, plugin_path, payload));
}

async fn inject_with_timeout(session: String, plugin_path: String, payload: String) {
    let child_result = Command::new("zellij")
        .arg("--session")
        .arg(&session)
        .args([
            "pipe",
            "--plugin",
            &plugin_path,
            "--name",
            transport::INJECT_INPUT_PIPE,
            "--",
            &payload,
        ])
        .spawn();

    let mut child = match child_result {
        Ok(c) => c,
        Err(e) => {
            warn!(
                "[ZellijEvents] Failed to spawn zellij pipe for inject: {}",
                e
            );
            return;
        }
    };

    match tokio::time::timeout(PIPE_TIMEOUT, child.wait()).await {
        Ok(Ok(status)) => {
            if !status.success() {
                warn!(
                    "[ZellijEvents] zellij pipe inject exited with status: {}",
                    status
                );
            }
        }
        Ok(Err(e)) => {
            warn!("[ZellijEvents] zellij pipe inject wait error: {}", e);
        }
        Err(_) => {
            warn!(
                "[ZellijEvents] zellij pipe inject timed out after {:?}, killing",
                PIPE_TIMEOUT
            );
            let _ = child.kill().await;
        }
    }
}

/// Helper to get current timestamp in ISO 8601 format.
pub fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}
