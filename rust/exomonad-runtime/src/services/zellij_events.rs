//! Zellij event emission service.
//!
//! Broadcasts agent lifecycle events to the Zellij plugin sidebar via pipe.
//! Uses tokio::spawn for non-blocking fire-and-forget with timeout.

use anyhow::{Context, Result};
use exomonad_ui_protocol::AgentEvent;
use std::time::Duration;
use tokio::process::Command;
use tracing::{debug, warn};

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
    let plugin_path = format!(
        "file:{}/.config/zellij/plugins/exomonad-plugin.wasm",
        std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string())
    );

    debug!("[ZellijEvents] Spawning async emit to session {}: {}", session, json);

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

/// Helper to get current timestamp in ISO 8601 format.
pub fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}
