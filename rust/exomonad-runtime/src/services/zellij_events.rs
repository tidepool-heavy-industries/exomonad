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
pub fn emit_event(event: &AgentEvent) -> Result<()> {
    // Check if we're in a Zellij session
    if std::env::var("ZELLIJ_SESSION_NAME").is_err() {
        debug!("Not in Zellij session, skipping event emission");
        return Ok(());
    }

    let json = serde_json::to_string(event).context("Failed to serialize event")?;
    let plugin_path = format!(
        "file:{}/.config/zellij/plugins/exomonad-plugin.wasm",
        std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string())
    );

    debug!("[ZellijEvents] Spawning async emit: {}", json);

    // Fire and forget: spawn task, don't await
    tokio::spawn(emit_with_timeout(plugin_path, json));

    Ok(())
}

async fn emit_with_timeout(plugin_path: String, json: String) {
    let child_result = Command::new("zellij")
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
