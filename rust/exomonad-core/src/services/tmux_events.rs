//! tmux event emission and input injection service.
//!
//! Replaces the former Zellij event system. Events are log-only (no sidebar).
//! Input injection uses tmux buffer pattern for multiline safety.

use crate::ui_protocol::AgentEvent;
use anyhow::{Context, Result};
use tracing::{info, warn};

use super::tmux_ipc::TmuxIpc;

/// Emit an agent event. Log-only — no tmux interaction.
/// Keeps function signature for callers.
pub fn emit_event(_session: &str, event: &AgentEvent) -> Result<()> {
    let json = serde_json::to_string(event).context("Failed to serialize event")?;
    info!("[TmuxEvents] Event: {}", json);
    Ok(())
}

/// Inject text into a target pane/window via tmux buffer pattern.
///
/// `target` is a tmux target specifier (pane_id like %N, or window name).
/// `_pane_name` is kept for API compatibility but tmux uses direct pane IDs.
pub fn inject_input(target: &str, _pane_name: Option<&str>, text: &str) {
    let session = match std::env::var("EXOMONAD_TMUX_SESSION") {
        Ok(s) => s,
        Err(_) => {
            warn!("[TmuxEvents] EXOMONAD_TMUX_SESSION not set, skipping inject_input");
            return;
        }
    };

    info!(
        "[TmuxEvents] Injecting input to target '{}': {} chars",
        target,
        text.len()
    );

    let ipc = TmuxIpc::new(&session);
    let target = target.to_string();
    let text = text.to_string();

    tokio::spawn(async move {
        let result = tokio::task::spawn_blocking(move || {
            ipc.inject_input(&target, &text)
        })
        .await;

        match result {
            Ok(Ok(())) => {}
            Ok(Err(e)) => warn!("[TmuxEvents] inject_input failed: {}", e),
            Err(e) => warn!("[TmuxEvents] spawn_blocking join error: {}", e),
        }
    });
}

/// Close a worker pane by pane_id.
pub fn close_worker_pane(pane_id: &str) {
    let session = match std::env::var("EXOMONAD_TMUX_SESSION") {
        Ok(s) => s,
        Err(_) => {
            warn!("[TmuxEvents] EXOMONAD_TMUX_SESSION not set, skipping close_worker_pane");
            return;
        }
    };

    info!("[TmuxEvents] Closing worker pane '{}'", pane_id);

    let ipc = TmuxIpc::new(&session);
    let pane = pane_id.to_string();

    tokio::spawn(async move {
        let result = tokio::task::spawn_blocking(move || ipc.kill_pane(&pane)).await;
        match result {
            Ok(Ok(())) => {}
            Ok(Err(e)) => warn!("[TmuxEvents] kill_pane failed: {}", e),
            Err(e) => warn!("[TmuxEvents] spawn_blocking join error: {}", e),
        }
    });
}

/// Helper to get current timestamp in ISO 8601 format.
pub fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}
