//! tmux event emission and input injection service.
//!
//! Replaces the original event system. Events are log-only (no sidebar).
//! Input injection uses tmux buffer pattern for multiline safety.

use crate::ui_protocol::AgentEvent;
use anyhow::{Context, Result};
use tracing::info;

use super::tmux_ipc::{PaneId, TmuxIpc};

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
pub async fn inject_input(target: &str, text: &str) -> Result<()> {
    let session =
        std::env::var("EXOMONAD_TMUX_SESSION").context("EXOMONAD_TMUX_SESSION not set")?;

    info!(
        "[TmuxEvents] Injecting input to target '{}': {} chars",
        target,
        text.len()
    );

    let ipc = TmuxIpc::new(&session);
    let target = target.to_string();
    let text = text.to_string();

    tokio::task::spawn_blocking(move || ipc.inject_input(&target, &text))
        .await
        .context("spawn_blocking join error")?
}

/// Close a worker pane by pane_id.
pub async fn close_worker_pane(pane_id: &str) -> Result<()> {
    let session =
        std::env::var("EXOMONAD_TMUX_SESSION").context("EXOMONAD_TMUX_SESSION not set")?;

    info!("[TmuxEvents] Closing worker pane '{}'", pane_id);

    let pid = PaneId::parse(pane_id).context("Invalid pane_id")?;
    let ipc = TmuxIpc::new(&session);

    tokio::task::spawn_blocking(move || ipc.kill_pane(&pid))
        .await
        .context("spawn_blocking join error")?
}

/// Helper to get current timestamp in ISO 8601 format.
pub fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}
