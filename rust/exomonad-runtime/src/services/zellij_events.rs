//! Zellij event emission service.
//!
//! Broadcasts agent lifecycle events to the Zellij plugin sidebar via pipe.

use anyhow::{Context, Result};
use duct::cmd;
use exomonad_ui_protocol::AgentEvent;
use tracing::debug;

/// Emit an agent event to the Zellij plugin sidebar via pipe.
///
/// Events are broadcast to all plugins subscribed to the `exomonad-events` pipe.
/// The sidebar plugin will render these events in real-time.
pub fn emit_event(event: &AgentEvent) -> Result<()> {
    // Check if we're in a Zellij session
    if std::env::var("ZELLIJ_SESSION_NAME").is_err() {
        debug!("Not in Zellij session, skipping event emission");
        return Ok(());
    }

    let json = serde_json::to_string(event).context("Failed to serialize event")?;

    debug!("[ZellijEvents] Emitting event: {}", json);

    // Use --plugin flag to auto-launch the plugin if not running
    // This prevents hanging when no plugin is subscribed to the pipe
    let plugin_path = format!("file:{}",
        std::env::var("HOME")
            .unwrap_or_else(|_| "/Users/inannamalick".to_string())
        + "/.config/zellij/plugins/exomonad-plugin.wasm"
    );

    cmd!(
        "zellij", "pipe",
        "--plugin", &plugin_path,
        "--name", "exomonad-events",
        "--", &json
    )
    .start()
    .context("Failed to spawn zellij pipe")?;

    Ok(())
}

/// Helper to get current timestamp in ISO 8601 format.
pub fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}
