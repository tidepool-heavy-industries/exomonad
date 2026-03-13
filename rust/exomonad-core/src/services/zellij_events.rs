//! Zellij event emission service.
//!
//! Broadcasts agent lifecycle events to the Zellij plugin sidebar via direct IPC.
//! Uses tokio::spawn_blocking for non-blocking fire-and-forget.

use crate::ui_protocol::{transport, AgentEvent};
use anyhow::{Context, Result};
use tracing::{debug, info, warn};

use super::zellij_ipc::ZellijIpc;

/// Emit an agent event to the Zellij plugin sidebar via direct IPC.
///
/// Non-blocking: spawns a blocking task for the synchronous socket write.
/// Returns immediately. Errors are logged, not propagated.
pub fn emit_event(session: &str, event: &AgentEvent) -> Result<()> {
    let json = serde_json::to_string(event).context("Failed to serialize event")?;
    let plugin_path = crate::layout::resolve_plugin_path()
        .context("Zellij plugin not found. Run 'just install-all' to install it.")?;

    debug!(
        "[ZellijEvents] Emitting event to session {}: {}",
        session, json
    );

    let ipc = ZellijIpc::new(session);
    let pipe_name = "exomonad-events".to_string();

    tokio::spawn(async move {
        let result = tokio::task::spawn_blocking(move || {
            ipc.pipe_to_plugin(&plugin_path, &pipe_name, &json)
        })
        .await;

        match result {
            Ok(Ok(())) => {}
            Ok(Err(e)) => warn!("[ZellijEvents] pipe_to_plugin failed: {}", e),
            Err(e) => warn!("[ZellijEvents] spawn_blocking join error: {}", e),
        }
    });

    Ok(())
}

/// Inject text into a target pane via the Zellij plugin.
///
/// `tab_name` identifies which plugin instance handles the message.
/// `pane_name` optionally targets a specific named pane within the tab
/// (used for worker panes that share a tab with the TL agent).
/// Falls back to the first terminal pane in the tab when `pane_name` is None.
///
/// Fire-and-forget: errors are logged, not propagated.
pub fn inject_input(tab_name: &str, pane_name: Option<&str>, text: &str) {
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

    let mut payload_obj = serde_json::json!({
        "tab_name": tab_name,
        "text": text,
    });
    if let Some(pname) = pane_name {
        payload_obj["pane_name"] = serde_json::Value::String(pname.to_string());
    }
    let payload = payload_obj.to_string();

    info!(
        "[ZellijEvents] Injecting input to tab '{}' pane {:?}: {} chars",
        tab_name,
        pane_name,
        text.len()
    );

    let ipc = ZellijIpc::new(&session);
    let pipe_name = transport::INJECT_INPUT_PIPE.to_string();

    tokio::spawn(async move {
        let result = tokio::task::spawn_blocking(move || {
            ipc.pipe_to_plugin(&plugin_path, &pipe_name, &payload)
        })
        .await;

        match result {
            Ok(Ok(())) => {}
            Ok(Err(e)) => warn!("[ZellijEvents] inject pipe_to_plugin failed: {}", e),
            Err(e) => warn!("[ZellijEvents] inject spawn_blocking join error: {}", e),
        }
    });
}

/// Register a stable slug→pane_id mapping in the plugin.
///
/// Sent immediately after `new_zellij_pane` so the plugin can resolve the pane
/// by slug key even after Gemini CLI renames its pane title. The plugin maps
/// `slug_key → pane_id` once at spawn time and uses it forever.
///
/// Fire-and-forget: errors are logged, not propagated.
pub fn register_worker_pane(slug_key: &str, display_name: &str) {
    let plugin_path = match crate::layout::resolve_plugin_path() {
        Some(p) => p,
        None => {
            warn!("[ZellijEvents] Plugin not found, skipping register_worker_pane");
            return;
        }
    };

    let session = match std::env::var("ZELLIJ_SESSION_NAME") {
        Ok(s) => s,
        Err(_) => {
            warn!("[ZellijEvents] ZELLIJ_SESSION_NAME not set, skipping register_worker_pane");
            return;
        }
    };

    let payload = serde_json::json!({
        "slug_key": slug_key,
        "display_name": display_name,
    })
    .to_string();

    info!(
        "[ZellijEvents] Registering worker pane slug '{}' with display_name '{}'",
        slug_key, display_name
    );

    let ipc = ZellijIpc::new(&session);
    let pipe_name = transport::REGISTER_PANE_PIPE.to_string();

    tokio::spawn(async move {
        let result = tokio::task::spawn_blocking(move || {
            ipc.pipe_to_plugin(&plugin_path, &pipe_name, &payload)
        })
        .await;

        match result {
            Ok(Ok(())) => {}
            Ok(Err(e)) => warn!(
                "[ZellijEvents] register_worker_pane pipe_to_plugin failed: {}",
                e
            ),
            Err(e) => warn!(
                "[ZellijEvents] register_worker_pane spawn_blocking join error: {}",
                e
            ),
        }
    });
}

/// Helper to get current timestamp in ISO 8601 format.
pub fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}
