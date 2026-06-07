//! `exo-node` — the per-node sidecar (Wave 2).
//!
//! Assembles the real [`exo_runtime::Runtime`] (all caps) + an injected
//! [`RoleRegistry`](exo_framework::RoleRegistry) (the domain's tools / hooks / roles, built by the
//! binary via `exo::roster()`) into a running **two-loop sidecar**, one process per agent:
//!
//! ```text
//!   OUTBOUND (N1):  serve the role's Tools (from the injected roster) over rmcp/stdio; send_message → Bus::deliver.
//!   INBOUND  (N2):  watch own ingestion inbox (cursor + notify-watch, N2b) → per entry,
//!                   last-hop dispatch (N2a) by agent_type: CC-in-team → Teams inbox; else tmux-paste.
//!   HOOK (N4):      `exomonad experimental hook` → the role's pre_tool_use / stop / session_start.
//! ```
//!
//! Convergence is on-disk (v2): a TL folds a finished child by merging its branch locally
//! (the `merge` tool). There is no GitHub poller / world-event layer.
//!
//! **Status: Wave-2 assembled.** [`bootstrap`] self-IDs from papers; the loop modules
//! (`outbound` N1, `dispatch` N2a, `inbound` N2b, `hook` N4) are implemented; [`run_node`]
//! wires the two stimuli as concurrent tokio tasks.

pub mod bootstrap;
pub mod dispatch;
pub mod error;
pub mod hook;
pub mod hooksock;
pub mod inbound;
pub mod outbound;
pub mod teamout;

#[cfg(test)]
mod test_support;

pub use bootstrap::{bootstrap, NodeContext};
pub use error::{NodeError, NodeResult};
pub use hook::{handle as handle_hook, HookEvent};

use std::sync::Arc;

/// Run the node's concurrent stimuli in one process (outbound serve + inbound watch + hooksock +
/// teamout):
/// - **outbound** ([`outbound::serve`]) — serve the role's MCP tools over stdio. This owns
///   stdin/stdout and returns when the stream closes (agent gone), so it is the node's
///   **lifetime anchor**: when it ends, the node ends.
/// - **inbound** ([`inbound::watch`]) — watch the ingestion inbox (cursor + notify) and route
///   each entry; ends on a `Control(Shutdown)`.
/// - **hooksock** ([`hooksock::serve`]) — background hook-RPC socket (N5); also aborted when serve returns.
/// - **teamout** ([`teamout::watch`]) — N6, Claude-only: watch this node's CC team inboxes and
///   forward the agent's outbound teammate messages / shutdown_requests onto the bus.
///
/// The background loops are aborted when `serve` returns. `Arc<NodeContext>` satisfies the
/// `R: Send + Sync + 'static` dispatch boundary. A background loop erroring is logged but does
/// not tear down the node — only the outbound anchor closing (or a shutdown) ends it.
pub async fn run_node(ctx: Arc<NodeContext>) -> NodeResult<()> {
    let inbound = tokio::spawn({
        let ctx = ctx.clone();
        async move {
            if let Err(e) = inbound::watch(ctx).await {
                tracing::error!("inbound loop exited with error: {e}");
            }
        }
    });

    // N5 — per-agent hook-RPC socket. Background like inbound; an error is logged, not fatal.
    let hooksock = tokio::spawn({
        let ctx = ctx.clone();
        async move {
            if let Err(e) = hooksock::serve(ctx).await {
                tracing::error!("hooksock loop exited with error: {e}");
            }
        }
    });

    // N6 — outbound Teams bridge (Claude nodes). Background; an error is logged, not fatal.
    let teamout = tokio::spawn({
        let ctx = ctx.clone();
        async move {
            if let Err(e) = teamout::watch(ctx).await {
                tracing::error!("teamout loop exited with error: {e}");
            }
        }
    });

    // Periodic status publisher — writes the node's status snapshot to disk for visibility.
    let status = tokio::spawn({
        let ctx = ctx.clone();
        async move {
            let status_path = exo_caps::paths::status_path(&home(), &ctx.run_id, &ctx.own_pane);
            // Ensure status directory exists
            if let Some(parent) = status_path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }

            let mut interval = tokio::time::interval(std::time::Duration::from_secs(5));
            loop {
                interval.tick().await;
                let shutdown_pending = ctx.shutdown_pending.lock().unwrap().is_some();
                let snapshot = ctx.runtime.status_snapshot(ctx.kind, shutdown_pending);
                if let Ok(bytes) = serde_json::to_vec(&snapshot) {
                    if let Err(e) =
                        exo_caps::Fs::write_atomic(&*ctx.runtime, &status_path, &bytes).await
                    {
                        tracing::error!("failed to write status snapshot: {e}");
                    }
                }
            }
        }
    });

    // The outbound serve owns stdio and runs for the node's lifetime.
    let result = outbound::serve(ctx).await;

    // Agent stream closed (or serve errored) → reap the background loops.
    inbound.abort();
    hooksock.abort();
    teamout.abort();
    status.abort();

    result
}

fn home() -> std::path::PathBuf {
    std::env::var("HOME")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| std::path::PathBuf::from("."))
}
