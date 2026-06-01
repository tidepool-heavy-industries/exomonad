//! `exo-node` — the per-node sidecar (Wave 2).
//!
//! Assembles the real [`exo_runtime::Runtime`] (all 9 caps) + [`exo_policy`] (tools / hooks
//! / events / `role_def`) into a running **two-loop sidecar**, one process per agent:
//!
//! ```text
//!   OUTBOUND (N1):  serve exo-policy Tools over rmcp/stdio; send_message → Bus::deliver.
//!   INBOUND  (N2):  watch own ingestion inbox (cursor + notify-watch, N2b) → per entry,
//!                   last-hop dispatch (N2a) by agent_type: CC-in-team → Teams inbox; else tmux-paste.
//!   SELF-POLL (N3): while an open PR exists, poll review_state/ci_status → WorldEvent →
//!                   on_world_event → InjectMessage / NotifyParent.
//!   HOOK (N4):      `exomonad experimental hook` → exo-policy pre_tool_use / stop / session_start.
//! ```
//!
//! See `docs/design/swarm/02-bus-and-sidecar.md` (two-loop model + cursor/restart),
//! `05-crates-and-binary.md` (modes), `06-migration.md` (the Wave-2 leaf table + the
//! "every WorldEvent variant has a live producer" converge gate), `01-identity.md` (papers).
//!
//! **Status: Wave-2 assembled.** [`bootstrap`] self-IDs from papers; the loop modules
//! (`outbound` N1, `dispatch` N2a, `inbound` N2b, `poll` N3, `hook` N4) are implemented;
//! [`run_node`] wires the three stimuli as concurrent tokio tasks.

pub mod bootstrap;
pub mod dispatch;
pub mod error;
pub mod hook;
pub mod inbound;
pub mod outbound;
pub mod poll;

pub use bootstrap::{bootstrap, NodeContext};
pub use error::{NodeError, NodeResult};
pub use hook::{handle as handle_hook, HookEvent};

use std::sync::Arc;

/// Run the node's three concurrent stimuli in one process:
/// - **outbound** ([`outbound::serve`]) — serve the role's MCP tools over stdio. This owns
///   stdin/stdout and returns when the stream closes (agent gone), so it is the node's
///   **lifetime anchor**: when it ends, the node ends.
/// - **inbound** ([`inbound::watch`]) — watch the ingestion inbox (cursor + notify) and route
///   each entry; ends on a `Control(Shutdown)`.
/// - **self-poll** ([`poll::supervise`]) — poll the node's own PR while one is open.
///
/// The two background loops are aborted when `serve` returns. `Arc<NodeContext>` satisfies the
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
    let poll = tokio::spawn({
        let ctx = ctx.clone();
        async move {
            if let Err(e) = poll::supervise(ctx).await {
                tracing::error!("self-poll exited with error: {e}");
            }
        }
    });

    // The outbound serve owns stdio and runs for the node's lifetime.
    let result = outbound::serve(ctx).await;

    // Agent stream closed (or serve errored) → reap the background loops.
    inbound.abort();
    poll.abort();

    result
}
