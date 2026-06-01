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
//!   HOOK (N4):      `exomonad hook` → exo-policy pre_tool_use / stop / session_start.
//! ```
//!
//! See `docs/design/swarm/02-bus-and-sidecar.md` (two-loop model + cursor/restart),
//! `05-crates-and-binary.md` (modes), `06-migration.md` (the Wave-2 leaf table + the
//! "every WorldEvent variant has a live producer" converge gate), `01-identity.md` (papers).
//!
//! **Status: Wave-2 scaffold.** [`bootstrap`] is real (self-ID from papers + exo-scry →
//! build `NodeContext`); the loop modules (`outbound` N1, `dispatch` N2a, `inbound` N2b,
//! `poll` N3, `hook` N4) are stubs the Gemini leaves fill in, one file each — non-overlapping
//! so leaves never collide. [`run_node`] assembles them as tokio tasks at converge.

pub mod bootstrap;
pub mod dispatch;
pub mod error;
pub mod hook;
pub mod inbound;
pub mod outbound;
pub mod poll;

pub use bootstrap::{bootstrap, NodeContext};
pub use error::{NodeError, NodeResult};

use std::sync::Arc;

/// Assemble and run the node's three concurrent stimuli (outbound MCP serve, inbound
/// inbox watch, self-poll) as tokio tasks in one process. The dispatch boundary requires
/// `R: Send + Sync + 'static` — satisfied by `Arc<NodeContext>`.
///
/// **Converge wiring (filled at convergence, after the loop leaves land).** Until then this
/// is a stub so the crate's public shape is fixed for the leaves.
pub async fn run_node(ctx: Arc<NodeContext>) -> NodeResult<()> {
    // Converge: tokio::join! the outbound serve, inbound watch, and self-poll supervisor.
    // Each leaf lands its half (outbound::serve / inbound::watch / poll::supervise) first.
    let _ = ctx;
    Err(NodeError::NotAssembled)
}
