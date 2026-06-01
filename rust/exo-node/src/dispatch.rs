//! **N2a — Last-hop dispatch.** Route one consumed ingestion entry INTO this agent, by the
//! node's own `agent_type` (= `kind.agent_type()`) + CC team membership (resolved via
//! `exo-scry`):
//!
//! | this node | mechanism |
//! |---|---|
//! | CC, in a team | write the CC Teams inbox → InboxPoller → `<teammate-message>` |
//! | CC, no team   | tmux-paste into its own pane |
//! | gemini        | tmux-paste into its own pane |
//!
//! For the tmux-paste path, render the entry with a `[from: X, kind: Y]` header (the input
//! box *is* the receive channel for non-CC runtimes). Reuse exomonad-core's tmux injection
//! (buffer pattern) + CC-inbox delivery — adapt, don't rewrite.
//!
//! **Status: stub (N2a leaf fills this).** Acceptance: a `Chat` entry delivered to a gemini
//! node lands pasted-with-header in its pane; a CC-in-team node's entry lands in its Teams
//! inbox. The dispatch is pure last-hop — `kind`-based routing (event/control) is N2b's job;
//! this only does the agent-facing write for entries N2b decides to deliver.

use std::sync::Arc;

use exo_caps::IngestionEntry;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Deliver one ingestion entry into this node's own agent (the runtime-specific last hop).
pub async fn dispatch(ctx: &Arc<NodeContext>, entry: &IngestionEntry) -> NodeResult<()> {
    let _ = (ctx, entry);
    todo!("N2a: route by ctx.kind.agent_type() + exo-scry membership -> Teams inbox | tmux-paste(header)")
}
