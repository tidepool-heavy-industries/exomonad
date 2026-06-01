//! **N3 — Self-poll.** The per-agent realization of the old central poller. While this node
//! has an open PR, periodically poll its own PR/CI state, turn transitions into
//! [`exo_policy::WorldEvent`]s, run `exo_policy::on_world_event`, and act on the result
//! (`InjectMessage` → own inbox; `NotifyParent` → parent inbox).
//!
//! WorldEvent producers (the "no dead variant" converge gate — see `06-migration.md`):
//! - `PrReview { state }` ← `ctx.runtime.review_state(pr)` (C2 cap).
//! - `CiStatus { status }` ← `ctx.runtime.ci_status(pr)` (C2 cap).
//! - `ReviewTimeout` ← when `review_state(pr).is_none()` past the ~15-min window (the window
//!   **resets on each feedback round**). Reuse `exomonad-core/.../github_poller.rs` timeout
//!   logic — adapt, don't rewrite.
//! - `SiblingMerged` is NOT produced here — it's parent-side, fanned out on the parent's
//!   merge path (it holds the child ledger). See [`crate::poll::fan_sibling_merged`].
//!
//! Discipline (bounds API load — no central poller): poll **every ~3 min, ONLY while an open
//! PR exists** (no PR → no polling → a swarm is sparse). **Tracked `AbortHandle`:** abort the
//! poll task when the PR merges/closes; a re-`file_pr` must dedup/replace, never leak a second
//! poller (a bare `tokio::spawn` double-polls).
//!
//! **Status: stub (N3 leaf fills this).** Acceptance: with a mock GitHub cap returning
//! Approved, one poll cycle emits a `PrReview{Approved}` → `NotifyParent([PR READY])` to the
//! parent inbox; aborting closes the task with no leak.

use std::sync::Arc;

use exo_caps::AgentName;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Supervise the self-poll lifecycle: spawn the poll task when a PR opens, hold its
/// `AbortHandle`, abort+replace on PR close / re-file. Runs for the node's lifetime.
pub async fn supervise(ctx: Arc<NodeContext>) -> NodeResult<()> {
    let _ = ctx;
    todo!("N3: while open-PR, 3-min poll review_state/ci_status -> WorldEvent -> on_world_event -> act; tracked AbortHandle, dedup on re-file")
}

/// Parent-side producer of `WorldEvent::SiblingMerged`: after this (parent) node merges a
/// child's PR, fan a `SiblingMerged { pr, branch }` to the OTHER children's inboxes (resolved
/// from the child ledger). Keeps every WorldEvent variant with a live producer.
///
/// **Status: stub (N3 leaf fills this).** Acceptance: merging child A's PR appends a
/// `SiblingMerged` ingestion entry to each live sibling's inbox, not to A's.
pub async fn fan_sibling_merged(
    ctx: &Arc<NodeContext>,
    merged_child: &AgentName,
    pr: u64,
    branch: &str,
) -> NodeResult<()> {
    let _ = (ctx, merged_child, pr, branch);
    todo!("N3 parent-side: fold child ledger -> append SiblingMerged to each sibling inbox except merged_child")
}
