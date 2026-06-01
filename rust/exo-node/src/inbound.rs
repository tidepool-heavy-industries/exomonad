//! **N2b — Inbound loop.** Drive the Bus READ side of this node's own ingestion inbox — the
//! cursor/restart half the `Bus` cap (write side) left for Wave 2. Per
//! `docs/design/swarm/02-bus-and-sidecar.md` *Cursor & restart*, implement EXACTLY:
//!
//! - **Cursor = byte-offset** in a sibling `pane-N.cursor`. Resume = seek + read forward, O(1).
//! - **Watch via the `notify` crate** (event-driven, never a poll loop, never hand-rolled
//!   inotify); on each wake re-read from the cursor (absorbs coalesced events).
//! - **Read only up to the last `\n`** — a torn trailing line is re-read once complete.
//! - **Advance the cursor AFTER a successful last-hop**, written **temp + rename** (atomic
//!   replace — a "small" overwrite is NOT crash-atomic). At-least-once, never dropped/corrupted.
//! - **Missing cursor** (fresh node) → start at current EOF; don't replay history.
//! - Parse each line as [`IngestionEntry`] (tolerant: serde defaults, no `deny_unknown_fields`).
//!
//! Then route each new entry by `kind`:
//! - `Chat` → [`crate::dispatch::dispatch`] (N2a last-hop).
//! - `Event` → parse the body into [`exo_policy::WorldEvent`] → `exo_policy::on_world_event`
//!   → act (`InjectMessage` = append to own inbox; `NotifyParent` = append to parent inbox).
//! - `Control(Shutdown { grace_ms })` → after the grace, self-kill OWN pane (the node knows
//!   `$TMUX_PANE`) — reaping pane + agent + sidecar in one shot.
//!
//! **Status: stub (N2b leaf fills this — race-prone; a sub-TL is warranted if a single
//! Gemini can't hold the cursor/torn-line/temp-rename invariants).** Acceptance: append N
//! lines while watching → exactly N delivered in order; kill mid-delivery → the one in-flight
//! line redelivers on restart (at-least-once), never a corrupt cursor.

use std::sync::Arc;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Watch the node's own ingestion inbox and route each new entry until shutdown.
pub async fn watch(ctx: Arc<NodeContext>) -> NodeResult<()> {
    let _ = ctx;
    todo!("N2b: notify-watch ctx.own_inbox; byte-offset cursor (temp+rename); per kind -> dispatch | on_world_event | shutdown")
}
