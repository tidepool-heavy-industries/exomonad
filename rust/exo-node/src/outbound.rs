//! **N1 — Outbound.** Serve the node's `exo-policy` tools over rmcp/stdio, and route
//! `send_message`/`notify_parent` through `Bus::deliver` (append to the *target's* ingestion
//! inbox — runtime-agnostic; policy never names Teams or tmux).
//!
//! Refactors the `teams-mcp` outbound server (`rust/teams-mcp/src/main.rs`): instead of
//! writing CC Teams inboxes directly, it exposes `role_def::<Runtime>(kind).tools` and the
//! tools' `Bus::deliver` writes the **ingestion** inbox. The rmcp `tools/list` →
//! `Tool::schema`, `tools/call` → `Tool::call(&*ctx.runtime, args)`.
//!
//! **Status: stub (N1 leaf fills this).** Acceptance: `tools/list` returns the role's tool
//! schemas; `tools/call` dispatches to `Tool::call` against the real `Runtime`; a
//! `notify_parent` call appends one `IngestionEntry` to `ctx.parent_inbox`.

use std::sync::Arc;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Serve the policy toolset over rmcp/stdio until the stream closes.
pub async fn serve(ctx: Arc<NodeContext>) -> NodeResult<()> {
    let _ = ctx;
    todo!("N1: rmcp stdio server exposing role_def(kind).tools; tools/call -> Tool::call(&*ctx.runtime, args)")
}
