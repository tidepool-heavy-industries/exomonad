//! Hook-RPC server (N5) — runs in the sidecar, sharing `ctx.runtime` with the other loops.

use std::sync::Arc;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Serve the per-agent hook-RPC socket. Binds `paths::hook_sock(home, run_id, own_pane)`
/// (remove-before-bind to clear a stale socket, then `0o600`), accepts connections, and for each
/// [`HookRequest`](exo_caps::HookRequest) runs the role's hook fn against the **live**
/// `ctx.runtime` (NOT a fresh `bootstrap()` Runtime — sharing the live one is the whole point),
/// then replies with a [`HookVerdict`](exo_caps::HookVerdict) whose `stdout` is already shaped
/// for the node's `agent_type` (Claude vs Gemini; **never a Gemini Stop `deny`** — Gemini
/// `AfterAgent` deny can infinite-loop, gemini-cli #20426).
///
/// Spawned as a background task by [`run_node`](crate::run_node) and aborted when the outbound
/// serve loop (the lifetime anchor) returns; an error here is logged, never fatal.
///
/// **Status: Wave-0 stub.** Body lands in leaf A1 (reuse the `serve.rs` `UnixListener` pattern).
pub async fn serve(_ctx: Arc<NodeContext>) -> NodeResult<()> {
    todo!("A1: remove-before-bind hook_sock + 0o600; accept loop; per-conn read HookRequest, run role_def(ctx.kind).<event> on ctx.runtime, shape verdict per agent_type, write HookVerdict")
}
