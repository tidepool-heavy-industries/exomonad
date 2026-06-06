//! Hook-RPC client — the short-lived `exomonad experimental hook` process.

use std::path::Path;

use exo_caps::{HookRequest, HookVerdict};

use crate::error::NodeResult;

/// Connect to a node's hook socket, send one [`HookRequest`], return the [`HookVerdict`]. The
/// caller (`exomonad experimental hook`) prints `verdict.stdout` verbatim and exits 0. On a
/// connect failure the caller must **fail open** (print the allow-shape for its agent type and
/// exit 0) — the sidecar being down means there are no tools to gate anyway.
///
/// **Status: Wave-0 stub.** Body lands in leaf A2 (reuse the `uds_client.rs` hyper-over-UnixStream
/// pattern; this is a tiny JSON request/response).
pub async fn client_request(_sock: &Path, _req: &HookRequest) -> NodeResult<HookVerdict> {
    todo!("A2: connect UDS, send HookRequest JSON, read HookVerdict JSON")
}
