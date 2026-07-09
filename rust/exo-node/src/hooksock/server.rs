//! Hook-RPC server (N5) — runs in the sidecar, sharing `ctx.runtime` with the other loops.

use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::sync::Arc;

use exo_caps::{HookEvent, HookRequest, HookVerdict};
use exo_framework::{Exomonad, HookDecision, HookInput, RoleDef};
use exo_runtime::Runtime;
use serde_json::json;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{UnixListener, UnixStream};
use tracing::{info, warn};

use crate::bootstrap::NodeContext;
use crate::error::{NodeError, NodeResult};

/// Serve the per-agent hook-RPC socket. Binds `paths::hook_sock(home, run_id, own_pane)`
/// (remove-before-bind to clear a stale socket, then `0o600`), accepts connections, and for each
/// [`HookRequest`](exo_caps::HookRequest) runs the role's hook fn against the **live**
/// `ctx.runtime` (NOT a fresh `bootstrap()` Runtime), then replies with a
/// [`HookVerdict`](exo_caps::HookVerdict) in the Claude hook-output shape (every tree node is a
/// Claude instance).
///
/// Spawned as a background task by [`run_node`](crate::run_node) and aborted when the outbound
/// serve loop returns; an error here is logged, never fatal.
#[tracing::instrument(skip(ctx), fields(node = %ctx.runtime.name().as_str()))]
pub async fn serve<D: Exomonad<Caps = Runtime>>(ctx: Arc<NodeContext<D>>) -> NodeResult<()> {
    let home = std::env::var("HOME").map_err(|_| NodeError::MissingContext("HOME"))?;
    let sock = exo_caps::paths::hook_sock(Path::new(&home), &ctx.run_id, &ctx.own_pane);

    if let Some(parent) = sock.parent() {
        std::fs::create_dir_all(parent)?;
    }
    // Remove a stale socket so bind() can't fail with EADDRINUSE. NotFound is fine.
    match std::fs::remove_file(&sock) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => return Err(e.into()),
    }

    let listener = UnixListener::bind(&sock)?;
    std::fs::set_permissions(&sock, std::fs::Permissions::from_mode(0o600))?;
    info!(socket = %sock.display(), "hooksock: listening");

    loop {
        let (stream, _addr) = listener.accept().await?;
        let ctx = ctx.clone();
        tokio::spawn(async move {
            if let Err(e) = handle_conn(ctx, stream).await {
                warn!("hooksock: connection handler error: {e}");
            }
        });
    }
}

/// One request/response cycle. The client writes a `HookRequest` then half-closes its write side;
/// we read to EOF, run the hook, write the `HookVerdict`, and close.
#[tracing::instrument(skip(ctx, stream), fields(node = %ctx.runtime.name().as_str()))]
async fn handle_conn<D: Exomonad<Caps = Runtime>>(
    ctx: Arc<NodeContext<D>>,
    stream: UnixStream,
) -> NodeResult<()> {
    let mut buf = Vec::new();
    // Read up to 64KB with a 2-second timeout to prevent memory exhaustion and deadlocks.
    // The client MUST half-close its write side for us to see EOF.
    let mut limited = stream.take(64 * 1024);
    let read_fut = limited.read_to_end(&mut buf);
    match tokio::time::timeout(std::time::Duration::from_secs(2), read_fut).await {
        Ok(Ok(_)) => {}
        Ok(Err(e)) => return Err(e.into()),
        Err(_) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::TimedOut,
                "hooksock: request timeout",
            )
            .into())
        }
    }
    let mut stream = limited.into_inner();

    let req: HookRequest = serde_json::from_slice(&buf).map_err(|e| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("hooksock: bad HookRequest: {e}"),
        )
    })?;

    info!(event = ?req.event, "hooksock: received hook request");
    let verdict = run(&ctx, &req).await;
    let out = serde_json::to_vec(&verdict)
        .map_err(|e| std::io::Error::other(format!("hooksock: encode HookVerdict: {e}")))?;
    stream.write_all(&out).await?;
    stream.shutdown().await?;

    Ok(())
}

/// Run the role's hook fn on the LIVE runtime, then shape stdout for the node's agent_type.
#[tracing::instrument(skip(ctx, req), fields(node = %ctx.runtime.name().as_str(), event = ?req.event))]
async fn run<D: Exomonad<Caps = Runtime>>(ctx: &NodeContext<D>, req: &HookRequest) -> HookVerdict {
    let rd = D::role_def(ctx.kind);
    let stdout = match req.event {
        HookEvent::PreToolUse => shape_pre_tool_use(&rd, &ctx.runtime, &req.stdin_json).await,
        HookEvent::SessionStart => {
            // SessionStart is handled one-shot by the client, never over the socket. A hook must
            // never wedge an agent, so be defensive and fail-safe allow if one ever arrives.
            warn!(
                outcome = "allow_fallback",
                "hooksock: unexpected SessionStart over socket; returning allow"
            );
            allow_json()
        }
    };
    info!(outcome = "success", "hooksock: hook execution complete");
    HookVerdict { stdout }
}

async fn shape_pre_tool_use(
    rd: &RoleDef<exo_runtime::Runtime>,
    rt: &exo_runtime::Runtime,
    stdin_json: &str,
) -> String {
    let input: HookInput = match serde_json::from_str(stdin_json) {
        Ok(i) => i,
        Err(e) => {
            warn!("hooksock: bad PreToolUse stdin ({e}); allowing");
            return allow_json();
        }
    };
    match (rd.pre_tool_use)(rt, &input).await {
        HookDecision::Allow => json!({"continue": true}).to_string(),
        HookDecision::Deny { reason } => {
            json!({"continue": true, "systemMessage": reason}).to_string()
        }
        HookDecision::Modify { input } => json!({
            "continue": true,
            "hookSpecificOutput": {"hookEventName": "PreToolUse", "toolInput": input}
        })
        .to_string(),
    }
}

fn allow_json() -> String {
    json!({"continue": true}).to_string()
}

#[cfg(test)]
mod tests {
    use super::allow_json;

    #[test]
    fn allow_shape_is_claude() {
        assert_eq!(allow_json(), r#"{"continue":true}"#);
    }
}
