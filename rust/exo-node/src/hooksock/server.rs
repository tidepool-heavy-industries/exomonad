//! Hook-RPC server (N5) — runs in the sidecar, sharing `ctx.runtime` with the other loops.

use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::sync::Arc;

use exo_caps::{AgentType, HookEvent, HookRequest, HookVerdict, RoleKind};
use exo_framework::{Exomonad, HookDecision, HookInput, RoleDef, StopDecision};
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
/// [`HookVerdict`](exo_caps::HookVerdict) shaped for the node's `agent_type` — **never a Gemini
/// Stop `deny`** (gemini-cli #20426 can infinite-loop on `AfterAgent` deny).
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

    // A Stop hook means the agent just went idle. If a cooperative shutdown is pending and our
    // subtree is now clear, this is the safe point to reap ourselves. No-op otherwise. Done AFTER
    // replying so the agent isn't left waiting on a verdict it'll never read.
    if req.event == HookEvent::Stop {
        crate::inbound::try_reap(&ctx).await;
    }
    Ok(())
}

/// Run the role's hook fn on the LIVE runtime, then shape stdout for the node's agent_type.
#[tracing::instrument(skip(ctx, req), fields(node = %ctx.runtime.name().as_str(), event = ?req.event))]
async fn run<D: Exomonad<Caps = Runtime>>(ctx: &NodeContext<D>, req: &HookRequest) -> HookVerdict {
    let rd = D::role_def(ctx.kind);
    let agent_type = ctx.kind.agent_type();
    let stdout = match req.event {
        HookEvent::PreToolUse => {
            shape_pre_tool_use(&rd, &ctx.runtime, &req.stdin_json, agent_type).await
        }
        HookEvent::Stop => shape_stop(&rd, &ctx.runtime, agent_type).await,
        HookEvent::SessionStart => {
            // SessionStart is handled one-shot by the client, never over the socket. A hook must
            // never wedge an agent, so be defensive and fail-safe allow if one ever arrives.
            warn!(
                outcome = "allow_fallback",
                "hooksock: unexpected SessionStart over socket; returning allow"
            );
            allow_json(agent_type)
        }
    };
    info!(outcome = "success", "hooksock: hook execution complete");
    HookVerdict { stdout }
}

async fn shape_pre_tool_use(
    rd: &RoleDef<exo_runtime::Runtime>,
    rt: &exo_runtime::Runtime,
    stdin_json: &str,
    agent_type: AgentType,
) -> String {
    let input: HookInput = match serde_json::from_str(stdin_json) {
        Ok(i) => i,
        Err(e) => {
            warn!("hooksock: bad PreToolUse stdin ({e}); allowing");
            return allow_json(agent_type);
        }
    };
    let decision = (rd.pre_tool_use)(rt, &input).await;
    match agent_type {
        AgentType::Claude | AgentType::Shoal => match decision {
            HookDecision::Allow => json!({"continue": true}).to_string(),
            HookDecision::Deny { reason } => {
                json!({"continue": true, "systemMessage": reason}).to_string()
            }
            HookDecision::Modify { input } => json!({
                "continue": true,
                "hookSpecificOutput": {"hookEventName": "PreToolUse", "toolInput": input}
            })
            .to_string(),
        },
        AgentType::Gemini => match decision {
            HookDecision::Allow => json!({}).to_string(),
            // BeforeTool deny is safe (delivered as a tool error, no retry loop).
            HookDecision::Deny { reason } => {
                json!({"decision": "deny", "reason": reason}).to_string()
            }
            // Gemini BeforeTool has no tool-input rewrite shape; surface nothing and allow.
            HookDecision::Modify { .. } => {
                warn!("hooksock: dropping Gemini PreToolUse Modify (no BeforeTool rewrite shape)");
                json!({}).to_string()
            }
        },
    }
}

async fn shape_stop(
    rd: &RoleDef<exo_runtime::Runtime>,
    rt: &exo_runtime::Runtime,
    agent_type: AgentType,
) -> String {
    stop_verdict((rd.stop)(rt).await, agent_type)
}

/// Pure agent-shaping of a [`StopDecision`] (split out so the wire shapes — and the #20426 safety
/// net — are unit-testable without a live runtime).
fn stop_verdict(decision: StopDecision, agent_type: AgentType) -> String {
    match agent_type {
        AgentType::Claude | AgentType::Shoal => match decision {
            StopDecision::Allow => json!({"continue": true}).to_string(),
            StopDecision::Block { reason } => {
                json!({"decision": "block", "reason": reason}).to_string()
            }
        },
        // SAFETY NET: never emit a Gemini Stop deny. `AfterAgent` deny can infinite-loop
        // (gemini-cli #20426). Policy already must not block Gemini at stop; this downgrade is
        // defence in depth.
        AgentType::Gemini => match decision {
            StopDecision::Allow => json!({}).to_string(),
            StopDecision::Block { reason } => {
                warn!("hooksock: downgrading Gemini Stop block to allow (gemini-cli #20426): {reason}");
                json!({}).to_string()
            }
        },
    }
}

fn allow_json(agent_type: AgentType) -> String {
    match agent_type {
        AgentType::Claude | AgentType::Shoal => json!({"continue": true}).to_string(),
        AgentType::Gemini => json!({}).to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::{allow_json, stop_verdict};
    use exo_caps::AgentType;
    use exo_framework::StopDecision;

    #[test]
    fn allow_shape_per_agent_type() {
        assert_eq!(allow_json(AgentType::Claude), r#"{"continue":true}"#);
        assert_eq!(allow_json(AgentType::Shoal), r#"{"continue":true}"#);
        assert_eq!(allow_json(AgentType::Gemini), "{}");
    }

    #[test]
    fn stop_allow_shapes_per_agent_type() {
        assert_eq!(
            stop_verdict(StopDecision::Allow, AgentType::Claude),
            r#"{"continue":true}"#
        );
        assert_eq!(stop_verdict(StopDecision::Allow, AgentType::Gemini), "{}");
    }

    #[test]
    fn stop_block_emits_block_for_claude() {
        let out = stop_verdict(
            StopDecision::Block {
                reason: "commit first".into(),
            },
            AgentType::Claude,
        );
        assert!(out.contains(r#""decision":"block""#));
        assert!(out.contains("commit first"));
    }

    #[test]
    fn stop_block_is_downgraded_to_allow_for_gemini() {
        // The #20426 safety net: a Gemini Stop block must NEVER reach the agent as `deny`
        // (it can infinite-loop). It is downgraded to the allow shape.
        let out = stop_verdict(
            StopDecision::Block {
                reason: "should be swallowed".into(),
            },
            AgentType::Gemini,
        );
        assert_eq!(out, "{}");
        assert!(!out.contains("deny"));
        assert!(!out.contains("should be swallowed"));
    }
}
