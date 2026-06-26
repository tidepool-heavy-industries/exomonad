//! Gates — `pre_tool_use` (antipattern nudges), `stop` (the live convergence gate), and
//! `session_start` (root identity bootstrap). These are the concrete domain hook bodies:
//! **functions generic over the caps they need** (no `dyn Caps`); the
//! [`RoleDef`](exo_framework::RoleDef) table stores them as `fn(&R, …) -> BoxFuture<…>`
//! monomorphized at the concrete runtime `R`, so the generic bound *is* the per-hook
//! least-privilege spec. The decision enums they return are the framework contract
//! ([`exo_framework::hooks`]).

use crate::review::ReviewSystem;
use exo_caps::{
    deliver_domain, Addressee, Bus, CapResult, ChildLiveness, Git, Kv, Lifecycle, Message,
    MessageBody, MessageKind, Summary,
};
use exo_framework::{BoxFuture, HookDecision, HookInput, SessionStartOutput, StopDecision};

/// Ported hook implementations.
///
/// `pre_tool_use` is a **default-ALLOW antipattern-nudge** hook. It inspects tool calls
/// for known antipatterns and returns `Deny` with a guidance message or `Modify` to
/// rewrite the call toward a better pattern. It is NOT a security/allowlist gate.
pub fn pre_tool_use<'a, R: Send + Sync>(
    _ctx: &'a R,
    input: &'a HookInput,
) -> BoxFuture<'a, HookDecision> {
    let tool_name = input.tool_name.clone();
    let tool_input = input.tool_input.clone();

    Box::pin(async move {
        // Antipattern: Avoid `git add .` or `git add -A` (Claude's `Bash` tool).
        if tool_name == "Bash" {
            if let Some(cmd) = tool_input.get("command").and_then(|v| v.as_str()) {
                let parts: Vec<&str> = cmd.split_whitespace().collect();
                for i in 0..parts.len() {
                    // Look for 'git add' followed by '.' or '-A' as a distinct argument.
                    if parts[i] == "git"
                        && i + 2 < parts.len()
                        && parts[i + 1] == "add"
                        && (parts[i + 2] == "." || parts[i + 2] == "-A")
                    {
                        return HookDecision::Deny {
                            reason: "Avoid `git add -A`/`git add .` — stage specific files by path to avoid committing stray artifacts.".into(),
                        };
                    }
                }
            }
        }

        HookDecision::Allow
    })
}

/// Build the [`ChildIdle`](Lifecycle::ChildIdle) a non-root node delivers to its parent at
/// turn-end. Minimal by design (v1): a fixed human-readable summary the parent's lifecycle handler
/// renders. `ChildIdle` is an engine-owned [`Lifecycle`] signal (the sidecar acts on it), so it
/// rides the typed lifecycle wire, not the domain payload.
fn child_idle_message() -> CapResult<Message> {
    let summary = "finished a turn and is yielding control";
    Ok(Message {
        text: MessageBody::new(format!("[idle] {summary}"))?,
        summary: Summary::new(summary.into())?,
        kind: MessageKind::Lifecycle(Lifecycle::ChildIdle {
            summary: summary.into(),
        }),
    })
}

/// Best-effort turn-end signal: deliver a `ChildIdle` to the parent. Logs and swallows any error
/// — a stop hook must never fail an agent's exit over a missed notification. Root has no parent,
/// so it never calls this (it uses `stop_allow`).
async fn notify_parent_idle<R: Bus>(ctx: &R) {
    match child_idle_message() {
        Ok(msg) => {
            if let Err(e) = ctx.deliver(Addressee::Parent, msg).await {
                tracing::error!("stop hook: failed to notify parent of idle: {e}");
            }
        }
        Err(e) => tracing::error!("stop hook: could not build ChildIdle message: {e}"),
    }
}

/// True if any direct child is still working — see [`ChildLiveness::any_child_busy`]. "Idle" means
/// the whole *subtree* is quiescent (a child reports `ChildIdle` upward only once its own subtree
/// is), so a node with a busy child must NOT signal idle — that's per-turn noise to the parent
/// while it's mid-flow. The cap is infallible and biases against false idle (probe failure ⇒ trust
/// the busy-bit; the `submit_branch`/`[READY]` flow is the authoritative "done").
async fn subtree_busy<R: ChildLiveness>(ctx: &R) -> bool {
    ctx.any_child_busy().await
}

/// Returns `Some(Block)` when the worktree is clean, the branch is ahead of its derived parent
/// branch, and `submit_branch` was never called this session — the "committed but never submitted"
/// failure mode. Returns `None` on any git/kv error (fail-open: never wedge on an error) and
/// whenever the conditions are not met (dirty, not ahead, or already submitted).
///
/// Base is derived by stripping the last `.`-segment from the current branch name, mirroring the
/// parent-branch convention: `root.my-dev` → base `root`, `root.tl.my-dev` → base `root.tl`.
/// If the base ref does not exist as a live git ref, `is_ahead_of` fails open (`Ok(false)`).
async fn committed_unsubmitted_block<R: Git + Kv + Send + Sync>(
    ctx: &R,
) -> Option<StopDecision> {
    let clean = match ctx.is_clean().await {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!("commit-guard: is_clean failed, allowing exit: {e}");
            return None;
        }
    };
    if !clean {
        return None;
    }
    let branch = match ctx.current_branch().await {
        Ok(b) => b,
        Err(e) => {
            tracing::warn!("commit-guard: current_branch failed, allowing exit: {e}");
            return None;
        }
    };
    let base = branch
        .as_str()
        .rsplit_once('.')
        .map(|(p, _)| p)
        .unwrap_or("main");
    let ahead = match ctx.is_ahead_of(base).await {
        Ok(a) => a,
        Err(e) => {
            tracing::warn!("commit-guard: is_ahead_of failed, allowing exit: {e}");
            return None;
        }
    };
    if !ahead {
        return None;
    }
    let submitted = match ctx.get("submit_branch_called").await {
        Ok(v) => v.is_some(),
        Err(e) => {
            tracing::warn!("commit-guard: kv.get failed, allowing exit: {e}");
            return None;
        }
    };
    if submitted {
        return None;
    }
    Some(StopDecision::Block {
        reason: format!(
            "You have committed work on `{branch}` that hasn't been submitted for review — \
             call submit_branch to hand it up, or notify_parent if you're handing off differently.",
            branch = branch.as_str()
        ),
    })
}

/// The local convergence gate for a spawned TL (v2 — no GitHub). A parent folds a child by
/// merging its **branch** off disk, so uncommitted work is invisible to that merge: block exit
/// while the worktree is dirty (commit or discard first). On a clean exit, also run the
/// committed-unsubmitted guard (clean + ahead-of-base + never called submit_branch → Block with
/// guidance). On a clean unblocked exit, notify the parent it went idle — but ONLY when the whole
/// subtree is quiescent. Fails OPEN on any git error — a hook must never wedge an agent.
pub fn stop<'a, R: Git + Bus + ChildLiveness + Kv + Send + Sync>(
    ctx: &'a R,
) -> BoxFuture<'a, StopDecision> {
    Box::pin(async move {
        match ctx.is_clean().await {
            Ok(true) => {
                if let Some(block) = committed_unsubmitted_block(ctx).await {
                    return block;
                }
                if !subtree_busy(ctx).await {
                    notify_parent_idle(ctx).await;
                }
                StopDecision::Allow
            }
            Ok(false) => StopDecision::Block {
                reason: "Uncommitted changes in your worktree. Commit your work (a parent merges \
                         your branch off disk — uncommitted changes are invisible to that merge), \
                         then stop."
                    .into(),
            },
            Err(e) => {
                tracing::error!("stop gate: could not read git status, allowing exit: {e}");
                StopDecision::Allow
            }
        }
    })
}

/// Unconditional-allow stop hook (root, reviewer): nothing to fold and no parent to notify, so
/// always allow exit. The root especially must never be gated — blocking it bricks the human's
/// session; the reviewer's `verdict` is already its done-signal.
pub fn stop_allow<R: Send + Sync>(_ctx: &R) -> BoxFuture<'_, StopDecision> {
    Box::pin(async move { StopDecision::Allow })
}

/// Stop hook for leaves (dev, worker): notify the parent this node yielded control (but ONLY when
/// the subtree is quiescent; skip if a child is busy), then ALWAYS allow exit. It NEVER blocks —
/// a leaf has no subtree to fold, so gating its exit would only risk wedging it. The
/// committed-before-fold guarantee for a dev is enforced by `submit_branch`'s committed-check, not
/// here; a worker is inline with no branch to fold.
pub fn stop_notify<'a, R: Bus + ChildLiveness + Send + Sync>(
    ctx: &'a R,
) -> BoxFuture<'a, StopDecision> {
    Box::pin(async move {
        if !subtree_busy(ctx).await {
            notify_parent_idle(ctx).await;
        }
        StopDecision::Allow
    })
}

/// Stop hook for dev leaves. Runs the committed-unsubmitted guard first: if the worktree is
/// clean, the branch is ahead of its base, and `submit_branch` was never called this session,
/// block with guidance to call `submit_branch`. Otherwise: notify the parent this node yielded
/// control (when the subtree is quiescent) and always allow exit. Workers share the parent's
/// branch and must NOT use this hook — use `stop_notify` instead.
pub fn stop_dev<'a, R: Git + Bus + ChildLiveness + Kv + Send + Sync>(
    ctx: &'a R,
) -> BoxFuture<'a, StopDecision> {
    Box::pin(async move {
        if let Some(block) = committed_unsubmitted_block(ctx).await {
            return block;
        }
        if !subtree_busy(ctx).await {
            notify_parent_idle(ctx).await;
        }
        StopDecision::Allow
    })
}

/// Stop hook for a reviewer (one-shot). A reviewer's `verdict` IS its done-signal, so on the happy
/// path (verdict produced) this stays SILENT — the sidecar already escalated `[READY]` with no LLM
/// turn. But a reviewer that ends its turn WITHOUT a verdict (e.g. it emitted the call as prose)
/// would otherwise vanish silently and stall the submitter forever, so deliver a loud
/// `ReviewAborted` to the parent. ALWAYS allows exit (it's ephemeral — nothing to fold). Biases
/// LOUD: a kv-read error is treated as no-verdict (a spurious re-submit beats a silent stall).
pub fn stop_reviewer<'a, R: Bus + Kv + Send + Sync>(ctx: &'a R) -> BoxFuture<'a, StopDecision> {
    Box::pin(async move {
        let produced = matches!(ctx.get("verdict_produced").await, Ok(Some(_)));
        if !produced {
            // ReviewAborted is a domain verdict — it rides the erased domain wire via
            // `deliver_domain`, so this gate needs only `Bus` (not `Bus<D::System>`).
            let verdict = ReviewSystem::ReviewAborted {
                reason: "exited without invoking the verdict tool (likely emitted as prose)"
                    .to_string(),
            };
            if let Err(e) = deliver_domain(
                ctx,
                Addressee::Parent,
                "[review aborted]",
                "reviewer exited without producing a verdict",
                &verdict,
            )
            .await
            {
                tracing::error!("stop_reviewer: failed to deliver ReviewAborted: {e}");
            }
        }
        StopDecision::Allow
    })
}

pub fn session_start<'a, R: Send + Sync>(_ctx: &'a R) -> BoxFuture<'a, SessionStartOutput> {
    Box::pin(async move {
        // Root identity bootstrap context injection goes here (additional_context).
        SessionStartOutput::default()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{Call, MockRuntime};
    use serde_json::json;

    fn delivered_child_idle_to_parent(calls: &[Call]) -> bool {
        calls.iter().any(|c| {
            matches!(
                c,
                Call::BusDeliver { to: Addressee::Parent, msg }
                    if matches!(msg.kind, MessageKind::Lifecycle(Lifecycle::ChildIdle { .. }))
            )
        })
    }

    fn delivered_review_aborted_to_parent(calls: &[Call]) -> bool {
        calls.iter().any(|c| {
            matches!(
                c,
                Call::BusDeliver { to: Addressee::Parent, msg }
                    if matches!(&msg.kind, MessageKind::Domain(p)
                        if matches!(
                            serde_json::from_str::<ReviewSystem>(&p.0),
                            Ok(ReviewSystem::ReviewAborted { .. })
                        ))
            )
        })
    }

    #[tokio::test]
    async fn test_stop_reviewer_aborts_when_no_verdict() {
        let ctx = MockRuntime::default(); // no verdict_produced flag set
        let res = stop_reviewer(&ctx).await;

        assert_eq!(res, StopDecision::Allow);
        assert!(delivered_review_aborted_to_parent(&ctx.calls_made()));
    }

    #[tokio::test]
    async fn test_stop_reviewer_silent_when_verdict_produced() {
        let ctx = MockRuntime::default();
        ctx.set("verdict_produced", "true").await.unwrap();
        let res = stop_reviewer(&ctx).await;

        assert_eq!(res, StopDecision::Allow);
        assert!(!delivered_review_aborted_to_parent(&ctx.calls_made()));
    }

    #[tokio::test]
    async fn test_stop_notifies_parent_when_subtree_idle() {
        // Clean worktree AND no busy children → genuine subtree idle → notify.
        let ctx = MockRuntime {
            child_busy: false,
            ..Default::default()
        };
        assert_eq!(stop(&ctx).await, StopDecision::Allow);
        assert!(
            delivered_child_idle_to_parent(&ctx.calls_made()),
            "clean stop with a quiescent subtree should notify parent of idle"
        );
    }

    #[tokio::test]
    async fn test_stop_suppresses_idle_when_subtree_busy() {
        // Clean worktree but a busy child (default mock) → TL is mid-flow, NOT idle → no notify.
        let ctx = MockRuntime::default(); // child_busy = true
        assert_eq!(stop(&ctx).await, StopDecision::Allow);
        assert!(
            !delivered_child_idle_to_parent(&ctx.calls_made()),
            "a TL with active children must not signal idle (per-turn noise)"
        );
    }

    #[tokio::test]
    async fn test_stop_block_when_dirty_does_not_notify() {
        let ctx = MockRuntime {
            is_clean: false,
            ..Default::default()
        };
        assert!(matches!(stop(&ctx).await, StopDecision::Block { .. }));
        assert!(
            !delivered_child_idle_to_parent(&ctx.calls_made()),
            "a blocked (still-working) node must not notify idle"
        );
    }

    #[tokio::test]
    async fn test_stop_notify_notifies_when_idle() {
        let ctx = MockRuntime {
            child_busy: false,
            ..Default::default()
        };
        assert_eq!(stop_notify(&ctx).await, StopDecision::Allow);
        assert!(delivered_child_idle_to_parent(&ctx.calls_made()));
    }

    #[tokio::test]
    async fn test_stop_notify_suppresses_when_busy() {
        let ctx = MockRuntime::default(); // child_busy = true
        assert_eq!(stop_notify(&ctx).await, StopDecision::Allow);
        assert!(
            !delivered_child_idle_to_parent(&ctx.calls_made()),
            "a node with active children must not signal idle"
        );
    }

    #[tokio::test]
    async fn test_stop_notify_never_blocks_even_if_deliver_fails() {
        let ctx = MockRuntime::failing("deliver");
        // Even when the bus delivery errors, a leaf's stop must allow exit (never block).
        assert_eq!(stop_notify(&ctx).await, StopDecision::Allow);
    }

    #[tokio::test]
    async fn test_pre_tool_use_allow_by_default() {
        let ctx = MockRuntime::default();
        let input = HookInput {
            tool_name: "some_unknown_tool".into(),
            tool_input: json!({ "arg": 1 }),
        };
        assert_eq!(pre_tool_use(&ctx, &input).await, HookDecision::Allow);
    }

    #[tokio::test]
    async fn test_pre_tool_use_git_add_antipattern_denied() {
        let ctx = MockRuntime::default();
        let tools = ["Bash"];

        for tool in tools {
            let input = HookInput {
                tool_name: tool.into(),
                tool_input: json!({ "command": "git add ." }),
            };
            match pre_tool_use(&ctx, &input).await {
                HookDecision::Deny { reason } => {
                    assert!(reason.contains("Avoid `git add -A`/`git add .`"));
                }
                _ => panic!("Should be Deny for {} with 'git add .'", tool),
            }

            let input_a = HookInput {
                tool_name: tool.into(),
                tool_input: json!({ "command": "git add -A" }),
            };
            match pre_tool_use(&ctx, &input_a).await {
                HookDecision::Deny { reason } => {
                    assert!(reason.contains("Avoid `git add -A`/`git add .`"));
                }
                _ => panic!("Should be Deny for {} with 'git add -A'", tool),
            }

            // Test with extra whitespace
            let input_ws = HookInput {
                tool_name: tool.into(),
                tool_input: json!({ "command": "  git   add    .  " }),
            };
            assert!(matches!(
                pre_tool_use(&ctx, &input_ws).await,
                HookDecision::Deny { .. }
            ));
        }
    }

    #[tokio::test]
    async fn test_pre_tool_use_git_add_specific_allowed() {
        let ctx = MockRuntime::default();
        let cases = [
            "git add src/main.rs",
            "git add .gitignore",
            "git add ./src/file",
        ];

        for cmd in cases {
            let input = HookInput {
                tool_name: "Bash".into(),
                tool_input: json!({ "command": cmd }),
            };
            assert_eq!(
                pre_tool_use(&ctx, &input).await,
                HookDecision::Allow,
                "Should allow '{}'",
                cmd
            );
        }
    }

    #[tokio::test]
    async fn test_stop_allow_when_clean() {
        let ctx = MockRuntime::default(); // is_clean = true
        assert_eq!(stop(&ctx).await, StopDecision::Allow);
    }

    #[tokio::test]
    async fn test_stop_block_when_dirty() {
        let ctx = MockRuntime {
            is_clean: false,
            ..Default::default()
        };
        match stop(&ctx).await {
            StopDecision::Block { reason } => {
                assert!(reason.contains("Uncommitted changes"));
            }
            _ => panic!("Should be Block when worktree is dirty"),
        }
    }

    #[tokio::test]
    async fn test_session_start_default() {
        let ctx = MockRuntime::default();
        assert_eq!(
            session_start(&ctx).await,
            SessionStartOutput {
                additional_context: None
            }
        );
    }

    // --- committed_unsubmitted_block / stop_dev / tl stop (commit-guard) tests ---

    #[tokio::test]
    async fn test_stop_dev_blocks_clean_ahead_unsubmitted() {
        // Guard fires: clean + ahead-of-base + submit_branch never called.
        let ctx = MockRuntime {
            is_clean: true,
            is_ahead: true,
            child_busy: false,
            ..Default::default()
        };
        match stop_dev(&ctx).await {
            StopDecision::Block { reason } => {
                assert!(reason.contains("submit_branch"), "guidance should mention submit_branch");
            }
            _ => panic!("should block when clean+ahead+unsubmitted"),
        }
    }

    #[tokio::test]
    async fn test_stop_dev_allows_when_submitted() {
        // Guard does not fire when submit_branch was called.
        let ctx = MockRuntime {
            is_clean: true,
            is_ahead: true,
            child_busy: false,
            ..Default::default()
        };
        ctx.set("submit_branch_called", "true").await.unwrap();
        assert_eq!(stop_dev(&ctx).await, StopDecision::Allow);
        // And the idle notification fires on a quiescent subtree.
        assert!(delivered_child_idle_to_parent(&ctx.calls_made()));
    }

    #[tokio::test]
    async fn test_stop_dev_allows_when_not_ahead() {
        // Guard does not fire when not ahead of base (nothing committed beyond parent).
        let ctx = MockRuntime {
            is_clean: true,
            is_ahead: false,
            child_busy: false,
            ..Default::default()
        };
        assert_eq!(stop_dev(&ctx).await, StopDecision::Allow);
    }

    #[tokio::test]
    async fn test_stop_dev_allows_when_dirty() {
        // Dirty worktree → guard's is_clean check returns false → None → stop_notify path → Allow.
        let ctx = MockRuntime {
            is_clean: false,
            is_ahead: true,
            ..Default::default()
        };
        assert_eq!(stop_dev(&ctx).await, StopDecision::Allow);
    }

    #[tokio::test]
    async fn test_stop_dev_fail_open_on_git_error() {
        // Any git error in the guard → fail-open (Allow), never block.
        let ctx = MockRuntime {
            is_clean: true,
            is_ahead: true, // would trigger guard, but is_ahead_of fails
            ..MockRuntime::failing("is_ahead_of")
        };
        assert_eq!(stop_dev(&ctx).await, StopDecision::Allow);
    }

    #[tokio::test]
    async fn test_stop_tl_blocks_clean_ahead_unsubmitted() {
        // TL stop: clean + ahead + not submitted → Block (commit-guard fires after dirty-gate).
        let ctx = MockRuntime {
            is_clean: true,
            is_ahead: true,
            child_busy: false,
            ..Default::default()
        };
        match stop(&ctx).await {
            StopDecision::Block { reason } => {
                assert!(reason.contains("submit_branch"), "guidance should mention submit_branch");
            }
            _ => panic!("TL stop should block when clean+ahead+unsubmitted"),
        }
    }

    #[tokio::test]
    async fn test_stop_tl_allows_when_submitted() {
        // TL stop: clean + ahead + submitted → Allow (guard clears, proceeds to idle-notify).
        let ctx = MockRuntime {
            is_clean: true,
            is_ahead: true,
            child_busy: false,
            ..Default::default()
        };
        ctx.set("submit_branch_called", "true").await.unwrap();
        assert_eq!(stop(&ctx).await, StopDecision::Allow);
        assert!(delivered_child_idle_to_parent(&ctx.calls_made()));
    }
}
