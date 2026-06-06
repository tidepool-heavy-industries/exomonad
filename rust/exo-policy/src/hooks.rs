//! Hooks — `pre_tool_use` (antipattern nudges), `stop` (the live PR-gate), and
//! `session_start` (root identity bootstrap). These are **functions generic over the caps
//! they need** (no `dyn Caps`); the [`RoleDef`](crate::roles::RoleDef) table stores them as
//! `fn(&R, …) -> BoxFuture<…>` monomorphized at the concrete runtime `R`, so the generic
//! bound *is* the per-hook least-privilege spec.
//!
//! **Status: Wave-3 scaffold.** The decision enums + `HookInput` are the frozen contract;
//! P6 fills in `pre_tool_use` / `stop` / `session_start` (one per concern) with mock-cap
//! tests.

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::tool::BoxFuture;
use exo_caps::{
    Addressee, Bus, CapResult, Git, Log, Message, MessageBody, MessageKind, Summary, SystemMessage,
};

/// A `PreToolUse` verdict. `Modify` rewrites the tool input in place (the PII-rewrite path).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "decision")]
pub enum HookDecision {
    Allow,
    Deny { reason: String },
    Modify { input: Value },
}

/// A `Stop` verdict — the local convergence gate. `Block` keeps the agent in its turn-loop
/// (uncommitted work the parent's merge can't see); `Allow` lets it exit.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "decision")]
pub enum StopDecision {
    Allow,
    Block { reason: String },
}

/// A `SessionStart` outcome — the additional context injected into the agent's conversation
/// (the root identity bootstrap; empty for non-root). Mirrors CC's `additionalContext`.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SessionStartOutput {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub additional_context: Option<String>,
}

/// The parsed CC hook payload `pre_tool_use` inspects. A thin typed view over the fields the
/// guard/PII logic actually reads — the sidecar's `hook` mode parses the raw CC JSON into
/// this. (Wave-2 wiring firms the full field set; P6 needs `tool_name` + `tool_input`.)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HookInput {
    pub tool_name: String,
    #[serde(default)]
    pub tool_input: Value,
}

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
        // Antipattern: Avoid `git add .` or `git add -A`.
        // Appears in Gemini's `run_shell_command` and Claude's `Bash`.
        if tool_name == "run_shell_command" || tool_name == "Bash" {
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

/// Build the [`ChildIdle`](SystemMessage::ChildIdle) a non-root node delivers to its parent at
/// turn-end. Minimal by design (v1): a fixed human-readable summary the parent's `handle_system`
/// renders. Refinement (dedupe, richer state from the hook payload) lands parent-side later, not
/// by growing this message.
fn child_idle_message() -> CapResult<Message> {
    let summary = "finished a turn and is yielding control";
    Ok(Message {
        text: MessageBody::new(format!("[idle] {summary}"))?,
        summary: Summary::new(summary.into())?,
        kind: MessageKind::System(SystemMessage::ChildIdle {
            summary: summary.into(),
        }),
    })
}

/// Best-effort turn-end signal: deliver a `ChildIdle` to the parent. Logs and swallows any error
/// — a stop hook must never fail an agent's exit over a missed notification. Root has no parent,
/// so it never calls this (it uses `stop_allow`).
async fn notify_parent_idle<R: Bus + Log>(ctx: &R) {
    match child_idle_message() {
        Ok(msg) => {
            if let Err(e) = ctx.deliver(Addressee::Parent, msg).await {
                ctx.error(&format!("stop hook: failed to notify parent of idle: {e}"));
            }
        }
        Err(e) => ctx.error(&format!(
            "stop hook: could not build ChildIdle message: {e}"
        )),
    }
}

/// The local convergence gate for a spawned TL (v2 — no GitHub). A parent folds a child by
/// merging its **branch** off disk, so uncommitted work is invisible to that merge: block exit
/// while the worktree is dirty (commit or discard first). On a clean exit the node is yielding
/// control, so notify the parent it went idle. Fails OPEN on any git error — a hook must never
/// wedge an agent in its turn-loop (that bricks the session).
pub fn stop<'a, R: Git + Log + Bus + Send + Sync>(ctx: &'a R) -> BoxFuture<'a, StopDecision> {
    Box::pin(async move {
        match ctx.is_clean().await {
            Ok(true) => {
                notify_parent_idle(ctx).await;
                StopDecision::Allow
            }
            Ok(false) => StopDecision::Block {
                reason: "Uncommitted changes in your worktree. Commit your work (a parent merges \
                         your branch off disk — uncommitted changes are invisible to that merge), \
                         then stop."
                    .into(),
            },
            Err(e) => {
                ctx.error(&format!(
                    "stop gate: could not read git status, allowing exit: {e}"
                ));
                StopDecision::Allow
            }
        }
    })
}

/// Stop hook for nodes that never file a PR (root, worker): always allow exit — there is
/// nothing to gate on, and querying GitHub would be pointless (and could wedge). The root
/// especially must never be gated: blocking it bricks the human's session.
pub fn stop_allow<R: Send + Sync>(_ctx: &R) -> BoxFuture<'_, StopDecision> {
    Box::pin(async move { StopDecision::Allow })
}

/// Stop hook for Gemini leaves (dev, worker): notify the parent this node yielded control, then
/// ALWAYS allow exit. It NEVER blocks — Gemini's `AfterAgent` `deny` can infinite-loop
/// (gemini-cli #20426), so a Gemini role must not block at stop. The committed-before-fold
/// guarantee for a dev is enforced by `submit_branch`'s committed-check, not here; a worker is
/// inline with no branch to fold.
pub fn stop_notify<'a, R: Bus + Log + Send + Sync>(ctx: &'a R) -> BoxFuture<'a, StopDecision> {
    Box::pin(async move {
        notify_parent_idle(ctx).await;
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
                    if matches!(msg.kind, MessageKind::System(SystemMessage::ChildIdle { .. }))
            )
        })
    }

    #[tokio::test]
    async fn test_stop_notifies_parent_on_clean_allow() {
        let ctx = MockRuntime::default(); // is_clean = true
        assert_eq!(stop(&ctx).await, StopDecision::Allow);
        assert!(
            delivered_child_idle_to_parent(&ctx.calls_made()),
            "clean stop should notify parent of idle"
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
    async fn test_stop_notify_allows_and_notifies() {
        let ctx = MockRuntime::default();
        assert_eq!(stop_notify(&ctx).await, StopDecision::Allow);
        assert!(delivered_child_idle_to_parent(&ctx.calls_made()));
    }

    #[tokio::test]
    async fn test_stop_notify_never_blocks_even_if_deliver_fails() {
        let ctx = MockRuntime::failing("deliver");
        // Even when the bus delivery errors, a Gemini stop must allow exit (never block).
        assert_eq!(stop_notify(&ctx).await, StopDecision::Allow);
    }

    #[test]
    fn hook_decision_serde_is_tagged() {
        let d = HookDecision::Deny {
            reason: "blocked".into(),
        };
        let j = serde_json::to_value(&d).unwrap();
        assert_eq!(
            j,
            serde_json::json!({ "decision": "deny", "reason": "blocked" })
        );
        let back: HookDecision = serde_json::from_value(j).unwrap();
        assert_eq!(d, back);
    }

    #[test]
    fn session_start_empty_omits_context() {
        let j = serde_json::to_value(SessionStartOutput::default()).unwrap();
        assert_eq!(j, serde_json::json!({}));
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
        let tools = ["run_shell_command", "Bash"];

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
                tool_name: "run_shell_command".into(),
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
}
