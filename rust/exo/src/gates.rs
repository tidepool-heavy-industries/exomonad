//! Gates — `pre_tool_use` (antipattern nudges) and `session_start` (root identity bootstrap).
//! These are the concrete domain hook bodies: **functions generic over the caps they need** (no
//! `dyn Caps`); the [`RoleDef`](exo_framework::RoleDef) table stores them as `fn(&R, …) ->
//! BoxFuture<…>` monomorphized at the concrete runtime `R`, so the generic bound *is* the per-hook
//! least-privilege spec. The decision enums they return are the framework contract
//! ([`exo_framework::hooks`]).
//!
//! There used to be a third hook here, `stop` (Claude Code's `Stop` event), with per-role
//! variants (`stop_allow`/`stop_notify`/`stop_dev`/`stop_reviewer`). It was removed: `Stop` fires
//! on every turn-end, including a node legitimately yielding to wait on a backgrounded async task
//! (a reviewer polling a `cargo build`, say), so it cannot tell "genuinely done" from "paused" —
//! every decision built on it was wrong some of the time (reviewer-abort-and-kill on a 1-second
//! async hop; a dirty-worktree exit-block nagging a node mid-wait; a `ChildIdle` busy-bit that fed
//! `ChildLiveness` false "subtree idle" reports up the tree). See `rust/exo/CLAUDE.md` for the full
//! account. What replaced it:
//! - Reviewer "done" → the `verdict` tool (unchanged); "abandoned" → a wall-clock timeout,
//!   [`crate::review::handle_review_tick`], run by the sidecar's watchdog loop.
//! - "uncommitted work before converging" → already independently enforced by `submit_branch`'s own
//!   precondition check ([`crate::tools::submit`]) — no Stop-time backstop needed.
//! - "is my subtree still working" → [`exo_caps::ChildLiveness`] collapsed to pure pane-existence.

use exo_caps::Reason;
use exo_framework::{BoxFuture, HookDecision, HookInput, SessionStartOutput};

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
        if tool_name.as_str() == "Bash" {
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
                            reason: Reason::new("Avoid `git add -A`/`git add .` — stage specific files by path to avoid committing stray artifacts.".to_string()).unwrap(),
                        };
                    }
                }
            }
        }

        HookDecision::Allow
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
    use crate::testing::MockRuntime;
    use exo_caps::ToolName;
    use serde_json::json;

    #[tokio::test]
    async fn test_pre_tool_use_allow_by_default() {
        let ctx = MockRuntime::default();
        let input = HookInput {
            tool_name: ToolName::new("some_unknown_tool".into()).unwrap(),
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
                tool_name: ToolName::new(tool.into()).unwrap(),
                tool_input: json!({ "command": "git add ." }),
            };
            match pre_tool_use(&ctx, &input).await {
                HookDecision::Deny { reason } => {
                    assert!(reason.as_str().contains("Avoid `git add -A`/`git add .`"));
                }
                _ => panic!("Should be Deny for {} with 'git add .'", tool),
            }

            let input_a = HookInput {
                tool_name: ToolName::new(tool.into()).unwrap(),
                tool_input: json!({ "command": "git add -A" }),
            };
            match pre_tool_use(&ctx, &input_a).await {
                HookDecision::Deny { reason } => {
                    assert!(reason.as_str().contains("Avoid `git add -A`/`git add .`"));
                }
                _ => panic!("Should be Deny for {} with 'git add -A'", tool),
            }

            // Test with extra whitespace
            let input_ws = HookInput {
                tool_name: ToolName::new(tool.into()).unwrap(),
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
                tool_name: ToolName::new("Bash".into()).unwrap(),
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
