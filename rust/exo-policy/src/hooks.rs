//! Hooks — `pre_tool_use` (antipattern nudges), `stop` (the live PR-gate), and
//! `session_start` (root identity bootstrap). These are **functions generic over the caps
//! they need** (no `dyn Caps`); the [`RoleDef`](crate::roles::RoleDef) table stores them as
//! `fn(&R, …) -> BoxFuture<…>` monomorphized at the concrete runtime `R`, so the generic
//! bound *is* the per-hook least-privilege spec. See `docs/design/swarm/04-policy.md`.
//!
//! **Status: Wave-3 scaffold.** The decision enums + `HookInput` are the frozen contract;
//! P6 fills in `pre_tool_use` / `stop` / `session_start` (one per concern) with mock-cap
//! tests.

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::tool::BoxFuture;
use exo_caps::{Git, GitHub};

/// A `PreToolUse` verdict. `Modify` rewrites the tool input in place (the PII-rewrite path).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "decision")]
pub enum HookDecision {
    Allow,
    Deny { reason: String },
    Modify { input: Value },
}

/// A `Stop` verdict — the live PR-gate. `Block` keeps the agent in its turn-loop (e.g. an
/// open PR has unaddressed `ChangesRequested`); `Allow` lets it exit.
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

// TODO(cap): The stop hook needs to query the PR for the current branch. Currently
// this requires Git + GitHub bounds. If we add pr_for_current_branch() to GitHub,
// we can drop the Git bound.
pub fn stop<'a, R: Git + GitHub + Send + Sync>(ctx: &'a R) -> BoxFuture<'a, StopDecision> {
    Box::pin(async move {
        // The live PR-gate: block if there's an open PR with unaddressed changes.
        // Fail closed: block on error so the agent doesn't exit on transient failures.
        let branch = match ctx.current_branch().await {
            Ok(b) => b,
            Err(e) => {
                return StopDecision::Block {
                    reason: format!("Failed to get current branch: {}", e),
                }
            }
        };

        let pr = match ctx.pr_for_branch(&branch).await {
            Ok(Some(pr)) => pr,
            Ok(None) => return StopDecision::Allow,
            Err(e) => {
                return StopDecision::Block {
                    reason: format!("Failed to query PR for branch '{}': {}", branch.as_str(), e),
                }
            }
        };

        match ctx.has_unaddressed_changes(pr).await {
            Ok(true) => StopDecision::Block {
                reason: format!("Open PR #{} has unaddressed ChangesRequested", pr),
            },
            Ok(false) => StopDecision::Allow,
            Err(e) => StopDecision::Block {
                reason: format!("Failed to check PR #{} changes: {}", pr, e),
            },
        }
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
    use serde_json::json;

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
    async fn test_stop_allow_no_pr() {
        let ctx = MockRuntime::default();
        assert_eq!(stop(&ctx).await, StopDecision::Allow);
    }

    #[tokio::test]
    async fn test_stop_block_with_changes() {
        let ctx = MockRuntime {
            pr_for_branch: Some(123),
            has_unaddressed_changes: true,
            ..Default::default()
        };
        match stop(&ctx).await {
            StopDecision::Block { reason } => {
                assert!(reason.contains("Open PR #123 has unaddressed ChangesRequested"));
            }
            _ => panic!("Should be Block"),
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
