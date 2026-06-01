//! **N4 — Hook mode.** `exomonad hook <event>` reads the CC hook payload on stdin, self-IDs
//! (same papers + exo-scry path as the sidecar), runs the role's `exo-policy` hook, and emits
//! the verdict on stdout. **No central server** — the hook is a short-lived process that
//! builds a `Runtime` and calls policy directly.
//!
//! - `pre_tool_use` → `HookDecision` (default-allow antipattern *nudge*, NOT a gate — C3).
//! - `stop` → `StopDecision` (the live PR-gate: open PR with unaddressed ChangesRequested).
//! - `session_start` → `SessionStartOutput`. **Must do the REAL papers-based root identity
//!   bootstrap** (currently a `default()` no-op): inject `additionalContext` describing the
//!   node's tree identity (role/path/parent) so a fresh agent knows who it is. See doc 01.
//!
//! Reads the role's hook fns from `exo_policy::role_def::<Runtime>(kind)`.
//!
//! **Status: stub (N4 leaf fills this).** Acceptance: a `pre_tool_use` payload for an
//! unrecognized tool → `{"decision":"allow"}`; a `session_start` for a non-root node →
//! `additionalContext` naming its role + parent.

use std::path::Path;

use crate::bootstrap::{bootstrap, NodeContext};
use crate::error::NodeResult;
use exo_policy::{role_def, HookDecision, HookInput, StopDecision};
use serde_json::json;

/// The CC hook events this mode handles (mirrors the policy hook fns).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HookEvent {
    PreToolUse,
    Stop,
    SessionStart,
}

/// Handle one CC hook invocation: read stdin payload, self-ID via `papers_path`, run the
/// role's policy hook, write the verdict JSON to stdout.
pub async fn handle(event: HookEvent, papers_path: &Path, stdin_json: &str) -> NodeResult<String> {
    // 1. Self-ID: call crate::bootstrap::bootstrap
    let ctx = bootstrap(papers_path, std::env::current_dir()?)?;

    // 2. Get the role's hook fns: let rd = exo_policy::role_def::<exo_runtime::Runtime>(ctx.kind);
    let rd = role_def::<exo_runtime::Runtime>(ctx.kind);

    run_hook(ctx, rd, event, stdin_json).await
}

fn identity_context(ctx: &NodeContext) -> String {
    let role = ctx.kind.role_str();
    let name = ctx.runtime.name();
    let branch = ctx.runtime.branch();
    let parent = ctx.runtime.node_path().parent();

    let parent_str = match parent {
        Some(p) => p.name().as_str().to_string(),
        None => "none (root)".to_string(),
    };

    format!(
        "You are exomonad node '{}' (role: {}) on branch '{}'. Parent: {}.",
        name.as_str(),
        role,
        branch.as_str(),
        parent_str
    )
}

async fn run_hook(
    ctx: NodeContext,
    rd: exo_policy::RoleDef<exo_runtime::Runtime>,
    event: HookEvent,
    stdin_json: &str,
) -> NodeResult<String> {
    match event {
        HookEvent::PreToolUse => {
            let input: HookInput = serde_json::from_str(stdin_json).map_err(|e| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("Failed to parse PreToolUse input: {}", e),
                )
            })?;
            let decision = (rd.pre_tool_use)(&ctx.runtime, &input).await;

            let output = match decision {
                HookDecision::Allow => json!({"continue": true}),
                HookDecision::Deny { reason } => json!({
                    "continue": true,
                    "systemMessage": reason
                }),
                HookDecision::Modify { input } => json!({
                    "continue": true,
                    "hookSpecificOutput": {
                        "hookEventName": "PreToolUse",
                        "toolInput": input
                    }
                }),
            };
            Ok(serde_json::to_string(&output).unwrap())
        }
        HookEvent::Stop => {
            let decision = (rd.stop)(&ctx.runtime).await;
            let output = match decision {
                StopDecision::Allow => json!({"continue": true}),
                StopDecision::Block { reason } => json!({
                    "decision": "block",
                    "reason": reason
                }),
            };
            Ok(serde_json::to_string(&output).unwrap())
        }
        HookEvent::SessionStart => {
            let policy_output = (rd.session_start)(&ctx.runtime).await;
            let id_ctx = identity_context(&ctx);

            let combined_context = match policy_output.additional_context {
                Some(p_ctx) => format!("{}\n\n{}", id_ctx, p_ctx),
                None => id_ctx,
            };

            let output = json!({
                "hookSpecificOutput": {
                    "hookEventName": "SessionStart",
                    "additionalContext": combined_context
                }
            });
            Ok(serde_json::to_string(&output).unwrap())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, Branch, NodeKind, NodePath, PaneId};
    use exo_runtime::Runtime;
    use serde_json::{json, Value};
    use std::sync::Arc;

    fn mock_ctx(kind: NodeKind, path: Vec<&str>, branch: &str, has_parent: bool) -> NodeContext {
        let node_path = NodePath::new(
            path.into_iter()
                .map(|s| AgentName::new(s.to_string()).unwrap())
                .collect(),
        )
        .unwrap();
        let branch = Branch::new(branch.to_string()).unwrap();
        let parent_inbox = if has_parent {
            Some(exo_caps::InboxPath::new("/tmp/parent".into()))
        } else {
            None
        };

        let runtime = Runtime::new(
            node_path,
            branch,
            "/tmp/work".into(),
            parent_inbox.clone(),
            "run-123".into(),
            "session-123".into(),
            PaneId::new("%1".into()).unwrap(),
        );

        NodeContext {
            runtime: Arc::new(runtime),
            kind,
            own_pane: PaneId::new("%1".into()).unwrap(),
            own_inbox: exo_caps::InboxPath::new("/tmp/own".into()),
            parent_inbox,
            run_id: "run-123".into(),
        }
    }

    #[test]
    fn test_identity_context() {
        let ctx = mock_ctx(
            NodeKind::Dev,
            vec!["root", "dev-node"],
            "main.root.dev-node",
            true,
        );
        let id = identity_context(&ctx);
        assert!(id.contains("dev-node"));
        assert!(id.contains("(role: dev)"));
        assert!(id.contains("on branch 'main.root.dev-node'"));
        assert!(id.contains("Parent: root"));

        let ctx_root = mock_ctx(NodeKind::Root, vec!["root"], "main", false);
        let id_root = identity_context(&ctx_root);
        assert!(id_root.contains("Parent: none (root)"));
    }

    #[tokio::test]
    async fn test_run_hook_pre_tool_use_unrecognized() {
        let ctx = mock_ctx(NodeKind::Dev, vec!["root", "dev-node"], "main", false);
        let rd = role_def::<exo_runtime::Runtime>(NodeKind::Dev);
        let stdin = json!({
            "tool_name": "unrecognized_tool",
            "tool_input": {}
        })
        .to_string();

        let res = run_hook(ctx, rd, HookEvent::PreToolUse, &stdin)
            .await
            .unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();
        assert_eq!(val, json!({"continue": true}));
    }

    #[tokio::test]
    async fn test_run_hook_session_start() {
        let ctx = mock_ctx(NodeKind::Dev, vec!["root", "dev-node"], "main", false);
        let rd = role_def::<exo_runtime::Runtime>(NodeKind::Dev);

        let res = run_hook(ctx, rd, HookEvent::SessionStart, "")
            .await
            .unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();

        let add_ctx = val["hookSpecificOutput"]["additionalContext"]
            .as_str()
            .unwrap();
        assert!(add_ctx.contains("dev-node"));
        assert!(add_ctx.contains("role: dev"));
    }
}
