//! **N4 — Hook mode.** Implements the `exomonad experimental hook` interface for Claude Code.
//! This module handles hook events by performing a papers-based identity bootstrap and
//! executing policy hooks directly in a short-lived process, without requiring a central server.
//!
//! Supported hooks:
//! - `pre_tool_use`: Evaluates tool use against policy, providing nudges or anti-pattern
//!   warnings.
//! - `stop`: Implements the PR-gate, blocking session termination if there is an open PR
//!   with unaddressed changes.
//! - `session_start`: Performs the identity bootstrap, injecting context about the node's
//!   role, path, and parent into the agent's session so it understands its place in the swarm.
//!
//! Policy logic is dynamically resolved from `exo_policy::role_def` based on the node's role.

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
        "You are exomonad node '{}' (role: {}) on branch '{}'. Parent: {}.\n\
         If you do not already lead a team, create one now with the TeamCreate tool — that is \
         how messages from other agents reach you, delivered as native teammate-messages. Do \
         this before other work.",
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
    use exo_policy::{HookDecision, HookInput, RoleDef, StopDecision};
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
        assert!(id.contains("TeamCreate"));

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

    fn mock_stop_block<'a>(
        _: &'a exo_runtime::Runtime,
    ) -> exo_policy::tool::BoxFuture<'a, StopDecision> {
        Box::pin(async {
            StopDecision::Block {
                reason: "test reason".into(),
            }
        })
    }

    #[tokio::test]
    async fn test_run_hook_stop_blocked() {
        let ctx = mock_ctx(NodeKind::Dev, vec!["root", "dev"], "main", false);
        let rd = RoleDef {
            tools: vec![],
            pre_tool_use: exo_policy::hooks::pre_tool_use,
            stop: mock_stop_block,
            session_start: exo_policy::hooks::session_start,
            on_event: exo_policy::events::on_world_event,
        };

        let res = run_hook(ctx, rd, HookEvent::Stop, "").await.unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();
        assert_eq!(val["decision"], "block");
        assert_eq!(val["reason"], "test reason");
    }

    fn mock_pre_tool_deny<'a>(
        _: &'a exo_runtime::Runtime,
        _: &'a HookInput,
    ) -> exo_policy::tool::BoxFuture<'a, HookDecision> {
        Box::pin(async {
            HookDecision::Deny {
                reason: "test deny".into(),
            }
        })
    }

    #[tokio::test]
    async fn test_run_hook_pre_tool_deny() {
        let ctx = mock_ctx(NodeKind::Dev, vec!["root", "dev"], "main", false);
        let rd = RoleDef {
            tools: vec![],
            pre_tool_use: mock_pre_tool_deny,
            stop: exo_policy::hooks::stop,
            session_start: exo_policy::hooks::session_start,
            on_event: exo_policy::events::on_world_event,
        };
        let stdin = json!({
            "tool_name": "any",
            "tool_input": {}
        })
        .to_string();

        let res = run_hook(ctx, rd, HookEvent::PreToolUse, &stdin)
            .await
            .unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();
        assert_eq!(val["continue"], true);
        assert_eq!(val["systemMessage"], "test deny");
    }

    fn mock_pre_tool_modify<'a>(
        _: &'a exo_runtime::Runtime,
        _: &'a HookInput,
    ) -> exo_policy::tool::BoxFuture<'a, HookDecision> {
        Box::pin(async {
            HookDecision::Modify {
                input: json!({"modified": true}),
            }
        })
    }

    #[tokio::test]
    async fn test_run_hook_pre_tool_modify() {
        let ctx = mock_ctx(NodeKind::Dev, vec!["root", "dev"], "main", false);
        let rd = RoleDef {
            tools: vec![],
            pre_tool_use: mock_pre_tool_modify,
            stop: exo_policy::hooks::stop,
            session_start: exo_policy::hooks::session_start,
            on_event: exo_policy::events::on_world_event,
        };
        let stdin = json!({
            "tool_name": "any",
            "tool_input": {}
        })
        .to_string();

        let res = run_hook(ctx, rd, HookEvent::PreToolUse, &stdin)
            .await
            .unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();
        assert_eq!(val["continue"], true);
        assert_eq!(
            val["hookSpecificOutput"]["toolInput"],
            json!({"modified": true})
        );
    }
}
