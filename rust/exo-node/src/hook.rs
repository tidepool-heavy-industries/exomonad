//! **N4 — Hook mode.** Implements the `exo hook` interface for Claude Code.
//! This module handles hook events by performing a papers-based identity bootstrap and
//! executing policy hooks directly in a short-lived process, without requiring a central server.
//!
//! Supported hooks:
//! - `pre_tool_use`: Evaluates tool use against policy, providing nudges or anti-pattern
//!   warnings.
//! - `stop`: Implements the local convergence gate, blocking session termination while the
//!   worktree has uncommitted changes (a parent merges the branch off disk).
//! - `session_start`: Performs the identity bootstrap, injecting context about the node's
//!   role, path, and parent into the agent's session so it understands its place in the swarm.
//!
//! Policy logic is resolved from the injected [`RoleRegistry`] based on the node's role.

use std::path::Path;

use crate::bootstrap::{bootstrap, NodeContext};
use crate::error::NodeResult;
use exo_caps::RoleKind;
use exo_framework::{Exomonad, HookDecision, HookInput, StopDecision};
use exo_runtime::Runtime;
use serde_json::json;

/// The CC hook events this mode handles (mirrors the policy hook fns).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HookEvent {
    PreToolUse,
    Stop,
    SessionStart,
}

/// Handle one CC hook invocation: read stdin payload, self-ID via `papers_path`, run the
/// role's policy hook (resolved through the domain `D`), write the verdict JSON to stdout.
pub async fn handle<D: Exomonad<Caps = Runtime>>(
    event: HookEvent,
    papers_path: &Path,
    stdin_json: &str,
) -> NodeResult<String> {
    // 1. Self-ID against the domain `D`.
    let ctx = bootstrap::<D>(papers_path, std::env::current_dir()?)?;

    // 2. Resolve the role's hook fns through the domain.
    let rd = D::role_def(ctx.kind);

    run_hook(ctx, rd, event, stdin_json).await
}

fn identity_context<D: Exomonad>(ctx: &NodeContext<D>) -> String {
    let role = ctx.kind.role_str();
    let name = ctx.runtime.name();
    let branch = ctx.runtime.branch();
    let parent = ctx.runtime.node_path().parent();

    let parent_str = match parent {
        Some(p) => p.name().as_str().to_string(),
        None => "none (root)".to_string(),
    };

    // Claude nodes lead a solo team so the Bus's last hop can deliver native <teammate-message>s.
    // Team names are a GLOBAL namespace, so the agent picking its own collides across re-runs /
    // siblings → TeamCreate fails → the node leads no team → delivery degrades to tmux paste.
    // Mint a run-scoped unique name here instead. Every tree node is a Claude instance; a non-Claude
    // companion (Shoal) gets no team instruction (it receives via paste).
    let team_line = if ctx.kind.agent_type() == exo_caps::AgentType::Claude && !ctx.runtime.is_inline() {
        let run8: String = ctx.run_id.chars().take(8).collect();
        let safe_name: String = name
            .as_str()
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                    c
                } else {
                    '-'
                }
            })
            .collect();
        format!(
            "\nIf you do not already lead a team, create one named exactly `exo-{safe_name}-{run8}` \
             with the TeamCreate tool — that is how messages from other agents reach you as native \
             teammate-messages. Do this before other work."
        )
    } else {
        String::new()
    };

    format!(
        "You are exomonad node '{}' (role: {}) on branch '{}'. Parent: {}.{}",
        name.as_str(),
        role,
        branch.as_str(),
        parent_str,
        team_line
    )
}

async fn run_hook<D: Exomonad>(
    ctx: NodeContext<D>,
    rd: exo_framework::RoleDef<Runtime>,
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
    use crate::test_support::{
        test_pre_tool_use, test_session_start, test_stop, TestDomain, TestRole,
    };
    use exo_caps::{AgentName, Branch, NodePath, PaneId};
    use exo_framework::{BoxFuture, HookDecision, HookInput, RoleDef, StopDecision};
    use exo_runtime::Runtime;
    use serde_json::{json, Value};
    use std::sync::Arc;

    fn mock_ctx(
        kind: TestRole,
        path: Vec<&str>,
        branch: &str,
        has_parent: bool,
    ) -> NodeContext<TestDomain> {
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
            exo_caps::ChildKind::Worktree,
        );

        NodeContext {
            runtime: Arc::new(runtime),
            kind,
            own_pane: PaneId::new("%1".into()).unwrap(),
            own_inbox: exo_caps::InboxPath::new("/tmp/own".into()),
            parent_inbox,
            run_id: "run-123".into(),
            shutdown_pending: std::sync::Mutex::new(None),
            exited_children: std::sync::Mutex::new(std::collections::HashSet::new()),
        }
    }

    #[test]
    fn test_identity_context() {
        let ctx = mock_ctx(
            TestRole::Dev,
            vec!["root", "dev-node"],
            "main.root.dev-node",
            true,
        );
        let id = identity_context(&ctx);
        assert!(id.contains("dev-node"));
        assert!(id.contains("(role: dev)"));
        assert!(id.contains("on branch 'main.root.dev-node'"));
        assert!(id.contains("Parent: root"));
        // Dev is a Claude instance now — it leads a solo team so its parent can reach it natively.
        assert!(
            id.contains("TeamCreate"),
            "Claude dev must get a team instruction: {id}"
        );
        assert!(
            id.contains("exo-dev-node-run-123"),
            "expected run-scoped team name: {id}"
        );

        // Tl also leads a solo team with a run-scoped, globally-unique name (`run-123`[..8]).
        let ctx_tl = mock_ctx(
            TestRole::Tl,
            vec!["root", "tl-node"],
            "main.root.tl-node",
            true,
        );
        let id_tl = identity_context(&ctx_tl);
        assert!(id_tl.contains("TeamCreate"));
        assert!(
            id_tl.contains("exo-tl-node-run-123"),
            "expected run-scoped team name: {id_tl}"
        );

        let ctx_root = mock_ctx(TestRole::Root, vec!["root"], "main", false);
        let id_root = identity_context(&ctx_root);
        assert!(id_root.contains("Parent: none (root)"));
        // Root is Claude too — it leads a team so children's ChildIdle can be delivered natively.
        assert!(id_root.contains("exo-root-run-123"));
    }

    #[tokio::test]
    async fn test_run_hook_pre_tool_use_unrecognized() {
        let ctx = mock_ctx(TestRole::Dev, vec!["root", "dev-node"], "main", false);
        let rd = crate::test_support::test_role_def(TestRole::Dev);
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
        let ctx = mock_ctx(TestRole::Dev, vec!["root", "dev-node"], "main", false);
        let rd = crate::test_support::test_role_def(TestRole::Dev);

        let res = run_hook(ctx, rd, HookEvent::SessionStart, "")
            .await
            .unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();

        let add_ctx = val["hookSpecificOutput"]["additionalContext"]
            .as_str()
            .unwrap();
        assert!(add_ctx.contains("dev-node"));
        assert!(add_ctx.contains("role: dev"));
        // A role's protocol is delivered via the launch-time --append-system-prompt, NEVER appended
        // to the session-start additionalContext.
        assert!(
            !add_ctx.contains("TEST-DEV-PROTOCOL-MARKER"),
            "role protocol must not be appended to additionalContext: {add_ctx}"
        );
    }

    #[tokio::test]
    async fn test_run_hook_session_start_no_longer_appends_claude_protocol() {
        // A Claude node (Tl) no longer gets its role protocol appended to the session-start additionalContext
        // in the hook; it now receives it via the launch-time system prompt.
        let ctx = mock_ctx(TestRole::Tl, vec!["root", "tl-node"], "main", true);
        let rd = crate::test_support::test_role_def(TestRole::Tl);

        let res = run_hook(ctx, rd, HookEvent::SessionStart, "")
            .await
            .unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();
        let add_ctx = val["hookSpecificOutput"]["additionalContext"]
            .as_str()
            .unwrap();

        assert!(add_ctx.contains("tl-node"));
        assert!(
            !add_ctx.contains("TEST-TL-PROTOCOL-MARKER"),
            "Protocol must NOT be appended in the hook: {add_ctx}"
        );
    }

    fn mock_stop_block<'a>(_: &'a exo_runtime::Runtime) -> BoxFuture<'a, StopDecision> {
        Box::pin(async {
            StopDecision::Block {
                reason: "test reason".into(),
            }
        })
    }

    #[tokio::test]
    async fn test_run_hook_stop_blocked() {
        let ctx = mock_ctx(TestRole::Dev, vec!["root", "dev"], "main", false);
        let rd = RoleDef {
            tools: vec![],
            pre_tool_use: test_pre_tool_use,
            stop: mock_stop_block,
            session_start: test_session_start,
        };

        let res = run_hook(ctx, rd, HookEvent::Stop, "").await.unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();
        assert_eq!(val["decision"], "block");
        assert_eq!(val["reason"], "test reason");
    }

    fn mock_pre_tool_deny<'a>(
        _: &'a exo_runtime::Runtime,
        _: &'a HookInput,
    ) -> BoxFuture<'a, HookDecision> {
        Box::pin(async {
            HookDecision::Deny {
                reason: "test deny".into(),
            }
        })
    }

    #[tokio::test]
    async fn test_run_hook_pre_tool_deny() {
        let ctx = mock_ctx(TestRole::Dev, vec!["root", "dev"], "main", false);
        let rd = RoleDef {
            tools: vec![],
            pre_tool_use: mock_pre_tool_deny,
            stop: test_stop,
            session_start: test_session_start,
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
    ) -> BoxFuture<'a, HookDecision> {
        Box::pin(async {
            HookDecision::Modify {
                input: json!({"modified": true}),
            }
        })
    }

    #[tokio::test]
    async fn test_run_hook_pre_tool_modify() {
        let ctx = mock_ctx(TestRole::Dev, vec!["root", "dev"], "main", false);
        let rd = RoleDef {
            tools: vec![],
            pre_tool_use: mock_pre_tool_modify,
            stop: test_stop,
            session_start: test_session_start,
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
