//! **N4 — Hook mode.** Implements the `exo hook` interface for Claude Code.
//! This module handles hook events by performing a papers-based identity bootstrap and
//! executing policy hooks directly in a short-lived process, without requiring a central server.
//!
//! Supported hooks:
//! - `pre_tool_use`: Evaluates tool use against policy, providing nudges or anti-pattern
//!   warnings.
//! - `session_start`: Performs the identity bootstrap, injecting context about the node's
//!   role, path, and parent into the agent's session so it understands its place in the swarm.
//!
//! There used to be a `stop` hook here (Claude Code's `Stop` event, a local convergence gate). It
//! was removed — `Stop` fires on every turn-end, including a node legitimately yielding to wait on
//! a backgrounded async task, so it can't distinguish "genuinely done" from "paused". See
//! `rust/exo/CLAUDE.md` for the full account and what replaced it.
//!
//! Policy logic is resolved from the injected [`RoleRegistry`] based on the node's role.

use std::path::Path;

use crate::bootstrap::{bootstrap, NodeContext};
use crate::error::NodeResult;
use exo_caps::RoleKind;
use exo_framework::{Exomonad, HookDecision, HookInput};
use exo_runtime::Runtime;
use serde_json::json;

/// The CC hook events this mode handles (mirrors the policy hook fns).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HookEvent {
    PreToolUse,
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

    run_hook(ctx, rd, event, papers_path, stdin_json).await
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

    // Messages from other agents arrive over the listen wake channel (see `listen_instruction`) —
    // exo owns its delivery channel end to end (CC Agent Teams native delivery was retired; a solo
    // session-lead never drains its teammate inbox as of CC 2.1.178, GH#26426). A child reaches
    // its parent via the `notify_parent` MCP tool; no team setup is needed.
    format!(
        "You are exomonad node '{}' (role: {}) on branch '{}'. Parent: {}.",
        name.as_str(),
        role,
        branch.as_str(),
        parent_str,
    )
}

/// Single-quote shell escaping for the papers path interpolated into the Monitor command (the
/// harness runs it in a shell). Local because `exo-node` links neither `exomonad-shared` nor the
/// `shell_escape` crate.
fn shell_escape_single(s: &str) -> String {
    format!("'{}'", s.replace('\'', r"'\''"))
}

/// The wake-channel arming instruction, appended to every SessionStart `additionalContext` (every
/// role including root — root gets no `--append-system-prompt`, so this is the one channel that
/// reaches it — and re-fired on every resume/clear, which is exactly the re-arm reminder needed).
///
/// The papers path is canonicalized to absolute: Monitor runs the command in a shell whose cwd is
/// the agent's worktree, and an inline worker's cwd is its *parent's* dir.
fn listen_instruction(papers_path: &Path) -> String {
    let papers = papers_path
        .canonicalize()
        .unwrap_or_else(|_| papers_path.to_path_buf());
    let cmd = exo_caps::invocation::listen_command(&shell_escape_single(
        papers.display().to_string().as_str(),
    ));
    format!(
        "WAKE CHANNEL (required) — your FIRST action, before anything else: run \
         ToolSearch(\"select:Monitor\") to load the Monitor tool, then arm it:\n\
         Monitor {{ command: \"{cmd}\", description: \"exo swarm inbox\", persistent: true }}\n\
         All swarm messages ([from: X, kind: Y] notes, sometimes a one-line @-file reference) \
         arrive as notifications from this monitor and wake you between turns. Until it is armed, \
         messages QUEUE durably and cannot reach you — and they drain the moment it connects. If \
         the watch ever ends (it reports this), re-arm it the same way; queued messages then drain."
    )
}

async fn run_hook<D: Exomonad>(
    ctx: NodeContext<D>,
    rd: exo_framework::RoleDef<Runtime>,
    event: HookEvent,
    papers_path: &Path,
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
                    "systemMessage": reason.as_str()
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
        HookEvent::SessionStart => {
            let policy_output = (rd.session_start)(&ctx.runtime).await;
            let id_ctx = identity_context(&ctx);

            let combined_context = match policy_output.additional_context {
                Some(p_ctx) => format!("{}\n\n{}", id_ctx, p_ctx),
                None => id_ctx,
            };
            let combined_context =
                format!("{}\n\n{}", combined_context, listen_instruction(papers_path));

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
    use crate::test_support::{test_session_start, TestDomain, TestRole};
    use exo_caps::{AgentName, Branch, NodePath, PaneId, Reason};
    use exo_framework::{BoxFuture, HookDecision, HookInput, RoleDef};
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
            listener: crate::listen::ListenerSlot::new(),
            inbox_wake: std::sync::Arc::new(tokio::sync::Notify::new()),
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
        // CC Agent Teams native delivery is retired — no node is told to create a team.
        assert!(
            !id.contains("TeamCreate"),
            "no TeamCreate instruction expected: {id}"
        );

        let ctx_root = mock_ctx(TestRole::Root, vec!["root"], "main", false);
        let id_root = identity_context(&ctx_root);
        assert!(id_root.contains("Parent: none (root)"));
        assert!(!id_root.contains("TeamCreate"));
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

        let res = run_hook(ctx, rd, HookEvent::PreToolUse, Path::new("/tmp/papers.json"), &stdin)
            .await
            .unwrap();
        let val: Value = serde_json::from_str(&res).unwrap();
        assert_eq!(val, json!({"continue": true}));
    }

    #[tokio::test]
    async fn test_run_hook_session_start() {
        let ctx = mock_ctx(TestRole::Dev, vec!["root", "dev-node"], "main", false);
        let rd = crate::test_support::test_role_def(TestRole::Dev);

        let res = run_hook(ctx, rd, HookEvent::SessionStart, Path::new("/tmp/papers.json"), "")
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
        // The wake-channel arming instruction IS appended, carrying the exact Monitor command.
        assert!(
            add_ctx.contains("exo listen --papers '/tmp/papers.json'"),
            "arm-Monitor instruction with the papers path expected: {add_ctx}"
        );
        assert!(add_ctx.contains("persistent: true"));
    }

    #[test]
    fn test_listen_instruction_escapes_papers_path() {
        let inst = listen_instruction(Path::new("/tmp/it's a dir/node.json"));
        assert!(inst.contains(r"exo listen --papers '/tmp/it'\''s a dir/node.json'"));
    }

    #[tokio::test]
    async fn test_run_hook_session_start_no_longer_appends_claude_protocol() {
        // A Claude node (Tl) no longer gets its role protocol appended to the session-start additionalContext
        // in the hook; it now receives it via the launch-time system prompt.
        let ctx = mock_ctx(TestRole::Tl, vec!["root", "tl-node"], "main", true);
        let rd = crate::test_support::test_role_def(TestRole::Tl);

        let res = run_hook(ctx, rd, HookEvent::SessionStart, Path::new("/tmp/papers.json"), "")
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

    fn mock_pre_tool_deny<'a>(
        _: &'a exo_runtime::Runtime,
        _: &'a HookInput,
    ) -> BoxFuture<'a, HookDecision> {
        Box::pin(async {
            HookDecision::Deny {
                reason: Reason::new("test deny".into()).unwrap(),
            }
        })
    }

    #[tokio::test]
    async fn test_run_hook_pre_tool_deny() {
        let ctx = mock_ctx(TestRole::Dev, vec!["root", "dev"], "main", false);
        let rd = RoleDef {
            tools: vec![],
            pre_tool_use: mock_pre_tool_deny,
            session_start: test_session_start,
        };
        let stdin = json!({
            "tool_name": "any",
            "tool_input": {}
        })
        .to_string();

        let res = run_hook(ctx, rd, HookEvent::PreToolUse, Path::new("/tmp/papers.json"), &stdin)
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
            session_start: test_session_start,
        };
        let stdin = json!({
            "tool_name": "any",
            "tool_input": {}
        })
        .to_string();

        let res = run_hook(ctx, rd, HookEvent::PreToolUse, Path::new("/tmp/papers.json"), &stdin)
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
