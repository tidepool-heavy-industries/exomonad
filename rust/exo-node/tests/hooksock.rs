//! hooksock e2e: a real UDS hook-RPC round-trip — `client_request` ↔ the real `hooksock::serve`.
//!
//! Exercises the actual socket plumbing end-to-end over a tempdir: the sidecar's server binds its
//! per-agent socket, runs the role's hook fn on the live runtime, and shapes the verdict; the thin
//! client connects, sends a `HookRequest`, and reads the `HookVerdict` back. The injected test
//! roster's hooks allow/default (no git/bus dependency — this asserts the transport + shaping,
//! not the policy decisions, which are unit-tested in `exo`).

use std::sync::Arc;
use std::time::Duration;

use exo_caps::{AgentName, Branch, HookEvent, HookRequest, InboxPath, NodePath, PaneId};
use exo_node::bootstrap::NodeContext;
use exo_node::hooksock;
use exo_runtime::Runtime;

mod common;

fn test_ctx(dir: &std::path::Path, run_id: &str) -> Arc<NodeContext<common::TestDomain>> {
    let own_pane = PaneId::new("%7".into()).unwrap();
    let runtime = Runtime::new(
        NodePath::new(vec![AgentName::new("root".into()).unwrap()]).unwrap(),
        Branch::new("root".into()).unwrap(),
        dir.to_path_buf(),
        None,
        run_id.to_string(),
        "test-session".into(),
        own_pane.clone(),
        exo_caps::ChildKind::Worktree,
    );
    Arc::new(NodeContext {
        runtime: Arc::new(runtime),
        kind: common::TestRole::Root,
        own_pane,
        own_inbox: InboxPath::new(dir.join("own-inbox.jsonl")),
        parent_inbox: None,
        run_id: run_id.to_string(),
        shutdown_pending: std::sync::Mutex::new(None),
        exited_children: std::sync::Mutex::new(std::collections::HashSet::new()),
    })
}

#[tokio::test]
async fn hook_rpc_round_trips_over_uds() {
    let dir = tempfile::tempdir().unwrap();
    // The server binds `$HOME/.claude/exo/sockets/{run_id}/pane-N.sock`; isolate HOME to the
    // tempdir so the test owns its socket path (single test in this file → no env race).
    std::env::set_var("HOME", dir.path());
    let run_id = "hooksock-test";
    let ctx = test_ctx(dir.path(), run_id);
    let sock = exo_caps::paths::hook_sock(dir.path(), run_id, &ctx.own_pane);

    let server = tokio::spawn(hooksock::serve(ctx.clone()));

    // Wait for the server to bind (creates dirs + binds the socket).
    for _ in 0..100 {
        if sock.exists() {
            break;
        }
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    assert!(sock.exists(), "server never bound {}", sock.display());

    // PreToolUse on a shell call → the injected gate's `Deny`, shaped for Claude as a nudge
    // (continue + systemMessage). Asserts the transport's `Deny → nudge` shaping; the concrete
    // antipattern rules are unit-tested in `exo`.
    let v = hooksock::client_request(
        "root",
        &sock,
        &HookRequest {
            event: HookEvent::PreToolUse,
            stdin_json: serde_json::json!({
                "tool_name": "Bash",
                "tool_input": { "command": "git add ." }
            })
            .to_string(),
        },
    )
    .await
    .expect("pre_tool_use round-trip");
    assert!(
        v.stdout.contains(r#""continue":true"#) && v.stdout.contains("systemMessage"),
        "expected deny nudge shape, got {}",
        v.stdout
    );

    server.abort();
}
