//! Converge e2e: a parent↔child message round-trip over the real filesystem bus.
//!
//! This exercises the REAL components — the `exo_runtime::Runtime` `Bus` impl (the append
//! side) wired end-to-end over a tempdir. The tmux/Teams last hops are out of scope here (no
//! live tmux in CI). What this asserts is that a child's `Bus::deliver` writes a real,
//! parseable bus entry into its parent's ingestion inbox, runtime-stamped with the sender's
//! identity (which policy cannot spoof).

use std::sync::Arc;

use exo_caps::{
    Addressee, AgentName, Branch, Bus, InboxPath, IngestionEntry, Message, MessageBody,
    MessageKind, NodePath, PaneId, Persona, Summary,
};
use exo_runtime::Runtime;

use exo_node::bootstrap::NodeContext;

mod common;

/// Build a real `NodeContext` rooted at `dir`, with a parent inbox the up-edge points at.
fn test_ctx(
    dir: &std::path::Path,
    parent_inbox: Option<InboxPath>,
) -> Arc<NodeContext<common::TestDomain>> {
    let own_pane = PaneId::new("%42".into()).unwrap();
    let run_id = "converge-run".to_string();
    let runtime = Runtime::new(
        NodePath::new(vec![
            AgentName::new("root".into()).unwrap(),
            AgentName::new("me".into()).unwrap(),
        ])
        .unwrap(),
        Branch::new("root.me".into()).unwrap(),
        dir.to_path_buf(),
        parent_inbox.clone(),
        run_id.clone(),
        "test-session".into(),
        own_pane.clone(),
    );
    let own_inbox = InboxPath::new(dir.join("own-inbox.jsonl"));
    Arc::new(NodeContext {
        runtime: Arc::new(runtime),
        kind: exo_caps::NodeKind::Tl,
        own_pane,
        own_inbox,
        parent_inbox,
        run_id,
        shutdown_pending: std::sync::Mutex::new(None),
        exited_children: std::sync::Mutex::new(std::collections::HashSet::new()),
    })
}

fn read_entries(path: &std::path::Path) -> Vec<IngestionEntry> {
    let content = std::fs::read_to_string(path).unwrap_or_default();
    content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l).expect("inbox line parses as IngestionEntry"))
        .collect()
}

/// A parent↔child message round-trip over the real filesystem bus: the child's `Bus::deliver`
/// (the `Runtime` append impl) writes the parent's ingestion inbox, and the entry reads back
/// with the runtime-stamped envelope (`from` = the child, not spoofable by policy).
#[tokio::test]
async fn message_round_trips_parent_inbox_over_the_bus() {
    let dir = tempfile::tempdir().unwrap();
    let parent_inbox = InboxPath::new(dir.path().join("parent-inbox.jsonl"));
    let ctx = test_ctx(dir.path(), Some(parent_inbox.clone()));

    ctx.runtime
        .deliver(
            Addressee::Parent,
            Message {
                text: MessageBody::new("wave 2 converged".into()).unwrap(),
                summary: Summary::new("status".into()).unwrap(),
                kind: MessageKind::Chat,
            },
        )
        .await
        .unwrap();

    let entries = read_entries(parent_inbox.as_path());
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].msg.text.as_str(), "wave 2 converged");
    // The runtime stamps `from` = this node's own name; policy can't spoof it.
    assert_eq!(entries[0].from, Persona::Agent(ctx.runtime.name()));
}
