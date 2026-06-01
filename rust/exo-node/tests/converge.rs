//! Wave-2 converge e2e: the "every `WorldEvent` variant has a live producer, no dead
//! variant" gate (see `docs/design/swarm/06-migration.md`), plus a parent↔child message
//! round-trip over the real filesystem bus.
//!
//! These exercise the REAL components — `exo_policy::on_world_event` (the consumer), the
//! `exo_runtime::Runtime` `Bus` impl (the append side), and `exo_node::poll::fan_sibling_merged`
//! (the parent-side `SiblingMerged` producer) — wired end-to-end over a tempdir. The tmux/
//! GitHub last hops are out of scope here (no live tmux/network in CI); the self-poll's own
//! producer logic is unit-tested in `poll.rs`. What this asserts is that each variant maps to
//! a real action and that the producer paths write real, parseable bus entries.

use std::sync::Arc;

use exo_caps::{
    Addressee, AgentName, Branch, Bus, ChildKind, ChildRecord, InboxPath, IngestionEntry, Message,
    MessageBody, MessageKind, NodePath, PaneId, Persona, Summary,
};
use exo_policy::events::{on_world_event, EventAction, WorldEvent};
use exo_policy::{CiStatus, ReviewState};
use exo_runtime::Runtime;

use exo_node::bootstrap::NodeContext;

/// Build a real `NodeContext` rooted at `dir`, with a parent inbox the up-edge points at.
fn test_ctx(dir: &std::path::Path, parent_inbox: Option<InboxPath>) -> Arc<NodeContext> {
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

/// Gate part 1 — every `WorldEvent` variant maps to a real `EventAction` (no dead variant).
/// `on_world_event` is the single consumer; this proves each variant is *handled*, and the
/// producer sites (below + `poll.rs` unit tests) prove each is *emitted*.
#[tokio::test]
async fn every_world_event_variant_has_a_live_action() {
    let dir = tempfile::tempdir().unwrap();
    let ctx = test_ctx(dir.path(), None);
    let r = &*ctx.runtime;

    // PrReview(Approved) → NotifyParent [PR READY]
    let a = on_world_event(
        r,
        &WorldEvent::PrReview {
            pr: 1,
            state: ReviewState::Approved,
        },
    )
    .await;
    assert!(
        matches!(a, EventAction::NotifyParent { .. }),
        "PrReview(Approved) must notify parent"
    );

    // CiStatus(Failing) → NotifyParent [CI FAILING]
    let a = on_world_event(
        r,
        &WorldEvent::CiStatus {
            pr: 1,
            status: CiStatus::Failing,
        },
    )
    .await;
    assert!(
        matches!(a, EventAction::NotifyParent { .. }),
        "CiStatus(Failing) must notify parent"
    );

    // ReviewTimeout → NotifyParent [REVIEW TIMEOUT]
    let a = on_world_event(r, &WorldEvent::ReviewTimeout { pr: 1 }).await;
    assert!(
        matches!(a, EventAction::NotifyParent { .. }),
        "ReviewTimeout must notify parent"
    );

    // SiblingMerged → InjectMessage (rebase nudge into own conversation)
    let a = on_world_event(
        r,
        &WorldEvent::SiblingMerged {
            pr: 1,
            branch: "root.sibling".into(),
        },
    )
    .await;
    assert!(
        matches!(a, EventAction::InjectMessage { .. }),
        "SiblingMerged must inject a message"
    );
}

/// Gate part 2 — the parent-side `SiblingMerged` producer writes real bus entries to every
/// sibling except the one that merged (the producer that closes the "no dead variant" loop).
#[tokio::test]
async fn parent_fans_sibling_merged_to_other_children() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join(".exo")).unwrap();
    let ctx = test_ctx(dir.path(), None);

    // Seed a child ledger with three children a/b/c, each with its own inbox file.
    let mut ledger = String::new();
    for name in ["a", "b", "c"] {
        let rec = ChildRecord::Spawned {
            child: AgentName::new(name.into()).unwrap(),
            kind: ChildKind::Worktree,
            pane: PaneId::new(format!("%{}", 100 + name.as_bytes()[0] as u32)).unwrap(),
            inbox: InboxPath::new(dir.path().join(format!("{name}.jsonl"))),
        };
        ledger.push_str(&serde_json::to_string(&rec).unwrap());
        ledger.push('\n');
    }
    std::fs::write(dir.path().join(".exo/children.jsonl"), ledger).unwrap();

    // Child `a` merged → fan SiblingMerged to b and c, not a.
    exo_node::poll::fan_sibling_merged(&ctx, &AgentName::new("a".into()).unwrap(), 7, "root.a")
        .await
        .unwrap();

    let a = read_entries(&dir.path().join("a.jsonl"));
    let b = read_entries(&dir.path().join("b.jsonl"));
    let c = read_entries(&dir.path().join("c.jsonl"));
    assert!(
        a.is_empty(),
        "the merged child must NOT receive its own SiblingMerged"
    );
    assert_eq!(b.len(), 1, "sibling b receives one SiblingMerged");
    assert_eq!(c.len(), 1, "sibling c receives one SiblingMerged");

    // The fanned entry is a kind=event carrying a parseable WorldEvent::SiblingMerged.
    assert!(matches!(b[0].msg.kind, MessageKind::Event));
    let ev: WorldEvent = serde_json::from_str(b[0].msg.text.as_str()).unwrap();
    assert_eq!(
        ev,
        WorldEvent::SiblingMerged {
            pr: 7,
            branch: "root.a".into()
        }
    );
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
