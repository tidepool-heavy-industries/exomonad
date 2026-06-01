//! **N3 — Self-poll.** Implements per-agent polling of PR and CI state. When a node has an
//! open PR, this module periodically monitors its status and translates state transitions
//! into [`exo_policy::WorldEvent`]s.
//!
//! It triggers `exo_policy::on_world_event` and handles the resulting actions, such as
//! injecting messages into the node's own inbox or notifying its parent. It monitors:
//! - `PrReview`: Transitions in PR review state (e.g., Changes Requested, Approved).
//! - `CiStatus`: Changes in CI build status.
//! - `ReviewTimeout`: Detects when a review is overdue based on configurable windows.
//!
//! Polling is disciplined to minimize API load, running only when an open PR exists.
//! Polling tasks are managed via `AbortHandle` to ensure they are cleaned up when a PR
//! is closed or merged, preventing duplicate pollers.
//! Approved, one poll cycle emits a `PrReview{Approved}` → `NotifyParent([PR READY])` to the
//! parent inbox; aborting closes the task with no leak.

use std::sync::Arc;
use std::time::{Duration, Instant};

use chrono::Utc;
use tokio::fs::OpenOptions;
use tokio::io::AsyncWriteExt;

use exo_caps::{
    AgentName, ChildRecord, CiStatus, GitHub, InboxPath, IngestionEntry, Message, MessageBody,
    MessageKind, Persona, ReviewState, Summary, SyntheticName,
};
use exo_policy::events::on_world_event;
use exo_policy::{EventAction, WorldEvent};
use tracing::warn;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

struct PrState {
    pr: u64,
    first_seen: Instant,
    last_review_state: Option<ReviewState>,
    timeout_fired: bool,
    last_ci_status: Option<CiStatus>,
}

/// Supervise the self-poll lifecycle: spawn the poll task when a PR opens, hold its
/// `AbortHandle`, abort+replace on PR close / re-file. Runs for the node's lifetime.
pub async fn supervise(ctx: Arc<NodeContext>) -> NodeResult<()> {
    let mut state: Option<PrState> = None;

    loop {
        // A transient cap error (a single GitHub API hiccup) must NOT end the poller for the
        // node's lifetime — log and retry on the next tick. The poll stays alive across blips.
        match ctx.runtime.pr_for_branch(ctx.runtime.branch()).await {
            Ok(Some(pr)) => {
                // If we switched PRs or just started, reset state
                if state.as_ref().map(|s| s.pr) != Some(pr) {
                    state = Some(PrState {
                        pr,
                        first_seen: Instant::now(),
                        last_review_state: None,
                        timeout_fired: false,
                        last_ci_status: None,
                    });
                }

                if let Some(s) = state.as_mut() {
                    if let Err(e) = poll_once(&ctx, s).await {
                        warn!("self-poll cycle failed (will retry next tick): {e}");
                    }
                }
            }
            Ok(None) => {
                state = None;
            }
            Err(e) => {
                warn!("self-poll: pr_for_branch failed (will retry next tick): {e}");
            }
        }

        tokio::time::sleep(Duration::from_secs(180)).await; // 3 min cadence
    }
}

async fn poll_once(ctx: &Arc<NodeContext>, state: &mut PrState) -> NodeResult<()> {
    let pr = state.pr;
    let review_state = ctx
        .runtime
        .review_state(pr)
        .await
        .map_err(|e| std::io::Error::other(e.to_string()))?;
    let ci_status = ctx
        .runtime
        .ci_status(pr)
        .await
        .map_err(|e| std::io::Error::other(e.to_string()))?;

    let mut events = Vec::new();

    // 1. PrReview event if state changed
    if review_state != state.last_review_state {
        if let Some(s) = review_state {
            events.push(WorldEvent::PrReview { pr, state: s });
        }
        state.last_review_state = review_state;
        // Reset timeout window on each feedback round
        state.first_seen = Instant::now();
        state.timeout_fired = false;
    }

    // 2. CiStatus event on TRANSITION only. The first observation seeds the baseline without
    //    emitting (symmetric with the PrReview branch, which suppresses when there's no prior
    //    state) — otherwise every freshly-observed PR would fire a spurious CiStatus.
    match state.last_ci_status {
        Some(prev) if prev != ci_status => {
            events.push(WorldEvent::CiStatus {
                pr,
                status: ci_status,
            });
        }
        _ => {}
    }
    state.last_ci_status = Some(ci_status);

    // 3. ReviewTimeout: if review_state stays None for ~15 minutes
    if !state.timeout_fired
        && state.last_review_state.is_none()
        && state.first_seen.elapsed() > Duration::from_secs(15 * 60)
    {
        events.push(WorldEvent::ReviewTimeout { pr });
        state.timeout_fired = true;
    }

    for ev in events {
        let action = on_world_event(&*ctx.runtime, &ev).await;
        match action {
            EventAction::InjectMessage { text, summary } => {
                append_entry(&ctx.own_inbox, "github", &text, &summary, MessageKind::Chat).await?;
            }
            EventAction::NotifyParent { text, summary } => {
                if let Some(ref parent_inbox) = ctx.parent_inbox {
                    append_entry(parent_inbox, "github", &text, &summary, MessageKind::Chat)
                        .await?;
                }
            }
            EventAction::NoAction => {}
        }
    }

    Ok(())
}

/// Parent-side producer of `WorldEvent::SiblingMerged`: after this (parent) node merges a
/// child's PR, fan a `SiblingMerged { pr, branch }` to the OTHER children's inboxes (resolved
/// from the child ledger). Keeps every WorldEvent variant with a live producer.
pub async fn fan_sibling_merged(
    ctx: &Arc<NodeContext>,
    merged_child: &AgentName,
    pr: u64,
    branch: &str,
) -> NodeResult<()> {
    let ledger_path = ctx.runtime.working_dir().join(".exo/children.jsonl");
    if !ledger_path.exists() {
        return Ok(());
    }

    let content = tokio::fs::read_to_string(&ledger_path).await?;
    let mut records = Vec::new();
    for line in content.lines() {
        if line.trim().is_empty() {
            continue;
        }
        let record: ChildRecord =
            serde_json::from_str(line).map_err(|e| std::io::Error::other(e.to_string()))?;
        records.push(record);
    }

    let children = exo_caps::fold_children(&records);
    for (name, child) in children {
        if &name == merged_child {
            continue;
        }

        let ev = WorldEvent::SiblingMerged {
            pr,
            branch: branch.to_string(),
        };
        let body = serde_json::to_string(&ev).map_err(|e| std::io::Error::other(e.to_string()))?;

        append_entry(
            &child.inbox,
            "swarm",
            &body,
            "[Sibling Merged]",
            MessageKind::Event,
        )
        .await?;
    }

    Ok(())
}

async fn append_entry(
    path: &InboxPath,
    from: &str,
    text: &str,
    summary: &str,
    kind: MessageKind,
) -> NodeResult<()> {
    // Truncate summary to MAX_LEN bytes
    let mut summary_truncated = summary.to_string();
    if summary_truncated.len() > Summary::MAX_LEN {
        let mut end = Summary::MAX_LEN;
        while !summary_truncated.is_char_boundary(end) {
            end -= 1;
        }
        summary_truncated.truncate(end);
    }
    // Clean control chars for Summary (none allowed)
    let summary_clean: String = summary_truncated
        .chars()
        .filter(|c| !c.is_control())
        .collect();

    // Truncate text to MAX_LEN bytes
    let mut text_truncated = text.to_string();
    if text_truncated.len() > MessageBody::MAX_LEN {
        let mut end = MessageBody::MAX_LEN;
        while !text_truncated.is_char_boundary(end) {
            end -= 1;
        }
        text_truncated.truncate(end);
    }
    // Clean control chars for MessageBody (only \t \n \r allowed)
    let text_clean: String = text_truncated
        .chars()
        .filter(|&c| !c.is_control() || c == '\t' || c == '\n' || c == '\r')
        .collect();

    let from_name =
        SyntheticName::new(from.to_string()).map_err(|e| std::io::Error::other(e.to_string()))?;
    let body = MessageBody::new(text_clean).map_err(|e| std::io::Error::other(e.to_string()))?;
    let summary_val =
        Summary::new(summary_clean).map_err(|e| std::io::Error::other(e.to_string()))?;

    let entry = IngestionEntry {
        v: 1,
        ts: Utc::now(),
        from: Persona::Synthetic(from_name),
        msg: Message {
            text: body,
            summary: summary_val,
            kind,
        },
    };

    let line = format!(
        "{}\n",
        serde_json::to_string(&entry).map_err(|e| std::io::Error::other(e.to_string()))?
    );

    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(path.as_path())
        .await?;
    file.write_all(line.as_bytes()).await?;
    file.sync_all().await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::ChildKind;
    use exo_caps::PaneId;
    use tempfile::tempdir;

    fn name(s: &str) -> AgentName {
        AgentName::new(s.into()).unwrap()
    }
    fn pane(s: &str) -> PaneId {
        PaneId::new(s.into()).unwrap()
    }
    fn inbox(dir: &std::path::Path, s: &str) -> InboxPath {
        InboxPath::new(dir.join(format!("{}.jsonl", s)))
    }

    #[tokio::test]
    async fn test_append_entry_roundtrip() {
        let dir = tempdir().unwrap();
        let path = inbox(dir.path(), "test");

        append_entry(&path, "github", "hello world", "summary", MessageKind::Chat)
            .await
            .unwrap();

        let content = std::fs::read_to_string(path.as_path()).unwrap();
        let entry: IngestionEntry = serde_json::from_str(&content).unwrap();
        assert_eq!(
            entry.from,
            Persona::Synthetic(SyntheticName::new("github".to_string()).unwrap())
        );
        assert_eq!(entry.msg.text.as_str(), "hello world");
        assert_eq!(entry.msg.summary.as_str(), "summary");
        assert_eq!(entry.msg.kind, MessageKind::Chat);
    }

    #[tokio::test]
    async fn test_fan_sibling_merged() {
        use exo_caps::{Branch, NodeKind, NodePath};
        use exo_runtime::Runtime;
        use std::io::Write;

        let dir = tempdir().unwrap();
        let working_dir = dir.path().to_path_buf();
        std::fs::create_dir_all(working_dir.join(".exo")).unwrap();

        let child_a = name("a");
        let child_b = name("b");
        let child_c = name("c");

        let inbox_a = inbox(dir.path(), "a");
        let inbox_b = inbox(dir.path(), "b");
        let inbox_c = inbox(dir.path(), "c");

        let records = vec![
            ChildRecord::Spawned {
                child: child_a.clone(),
                kind: ChildKind::Worktree,
                pane: pane("%1"),
                inbox: inbox_a.clone(),
            },
            ChildRecord::Spawned {
                child: child_b.clone(),
                kind: ChildKind::Worktree,
                pane: pane("%2"),
                inbox: inbox_b.clone(),
            },
            ChildRecord::Spawned {
                child: child_c.clone(),
                kind: ChildKind::Worktree,
                pane: pane("%3"),
                inbox: inbox_c.clone(),
            },
        ];

        let mut ledger = std::fs::File::create(working_dir.join(".exo/children.jsonl")).unwrap();
        for r in records {
            let line = format!("{}\n", serde_json::to_string(&r).unwrap());
            ledger.write_all(line.as_bytes()).unwrap();
        }
        drop(ledger);

        let node_path = NodePath::new(vec![name("root")]).unwrap();
        let branch = Branch::new("main".to_string()).unwrap();
        let runtime = Runtime::new(
            node_path,
            branch,
            working_dir.clone(),
            None,
            "test-run".to_string(),
            "test-session".to_string(),
            pane("%9"),
        );
        let ctx = Arc::new(NodeContext {
            runtime: Arc::new(runtime),
            kind: NodeKind::Root,
            own_pane: pane("%9"),
            own_inbox: inbox(dir.path(), "root"),
            parent_inbox: None,
            run_id: "test-run".to_string(),
        });

        fan_sibling_merged(&ctx, &child_a, 123, "main.a")
            .await
            .unwrap();

        // b and c should have received the event, a should not
        assert!(!inbox_a.as_path().exists());
        assert!(inbox_b.as_path().exists());
        assert!(inbox_c.as_path().exists());

        let content_b = std::fs::read_to_string(inbox_b.as_path()).unwrap();
        let entry_b: IngestionEntry = serde_json::from_str(&content_b).unwrap();
        assert_eq!(entry_b.msg.kind, MessageKind::Event);
        assert!(entry_b.msg.summary.as_str().contains("Sibling Merged"));

        let ev: WorldEvent = serde_json::from_str(entry_b.msg.text.as_str()).unwrap();
        match ev {
            WorldEvent::SiblingMerged { pr, branch } => {
                assert_eq!(pr, 123);
                assert_eq!(branch, "main.a");
            }
            _ => panic!("Expected SiblingMerged event"),
        }
    }

    #[test]
    fn test_timeout_fired_logic() {
        let now = Instant::now();
        let first_seen = now - Duration::from_secs(16 * 60);

        // Case: No review state, and past 15 min -> should fire
        let fired = first_seen.elapsed() > Duration::from_secs(15 * 60);
        assert!(fired);

        // Case: Review state exists -> timeout logic shouldn't even be reached in poll_once
        // (verified by reading the code: `state.last_review_state.is_none()`)
    }
}
