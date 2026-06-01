//! World events — the single typed event enum [`WorldEvent`] and the [`EventAction`] a
//! handler returns. `on_world_event` is generic over the caps it needs (`GitHub` to inspect
//! PR/CI state) and **returns** an action; the sidecar's inbound loop / self-poll delivers
//! it. See `docs/design/swarm/04-policy.md`.
//!
//! **Status: Wave-3 scaffold.** `WorldEvent` + `EventAction` are the frozen contract; P7
//! fills in `on_world_event` (porting the current poller/event-handler behavior) and the
//! `role_def(NodeKind)` table, with mock-cap tests.

use serde::{Deserialize, Serialize};

use crate::BoxFuture;
pub use exo_caps::{CiStatus, GitHub, ReviewState};

/// The one typed event enum. A `kind=event` ingestion entry has its body parsed into this
/// before `on_world_event` runs; the in-process self-poll constructs one directly. There is
/// **no** parallel `EventType` on the message envelope (`MessageKind::Event` is a bare tag).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "event")]
pub enum WorldEvent {
    /// A Copilot/human review landed on my PR.
    PrReview { pr: u64, state: ReviewState },
    /// A sibling's PR merged — the parent fans this out to siblings that may need to rebase.
    SiblingMerged { pr: u64, branch: String },
    /// CI transitioned on my PR.
    CiStatus { pr: u64, status: CiStatus },
    /// No review arrived within the timeout window (≈15 min, resets on each feedback round).
    ReviewTimeout { pr: u64 },
}

/// What a world-event handler decides to do. The loop performs the IO: `InjectMessage` →
/// append to **own** inbox; `NotifyParent` → append to **parent** inbox; `NoAction` → drop.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "action")]
pub enum EventAction {
    InjectMessage { text: String, summary: String },
    NotifyParent { text: String, summary: String },
    NoAction,
}

pub fn on_world_event<'a, R: GitHub + Send + Sync>(
    _ctx: &'a R,
    e: &'a WorldEvent,
) -> BoxFuture<'a, EventAction> {
    Box::pin(async move {
        match e {
            WorldEvent::PrReview { pr, state } => match state {
                ReviewState::Approved => EventAction::NotifyParent {
                    text: format!(
                        "[PR READY] PR #{} approved by Copilot review. Merge with `merge_pr` tool.",
                        pr
                    ),
                    summary: "[PR READY]".to_string(),
                },
                ReviewState::ChangesRequested | ReviewState::Commented => EventAction::NoAction,
            },
            WorldEvent::SiblingMerged { pr: _, branch } => {
                let parent_branch = if let Some(last_dot) = branch.rfind('.') {
                    &branch[..last_dot]
                } else {
                    "main"
                };
                EventAction::InjectMessage {
                    text: format!(
                        "[Sibling Merged] PR on branch {} was merged into {}. Rebase your branch to pick up the changes: git fetch origin && git rebase origin/{}",
                        branch, parent_branch, parent_branch
                    ),
                    summary: "[Sibling Merged]".to_string(),
                }
            }
            WorldEvent::CiStatus { pr, status } => match status {
                CiStatus::Failing => EventAction::NotifyParent {
                    text: format!("[CI FAILING] PR #{} has failing CI status.", pr),
                    summary: "[CI FAILING]".to_string(),
                },
                CiStatus::Passing | CiStatus::Pending => EventAction::NoAction,
            },
            WorldEvent::ReviewTimeout { pr } => EventAction::NotifyParent {
                text: format!(
                    "[REVIEW TIMEOUT] PR #{} — no Copilot review after 15 minutes. Merge with `merge_pr` using `force: true`.",
                    pr
                ),
                summary: "[REVIEW TIMEOUT]".to_string(),
            },
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockRuntime;

    #[test]
    fn world_event_serde_round_trips() {
        let e = WorldEvent::PrReview {
            pr: 5,
            state: ReviewState::Approved,
        };
        let j = serde_json::to_value(&e).unwrap();
        assert_eq!(
            j,
            serde_json::json!({ "event": "pr_review", "pr": 5, "state": "approved" })
        );
        let back: WorldEvent = serde_json::from_value(j).unwrap();
        assert_eq!(e, back);
    }

    #[test]
    fn event_action_serde_is_tagged() {
        let a = EventAction::NoAction;
        assert_eq!(
            serde_json::to_value(&a).unwrap(),
            serde_json::json!({ "action": "no_action" })
        );
    }

    #[tokio::test]
    async fn test_on_world_event_approved() {
        let ctx = MockRuntime::default();
        let e = WorldEvent::PrReview {
            pr: 123,
            state: ReviewState::Approved,
        };
        let action = on_world_event(&ctx, &e).await;
        if let EventAction::NotifyParent { text, summary } = action {
            assert!(text.contains("[PR READY]"));
            assert!(text.contains("123"));
            assert_eq!(summary, "[PR READY]");
        } else {
            panic!("Expected NotifyParent, got {:?}", action);
        }
    }

    #[tokio::test]
    async fn test_on_world_event_timeout() {
        let ctx = MockRuntime::default();
        let e = WorldEvent::ReviewTimeout { pr: 123 };
        let action = on_world_event(&ctx, &e).await;
        if let EventAction::NotifyParent { text, summary } = action {
            assert!(text.contains("[REVIEW TIMEOUT]"));
            assert!(text.contains("123"));
            assert_eq!(summary, "[REVIEW TIMEOUT]");
        } else {
            panic!("Expected NotifyParent, got {:?}", action);
        }
    }

    #[tokio::test]
    async fn test_on_world_event_changes_requested() {
        let ctx = MockRuntime::default();
        let e = WorldEvent::PrReview {
            pr: 123,
            state: ReviewState::ChangesRequested,
        };
        let action = on_world_event(&ctx, &e).await;
        assert_eq!(action, EventAction::NoAction);
    }

    #[tokio::test]
    async fn test_mock_runtime_new_methods() {
        let ctx = MockRuntime {
            review_state: Some(ReviewState::Approved),
            ci_status: CiStatus::Failing,
            ..Default::default()
        };

        assert_eq!(
            ctx.review_state(123).await.unwrap(),
            Some(ReviewState::Approved)
        );
        assert_eq!(ctx.ci_status(123).await.unwrap(), CiStatus::Failing);
    }
}
