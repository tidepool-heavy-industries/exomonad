//! World events — the single typed event enum [`WorldEvent`] and the [`EventAction`] a
//! handler returns. `on_world_event` is generic over the caps it needs (`GitHub` to inspect
//! PR/CI state) and **returns** an action; the sidecar's inbound loop / self-poll delivers
//! it. See `docs/design/swarm/04-policy.md`.
//!
//! **Status: Wave-3 scaffold.** `WorldEvent` + `EventAction` are the frozen contract; P7
//! fills in `on_world_event` (porting the current poller/event-handler behavior) and the
//! `role_def(NodeKind)` table, with mock-cap tests.

use serde::{Deserialize, Serialize};

/// The one typed event enum. A `kind=event` ingestion entry has its body parsed into this
/// before `on_world_event` runs; the in-process self-poll constructs one directly. There is
/// **no** parallel `EventType` on the message envelope (`MessageKind::Event` is a bare tag).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "event")]
pub enum WorldEvent {
    /// A Copilot/human review landed on my PR.
    PrReview {
        pr: u64,
        state: ReviewState,
    },
    /// A sibling's PR merged — the parent fans this out to siblings that may need to rebase.
    SiblingMerged {
        pr: u64,
        branch: String,
    },
    /// CI transitioned on my PR.
    CiStatus {
        pr: u64,
        status: CiStatus,
    },
    /// No review arrived within the timeout window (≈15 min, resets on each feedback round).
    ReviewTimeout {
        pr: u64,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReviewState {
    Approved,
    ChangesRequested,
    Commented,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CiStatus {
    Passing,
    Failing,
    Pending,
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
