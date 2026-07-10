//! Child lifecycle — the parent's append-only `children.jsonl` records and the fold to
//! the parent's view of its children.
//!
//! This is the system's one "state machine", done **event-sourced**, not as typestate:
//! records are immutable facts; folding them yields state. That's deliberate — the
//! interesting transitions (running → exited) are driven by *external* OS events, so
//! they're computed **live** (pane-alive), never recorded. We only type the part that is
//! genuinely recorded: `Spawned` (parent logs the intent).

use crate::types::{AgentName, ChildKind, InboxPath, PaneId};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// One append-only lifecycle record. RECORDS — distinct from `MessageKind::Event`
/// world-events and from `Control` messages (the conflation the review caught). A single
/// variant today; the ledger stays a tag-dispatched enum on purpose so a future genuinely
/// recorded fact (e.g. a tombstone) is an added variant, not a wire-format break.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "record", rename_all = "snake_case")]
pub enum ChildRecord {
    /// Appended by the **parent**, *before* it creates the pane (so there is never an
    /// untracked process). Carries exactly what the parent needs to address + tear the
    /// child down. Per the derivable-field rule: `inbox` is **stored** (its
    /// derivation is scheme-coupled), while the child's `path` is **not** (derivable as
    /// `parent.path ++ child`, scheme-stable, and owned by the child's papers).
    Spawned {
        child: AgentName,
        kind: ChildKind,
        pane: PaneId,
        inbox: InboxPath,
        /// Non-secret cosmetic tag for a node whose Claude is launched on a non-default model via a
        /// [`launch_profile_env_prefix`](crate::RoleKind::launch_profile_env_prefix) (e.g. `"kimi"`).
        /// `None` for a default-Claude node. Surfaced in the tmux window + the `tree` tool; the
        /// auth token behind the redirect is **never** recorded here. Defaulted on read so existing
        /// ledgers parse.
        #[serde(default)]
        model_label: Option<String>,
    },
}

impl ChildRecord {
    pub fn child(&self) -> &AgentName {
        match self {
            ChildRecord::Spawned { child, .. } => child,
        }
    }
}

/// A parent's handle on one child — the fold of its records. This is the "child-handle"
/// the `NodeRef` discussion landed on: the folded record, not a separate floating type.
/// (`agent_type` is absent on purpose — a *sender* never needs it; the recipient picks its
/// own last-hop.)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Child {
    pub name: AgentName,
    pub kind: ChildKind,
    pub pane: PaneId,
    pub inbox: InboxPath,
    /// Cosmetic model tag (e.g. `"kimi"`) folded from the `Spawned` record; `None` for default Claude.
    pub model_label: Option<String>,
}

/// Fold append-only records into the current child set, **keyed by name** — uniqueness
/// is in the return type, and down-routing (find-child-by-`AgentName`) is an O(log n)
/// lookup. Newest `Spawned` wins (a retry is a fresh append).
pub fn fold_children(records: &[ChildRecord]) -> BTreeMap<AgentName, Child> {
    let mut map: BTreeMap<AgentName, Child> = BTreeMap::new();
    for r in records {
        match r {
            ChildRecord::Spawned {
                child,
                kind,
                pane,
                inbox,
                model_label,
            } => {
                map.insert(
                    child.clone(),
                    Child {
                        name: child.clone(),
                        kind: *kind,
                        pane: pane.clone(),
                        inbox: inbox.clone(),
                        model_label: model_label.clone(),
                    },
                );
            }
        }
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;

    fn name(s: &str) -> AgentName {
        AgentName::new(s.into()).unwrap()
    }
    fn pane(s: &str) -> PaneId {
        PaneId::new(s.into()).unwrap()
    }
    fn inbox() -> InboxPath {
        InboxPath::new("/tmp/x.jsonl".into())
    }

    #[test]
    fn fold_tracks_spawns() {
        let recs = vec![
            ChildRecord::Spawned {
                child: name("a"),
                kind: ChildKind::Worktree,
                pane: pane("%1"),
                inbox: inbox(),
                model_label: None,
            },
            ChildRecord::Spawned {
                child: name("b"),
                kind: ChildKind::Inline,
                pane: pane("%2"),
                inbox: inbox(),
                model_label: Some("kimi".into()),
            },
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids.len(), 2);
        // the cosmetic model tag folds through from the Spawned record
        assert_eq!(kids[&name("a")].model_label, None);
        assert_eq!(kids[&name("b")].model_label.as_deref(), Some("kimi"));
    }

    #[test]
    fn retry_overwrites_with_newest_spawn() {
        let recs = vec![
            ChildRecord::Spawned {
                child: name("a"),
                kind: ChildKind::Worktree,
                pane: pane("%1"),
                inbox: inbox(),
                model_label: None,
            },
            ChildRecord::Spawned {
                child: name("a"),
                kind: ChildKind::Worktree,
                pane: pane("%9"),
                inbox: inbox(),
                model_label: None,
            },
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids.len(), 1);
        assert_eq!(kids[&name("a")].pane.as_str(), "%9");
    }

    #[test]
    fn record_serde_is_tagged() {
        let r = ChildRecord::Spawned {
            child: name("a"),
            kind: ChildKind::Worktree,
            pane: pane("%1"),
            inbox: inbox(),
            model_label: None,
        };
        let json = serde_json::to_string(&r).unwrap();
        assert!(json.contains(r#""record":"spawned""#));
        let back: ChildRecord = serde_json::from_str(&json).unwrap();
        assert_eq!(r, back);
    }
}
