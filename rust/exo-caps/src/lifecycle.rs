//! Child lifecycle — the parent's append-only `children.jsonl` records and the fold to
//! the parent's view of its children.
//!
//! This is the system's one "state machine", done **event-sourced**, not as typestate:
//! records are immutable facts; folding them yields state. That's deliberate — the
//! interesting transitions (running → exited) are driven by *external* OS events, so
//! they're computed **live** (pane-alive), never recorded. We only type the part that is
//! genuinely recorded: `Spawned` (parent logs the intent) then `Started` (child checks
//! in). See docs 04/07.

use crate::types::{AgentName, ChildKind, InboxPath, PaneId};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// One append-only lifecycle record. RECORDS — distinct from `MessageKind::Event`
/// world-events and from `Control` messages (the conflation the review caught).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "record", rename_all = "snake_case")]
pub enum ChildRecord {
    /// Appended by the **parent**, *before* it creates the pane (so there is never an
    /// untracked process). Carries exactly what the parent needs to address + tear the
    /// child down. Per the derivable-field rule (doc 07): `inbox` is **stored** (its
    /// derivation is scheme-coupled), while the child's `path` is **not** (derivable as
    /// `parent.path ++ child`, scheme-stable, and owned by the child's papers).
    Spawned {
        child: AgentName,
        kind: ChildKind,
        pane: PaneId,
        inbox: InboxPath,
    },
    /// Appended by the **child** on boot (its check-in). A `Spawned` with no matching
    /// `Started` after a timeout is a failed/ghost spawn → the parent reaps/retries.
    Started { child: AgentName },
}

impl ChildRecord {
    pub fn child(&self) -> &AgentName {
        match self {
            ChildRecord::Spawned { child, .. } | ChildRecord::Started { child } => child,
        }
    }
}

/// The **recorded** lifecycle phase of a child. Deliberately only two states — the ones
/// the parent actually records. Running-vs-exited is observed **live** (pane-alive) and
/// never stored, so it is *not* a phase here (the observe-don't-store rule: we do not
/// reintroduce a persisted run-state machine).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ChildLifecycle {
    /// Parent logged the spawn; child has not checked in (ghost-spawn candidate on timeout).
    Spawned,
    /// Child appended its boot check-in.
    Started,
}

/// A parent's handle on one child — the fold of its records. This is the "child-handle"
/// the `NodeRef` discussion landed on: the folded record + its recorded lifecycle, not a
/// separate floating type. (`agent_type` is absent on purpose — a *sender* never needs
/// it; the recipient picks its own last-hop.)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Child {
    pub name: AgentName,
    pub kind: ChildKind,
    pub pane: PaneId,
    pub inbox: InboxPath,
    pub lifecycle: ChildLifecycle,
}

/// Fold append-only records into the current child set. Newest `Spawned` wins (a retry
/// is a fresh append); a `Started` upgrades the lifecycle. A `Started` with no prior
/// `Spawned` is ignored (cannot occur when the parent logs spawn-first). Deterministic
/// order (by name).
pub fn fold_children(records: &[ChildRecord]) -> Vec<Child> {
    let mut map: BTreeMap<&str, Child> = BTreeMap::new();
    for r in records {
        match r {
            ChildRecord::Spawned {
                child,
                kind,
                pane,
                inbox,
            } => {
                map.insert(
                    child.as_str(),
                    Child {
                        name: child.clone(),
                        kind: *kind,
                        pane: pane.clone(),
                        inbox: inbox.clone(),
                        lifecycle: ChildLifecycle::Spawned,
                    },
                );
            }
            ChildRecord::Started { child } => {
                if let Some(c) = map.get_mut(child.as_str()) {
                    c.lifecycle = ChildLifecycle::Started;
                }
            }
        }
    }
    map.into_values().collect()
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
    fn fold_tracks_spawn_then_start() {
        let recs = vec![
            ChildRecord::Spawned {
                child: name("a"),
                kind: ChildKind::Worktree,
                pane: pane("%1"),
                inbox: inbox(),
            },
            ChildRecord::Spawned {
                child: name("b"),
                kind: ChildKind::Inline,
                pane: pane("%2"),
                inbox: inbox(),
            },
            ChildRecord::Started { child: name("a") },
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids.len(), 2);
        let a = kids.iter().find(|c| c.name.as_str() == "a").unwrap();
        assert_eq!(a.lifecycle, ChildLifecycle::Started);
        let b = kids.iter().find(|c| c.name.as_str() == "b").unwrap();
        assert_eq!(b.lifecycle, ChildLifecycle::Spawned); // ghost-spawn candidate
    }

    #[test]
    fn retry_overwrites_with_newest_spawn() {
        let recs = vec![
            ChildRecord::Spawned {
                child: name("a"),
                kind: ChildKind::Worktree,
                pane: pane("%1"),
                inbox: inbox(),
            },
            ChildRecord::Spawned {
                child: name("a"),
                kind: ChildKind::Worktree,
                pane: pane("%9"),
                inbox: inbox(),
            },
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids.len(), 1);
        assert_eq!(kids[0].pane.as_str(), "%9");
    }

    #[test]
    fn record_serde_is_tagged() {
        let r = ChildRecord::Started { child: name("a") };
        let json = serde_json::to_string(&r).unwrap();
        assert!(json.contains(r#""record":"started""#));
        let back: ChildRecord = serde_json::from_str(&json).unwrap();
        assert_eq!(r, back);
    }
}
