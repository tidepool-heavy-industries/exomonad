//! Child lifecycle — the parent's append-only `children.jsonl` records and the fold to
//! the parent's view of its children.
//!
//! This is the system's one "state machine", done **event-sourced**, not as typestate:
//! records are immutable facts; folding them yields [`ChildState`]. The vocabulary covers
//! exactly what is genuinely *recorded*: what the parent DID (`Spawned`, `Reaped`), what it
//! OBSERVED once and must not re-observe (`Died` — the record is the dedup guard), and what a
//! child REPORTED that must outlive a context window (`Submitted`). Liveness of a still-live
//! child stays computed **live** (pane-alive), never written back — a tombstone is written only
//! when the child is gone for good.
//!
//! A tombstoned child never vanishes from the fold: the parent still needs to see it (a `Died`
//! child may hold unmerged work), and every consumer that resolves/probes a child asks
//! [`ChildState::is_terminal`] first — pane ids get recycled by tmux, so probing a dead child's
//! recorded pane can alias onto a *different* live agent.

use crate::types::{AgentName, Branch, ChildKind, InboxPath, PaneId};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// One append-only lifecycle record. RECORDS — distinct from `MessageKind::Event` world-events
/// and from `Control` messages. The ledger is a tag-dispatched enum on purpose: a new genuinely
/// recorded fact is an added variant, and because every field the older variants gained is
/// `#[serde(default)]`, an older `children.jsonl` line keeps parsing verbatim.
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
        /// The **effective** launch model (e.g. `"sonnet"`, `"kimi-for-coding"`) — what the child's
        /// Claude was actually started with, after the launch-profile-over-role-default precedence.
        /// Stamped by `birth_finish`. `None` on a pre-field ledger line.
        #[serde(default)]
        model: Option<String>,
        /// Hash of the directives bundle the child was launched with. The runtime stamps whatever
        /// the spawn spec carries; nothing computes one yet, so this is `None` today.
        #[serde(default)]
        directives_hash: Option<String>,
    },
    /// The parent tore this child down — appended by the **runtime** teardown paths
    /// (`Spawner::kill_pane` / `Spawner::reclaim_worktree`), never by a tool.
    Reaped {
        child: AgentName,
        #[serde(default)]
        at: Option<DateTime<Utc>>,
    },
    /// The watchdog observed the child's pane gone while the child was still un-reaped. Written
    /// **at most once** per child — the record itself is the dedup guard (a terminal child is
    /// excluded from every later death scan).
    Died {
        child: AgentName,
        pane: PaneId,
        #[serde(default)]
        at: Option<DateTime<Utc>>,
    },
    /// The child submitted `branch@sha` and is waiting for THIS node to merge it. Appended by the
    /// parent's sidecar when a `Lifecycle::Submitted` arrives, so the pending-merge queue survives
    /// a context window.
    Submitted {
        child: AgentName,
        branch: Branch,
        sha: String,
        #[serde(default)]
        reviewed: bool,
        #[serde(default)]
        at: Option<DateTime<Utc>>,
    },
}

impl ChildRecord {
    pub fn child(&self) -> &AgentName {
        match self {
            ChildRecord::Spawned { child, .. }
            | ChildRecord::Reaped { child, .. }
            | ChildRecord::Died { child, .. }
            | ChildRecord::Submitted { child, .. } => child,
        }
    }
}

/// The folded lifecycle state of one child. `Live` is the default at birth; `Submitted` is still
/// live (the child's pane is up, it's waiting on its parent's merge); `Reaped`/`Died` are
/// **tombstones** — the child is gone and its recorded pane must never be probed again.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum ChildState {
    Live,
    Submitted { sha: String, reviewed: bool },
    Reaped,
    Died,
}

impl ChildState {
    /// The child is gone: no pane probe, no status file, no delivery.
    pub fn is_terminal(&self) -> bool {
        matches!(self, ChildState::Reaped | ChildState::Died)
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
    /// The effective launch model folded from the `Spawned` record.
    pub model: Option<String>,
    /// Hash of the directives bundle this child was launched with, folded from the `Spawned`
    /// record; `None` when the child was spawned without directives.
    pub directives_hash: Option<String>,
    /// Where this child is in its lifecycle — see [`ChildState`].
    pub state: ChildState,
}

/// Fold append-only records into the current child set, **keyed by name** — uniqueness
/// is in the return type, and down-routing (find-child-by-`AgentName`) is an O(log n)
/// lookup.
///
/// Records apply **in order**. A `Spawned` inserts a fresh `Live` child (newest spawn wins; a
/// respawn under the same name resets its state). `Submitted`/`Reaped`/`Died` mutate the existing
/// entry's state, and a later record simply overwrites an earlier one — so the benign race
/// "watchdog wrote `Died` a moment before the runtime wrote `Reaped`" self-heals to `Reaped`.
/// A state record naming an unknown child is skipped silently: this is a tolerant pure function
/// (no tracing lives in `exo-caps`), and the caller's ledger is the only thing that could be torn.
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
                model,
                directives_hash,
            } => {
                map.insert(
                    child.clone(),
                    Child {
                        name: child.clone(),
                        kind: *kind,
                        pane: pane.clone(),
                        inbox: inbox.clone(),
                        model_label: model_label.clone(),
                        model: model.clone(),
                        directives_hash: directives_hash.clone(),
                        state: ChildState::Live,
                    },
                );
            }
            ChildRecord::Submitted {
                child,
                sha,
                reviewed,
                ..
            } => {
                if let Some(c) = map.get_mut(child) {
                    c.state = ChildState::Submitted {
                        sha: sha.clone(),
                        reviewed: *reviewed,
                    };
                }
            }
            ChildRecord::Reaped { child, .. } => {
                if let Some(c) = map.get_mut(child) {
                    c.state = ChildState::Reaped;
                }
            }
            ChildRecord::Died { child, .. } => {
                if let Some(c) = map.get_mut(child) {
                    c.state = ChildState::Died;
                }
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
    fn spawned(n: &str, kind: ChildKind, p: &str) -> ChildRecord {
        ChildRecord::Spawned {
            child: name(n),
            kind,
            pane: pane(p),
            inbox: inbox(),
            model_label: None,
            model: None,
            directives_hash: None,
        }
    }

    #[test]
    fn fold_tracks_spawns() {
        let recs = vec![
            spawned("a", ChildKind::Worktree, "%1"),
            ChildRecord::Spawned {
                child: name("b"),
                kind: ChildKind::Inline,
                pane: pane("%2"),
                inbox: inbox(),
                model_label: Some("kimi".into()),
                model: Some("kimi-for-coding".into()),
                directives_hash: None,
            },
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids.len(), 2);
        // the cosmetic model tag + the effective model fold through from the Spawned record
        assert_eq!(kids[&name("a")].model_label, None);
        assert_eq!(kids[&name("b")].model_label.as_deref(), Some("kimi"));
        assert_eq!(kids[&name("b")].model.as_deref(), Some("kimi-for-coding"));
        // fresh spawns are Live
        assert_eq!(kids[&name("a")].state, ChildState::Live);
    }

    #[test]
    fn retry_overwrites_with_newest_spawn() {
        let recs = vec![
            spawned("a", ChildKind::Worktree, "%1"),
            spawned("a", ChildKind::Worktree, "%9"),
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids.len(), 1);
        assert_eq!(kids[&name("a")].pane.as_str(), "%9");
    }

    #[test]
    fn respawn_resets_a_tombstoned_name_to_live() {
        let recs = vec![
            spawned("a", ChildKind::Worktree, "%1"),
            ChildRecord::Died {
                child: name("a"),
                pane: pane("%1"),
                at: None,
            },
            spawned("a", ChildKind::Worktree, "%7"),
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids[&name("a")].state, ChildState::Live);
        assert_eq!(kids[&name("a")].pane.as_str(), "%7");
    }

    #[test]
    fn fold_applies_state_records_in_order() {
        let recs = vec![
            spawned("a", ChildKind::Worktree, "%1"),
            ChildRecord::Submitted {
                child: name("a"),
                branch: Branch::new("root.a".into()).unwrap(),
                sha: "deadbeef".into(),
                reviewed: true,
                at: None,
            },
        ];
        let kids = fold_children(&recs);
        assert_eq!(
            kids[&name("a")].state,
            ChildState::Submitted {
                sha: "deadbeef".into(),
                reviewed: true
            }
        );
        assert!(!kids[&name("a")].state.is_terminal());
    }

    #[test]
    fn died_then_reaped_self_heals_to_reaped() {
        // The benign race: the watchdog observed the pane gone a moment before the runtime's own
        // teardown appended its Reaped. Last record wins.
        let recs = vec![
            spawned("a", ChildKind::Worktree, "%1"),
            ChildRecord::Died {
                child: name("a"),
                pane: pane("%1"),
                at: None,
            },
            ChildRecord::Reaped {
                child: name("a"),
                at: None,
            },
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids[&name("a")].state, ChildState::Reaped);
        assert!(kids[&name("a")].state.is_terminal());
    }

    #[test]
    fn tombstoned_children_stay_in_the_fold() {
        let recs = vec![
            spawned("a", ChildKind::Worktree, "%1"),
            ChildRecord::Died {
                child: name("a"),
                pane: pane("%1"),
                at: None,
            },
        ];
        let kids = fold_children(&recs);
        assert_eq!(kids.len(), 1, "a dead child must not vanish from the fold");
        assert_eq!(kids[&name("a")].state, ChildState::Died);
    }

    #[test]
    fn state_record_for_unknown_child_is_skipped() {
        let recs = vec![ChildRecord::Reaped {
            child: name("ghost"),
            at: None,
        }];
        assert!(fold_children(&recs).is_empty());
    }

    #[test]
    fn record_serde_is_tagged() {
        let r = spawned("a", ChildKind::Worktree, "%1");
        let json = serde_json::to_string(&r).unwrap();
        assert!(json.contains(r#""record":"spawned""#));
        let back: ChildRecord = serde_json::from_str(&json).unwrap();
        assert_eq!(r, back);
    }

    #[test]
    fn child_accessor_covers_every_variant() {
        let recs = vec![
            spawned("a", ChildKind::Worktree, "%1"),
            ChildRecord::Reaped {
                child: name("a"),
                at: None,
            },
            ChildRecord::Died {
                child: name("a"),
                pane: pane("%1"),
                at: None,
            },
            ChildRecord::Submitted {
                child: name("a"),
                branch: Branch::new("root.a".into()).unwrap(),
                sha: "x".into(),
                reviewed: false,
                at: None,
            },
        ];
        for r in &recs {
            assert_eq!(r.child(), &name("a"));
        }
    }
}
