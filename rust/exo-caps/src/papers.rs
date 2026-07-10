//! Type-1 papers (`node.json`) — a node's immutable birth identity, written by the parent
//! at spawn and read by the child's sidecar at boot. The one contract that crosses the
//! birth → self-ID seam (Spawner writes it; node bootstrap reads it).
//!
//! Assigned-at-birth, never derived: `role`/`parent`/tree-position exist in no runtime's
//! live state, so they are *recorded* once. `agent_type` is **not** stored — a domain derives
//! it from `role` ([`RoleKind::agent_type`](crate::RoleKind)). The `pane` is the universal key;
//! `parent_inbox` is the direct up-edge for `Bus::deliver(Parent, …)` (`None` only for the root).
//!
//! The role is stored **erased** as a [`RoleRecord`] (raw JSON) so `NodePapers` stays
//! domain-agnostic (non-generic): the parent writes its domain's `D::Role`, and the child's
//! bootstrap — the only typed reader of the role — deserializes it back to `D::Role` (validating
//! through serde). Same validate-on-read guarantee as a fully-typed papers struct, without
//! genericizing `NodePapers<R>` across every reader (`own_launch_policy`, the hook-socket
//! resolver, …) that only needs the non-role fields.

use crate::error::{CapError, CapResult};
use crate::types::{AgentName, Branch, ChildKind, NodePath, PaneId};
use crate::{InboxPath, RoleKind};
use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;

/// A node's role, recorded **erased** (raw JSON of the domain's `D::Role`). Typed back via
/// [`RoleRecord::typed`] by the one reader that knows the domain (bootstrap). Equality compares the
/// canonical raw string (`RawValue` has no `PartialEq`).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RoleRecord(pub Box<RawValue>);

impl RoleRecord {
    /// Record a typed domain role.
    pub fn new<R: RoleKind>(role: &R) -> CapResult<Self> {
        Ok(RoleRecord(serde_json::value::to_raw_value(role).map_err(
            |e| CapError::Json {
                context: "RoleRecord::new: encode role".into(),
                source: e,
            },
        )?))
    }
    /// Read the role back as the domain's role type (validates through serde).
    pub fn typed<R: RoleKind>(&self) -> CapResult<R> {
        serde_json::from_str(self.0.get()).map_err(|e| CapError::Json {
            context: "RoleRecord::typed: decode role".into(),
            source: e,
        })
    }
}

impl PartialEq for RoleRecord {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}
impl Eq for RoleRecord {}

/// A node's birth papers, persisted as `{cwd}/.exo/node.json` (worktree child) or the
/// pane-keyed run dir (inline worker). Schema-versioned (`v`, parsed tolerantly) so a
/// mixed-version swarm survives a rolling `cargo install`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NodePapers {
    /// Schema version. Defaulted on read so older papers don't fail to parse.
    #[serde(default = "default_papers_version")]
    pub v: u32,
    /// Tree address (list of segments — NOT a dot-string, since branch segments may
    /// contain `.`). `name = path.last()`, `parent.path = path[..len-1]`.
    pub path: NodePath,
    /// Git branch — decoupled from `path`, generated safely (a `.` in a segment can't
    /// corrupt it).
    pub branch: Branch,
    /// The node's role, erased ([`RoleRecord`]); bootstrap types it back to `D::Role`.
    /// `agent_type` derives from this (domain-side).
    pub role: RoleRecord,
    /// tmux pane — delivery target + inbox-key derivation.
    pub pane: PaneId,
    /// Path to the parent's ingestion inbox (the up-edge). `None` for the root.
    pub parent_inbox: Option<InboxPath>,
    /// Launch policy for this node's CHILDREN, inherited down the tree: a spawning node
    /// stamps each child's papers with its own policy and reads these back to decide how to
    /// launch the next generation. Defaulted on read so papers written by an older binary
    /// preserve today's behavior. `yolo` is a launch-policy knob inherited down the tree
    /// (retained for the Shoal/companion launch path and config round-tripping).
    #[serde(default = "default_yolo")]
    pub yolo: bool,
    /// Wrap a child's launch command in `nix develop` when its cwd has a `flake.nix`.
    /// Defaulted `false` — node children launch plain, matching the root.
    #[serde(default = "default_wrap_nix")]
    pub wrap_nix: bool,
    /// Whether this node shares its parent's worktree (Inline) or lives in its own (Worktree).
    /// Defaulted `Worktree` so papers written by older binaries (which had no `kind` field)
    /// still parse correctly — they are always worktree nodes.
    #[serde(default = "default_kind")]
    pub kind: ChildKind,
    /// Whether `submit_branch` should spawn a reviewer at all. Inherited down the tree exactly
    /// like `yolo`/`wrap_nix` — a spawning node stamps its own value onto each child's papers.
    /// Defaults to `false`: reviewers are not yet a fully-cooked feature (see the abandonment-
    /// timeout / nested-teardown history in `rust/exo/CLAUDE.md`), so a project opts in via
    /// `.exo/config.toml`'s `review_enabled = true` rather than getting them by default.
    #[serde(default = "default_review_enabled")]
    pub review_enabled: bool,
    /// The parent's real git branch, stamped at birth by the spawner from ITS OWN current
    /// branch (the parent is, by definition, on the branch its children fork from). `None` for
    /// the root (no parent). Backs `submit_branch`'s `needs_rebase` gate: the branch name
    /// embedded in `path`/`branch` is a tree-address coordinate, NOT necessarily a live git ref
    /// (a direct child of root has a derived parent coordinate of `root`, which is the root's
    /// exo IDENTITY, not the branch the human's root session is actually on) — this field
    /// carries the real ref instead. Defaulted on read (`#[serde(default)]`) so papers written
    /// by an older binary, which had no such field, still parse — they read back as `None`,
    /// which fails the rebase gate open exactly as it did before this field existed.
    #[serde(default)]
    pub parent_branch: Option<Branch>,
}

fn default_papers_version() -> u32 {
    1
}

fn default_yolo() -> bool {
    NodePapers::DEFAULT_YOLO
}

fn default_wrap_nix() -> bool {
    NodePapers::DEFAULT_WRAP_NIX
}

fn default_kind() -> ChildKind {
    ChildKind::Worktree
}

fn default_review_enabled() -> bool {
    NodePapers::DEFAULT_REVIEW_ENABLED
}

impl NodePapers {
    pub const VERSION: u32 = 1;
    /// Behavior-preserving launch defaults: node children launch yolo + non-nix-wrapped,
    /// matching the root. Single source of truth for both papers defaulting and the
    /// spawner's fallback when a node has no readable papers.
    pub const DEFAULT_YOLO: bool = true;
    pub const DEFAULT_WRAP_NIX: bool = false;
    /// Reviewers are opt-in, not a fully-cooked default — see the field doc on `review_enabled`.
    pub const DEFAULT_REVIEW_ENABLED: bool = false;

    /// Construct papers for a node being born (`v` set to the current [`VERSION`]). The role is
    /// recorded erased; `yolo` / `wrap_nix` / `review_enabled` are the launch policy stamped onto
    /// the child. `parent_branch` is the parent's own real git branch (`None` for the root).
    #[allow(clippy::too_many_arguments)]
    pub fn new<R: RoleKind>(
        path: NodePath,
        branch: Branch,
        role: R,
        pane: PaneId,
        parent_inbox: Option<InboxPath>,
        yolo: bool,
        wrap_nix: bool,
        review_enabled: bool,
        parent_branch: Option<Branch>,
    ) -> CapResult<Self> {
        Ok(NodePapers {
            v: Self::VERSION,
            path,
            branch,
            role: RoleRecord::new(&role)?,
            pane,
            parent_inbox,
            yolo,
            wrap_nix,
            kind: ChildKind::Worktree,
            review_enabled,
            parent_branch,
        })
    }

    /// Construct papers for the root node (no parent). The domain supplies its root role.
    pub fn root<R: RoleKind>(pane: PaneId, role: R) -> CapResult<Self> {
        Self::new(
            NodePath::new(vec![
                AgentName::new("root".into()).expect("valid agent name")
            ])
            .expect("valid node path"),
            Branch::new("root".into()).expect("valid branch name"),
            role,
            pane,
            None,
            Self::DEFAULT_YOLO,
            Self::DEFAULT_WRAP_NIX,
            Self::DEFAULT_REVIEW_ENABLED,
            None,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::AgentType;

    fn an(s: &str) -> AgentName {
        AgentName::new(s.into()).unwrap()
    }

    /// A stand-in domain role for the papers round-trip tests (exo-caps owns no concrete role).
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
    #[serde(rename_all = "lowercase")]
    enum TestRole {
        Root,
        Dev,
    }
    impl RoleKind for TestRole {
        fn all() -> &'static [Self] {
            &[TestRole::Root, TestRole::Dev]
        }
        fn agent_type(&self) -> AgentType {
            match self {
                TestRole::Root | TestRole::Dev => AgentType::Claude,
            }
        }
        fn role_str(&self) -> &'static str {
            match self {
                TestRole::Root => "root",
                TestRole::Dev => "dev",
            }
        }
    }

    #[test]
    fn papers_round_trip_through_json() {
        let papers = NodePapers::new(
            NodePath::new(vec![an("dev"), an("oauth-dev")]).unwrap(),
            Branch::new("dev.oauth-dev".into()).unwrap(),
            TestRole::Dev,
            PaneId::new("%317".into()).unwrap(),
            Some(InboxPath::new(
                "/home/u/.claude/exo/inboxes/run-1/pane-311.jsonl".into(),
            )),
            NodePapers::DEFAULT_YOLO,
            NodePapers::DEFAULT_WRAP_NIX,
            NodePapers::DEFAULT_REVIEW_ENABLED,
            Some(Branch::new("dev".into()).unwrap()),
        )
        .unwrap();
        let json = serde_json::to_string(&papers).unwrap();
        // role serializes erased as the raw role JSON (here the lowercase TestRole variant)
        assert!(json.contains(r#""role":"dev""#));
        let back: NodePapers = serde_json::from_str(&json).unwrap();
        assert_eq!(papers, back);
        // and the role types back to the domain enum
        assert_eq!(back.role.typed::<TestRole>().unwrap(), TestRole::Dev);
        assert_eq!(back.parent_branch.unwrap().as_str(), "dev");
    }

    #[test]
    fn version_defaults_when_absent() {
        // papers written by an older binary without `v` still parse
        let json =
            r#"{"path":["root"],"branch":"main","role":"root","pane":"%1","parent_inbox":null}"#;
        let papers: NodePapers = serde_json::from_str(json).unwrap();
        assert_eq!(papers.v, 1);
        // The launch-policy fields, absent from older papers, default to today's behavior.
        assert_eq!(papers.yolo, NodePapers::DEFAULT_YOLO);
        assert_eq!(papers.wrap_nix, NodePapers::DEFAULT_WRAP_NIX);
        assert_eq!(papers.role.typed::<TestRole>().unwrap(), TestRole::Root);
        // `kind` is absent from older papers — defaults to Worktree (back-compat).
        assert_eq!(papers.kind, ChildKind::Worktree);
        // `review_enabled` is absent from older papers — defaults to off.
        assert_eq!(papers.review_enabled, NodePapers::DEFAULT_REVIEW_ENABLED);
        // `parent_branch` is absent from older papers — defaults to `None`, which fails the
        // rebase gate open exactly as the old string-derivation did for an unresolvable ref.
        assert_eq!(papers.parent_branch, None);
    }

    #[test]
    fn root_constructor() {
        let pane = PaneId::new("%1".into()).unwrap();
        let papers = NodePapers::root(pane.clone(), TestRole::Root).unwrap();
        assert_eq!(papers.v, NodePapers::VERSION);
        assert_eq!(papers.role.typed::<TestRole>().unwrap(), TestRole::Root);
        assert!(papers.parent_inbox.is_none());
        assert_eq!(papers.path.name().as_str(), "root");
        assert_eq!(papers.branch.as_str(), "root");
        assert_eq!(papers.pane, pane);
    }
}
