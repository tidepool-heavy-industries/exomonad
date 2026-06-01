//! Type-1 papers (`node.json`) — a node's immutable birth identity, written by the parent
//! at spawn and read by the child's sidecar at boot. The one contract that crosses the
//! birth → self-ID seam (Spawner writes it; Wave-2 node bootstrap reads it). See doc 01.
//!
//! Assigned-at-birth, never derived: `role`/`parent`/tree-position exist in no runtime's
//! live state, so they are *recorded* once. `agent_type` is **not** stored — it derives
//! from `role` ([`NodeKind::agent_type`]). The `pane` is the universal key; `parent_inbox`
//! is the direct up-edge for `Bus::deliver(Parent, …)` (`None` only for the root).

use crate::types::{Branch, NodeKind, NodePath, PaneId};
use crate::InboxPath;
use serde::{Deserialize, Serialize};

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
    /// The `NodeKind` (`root`/`tl`/`dev`/`worker`). `agent_type` derives from this.
    pub role: NodeKind,
    /// tmux pane — delivery target + inbox-key derivation.
    pub pane: PaneId,
    /// Path to the parent's ingestion inbox (the up-edge). `None` for the root.
    pub parent_inbox: Option<InboxPath>,
}

fn default_papers_version() -> u32 {
    1
}

impl NodePapers {
    pub const VERSION: u32 = 1;

    /// Construct papers for a node being born (`v` set to the current [`VERSION`]).
    pub fn new(
        path: NodePath,
        branch: Branch,
        role: NodeKind,
        pane: PaneId,
        parent_inbox: Option<InboxPath>,
    ) -> Self {
        NodePapers {
            v: Self::VERSION,
            path,
            branch,
            role,
            pane,
            parent_inbox,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::AgentName;

    fn an(s: &str) -> AgentName {
        AgentName::new(s.into()).unwrap()
    }

    #[test]
    fn papers_round_trip_through_json() {
        let papers = NodePapers::new(
            NodePath::new(vec![an("dev"), an("oauth-gemini")]).unwrap(),
            Branch::new("dev.oauth-gemini".into()).unwrap(),
            NodeKind::Dev,
            PaneId::new("%317".into()).unwrap(),
            Some(InboxPath::new(
                "/home/u/.claude/exo/inboxes/run-1/pane-311.jsonl".into(),
            )),
        );
        let json = serde_json::to_string(&papers).unwrap();
        // role serializes as the lowercase NodeKind variant
        assert!(json.contains(r#""role":"dev""#));
        let back: NodePapers = serde_json::from_str(&json).unwrap();
        assert_eq!(papers, back);
    }

    #[test]
    fn root_papers_have_no_parent_inbox() {
        let papers = NodePapers::new(
            NodePath::new(vec![an("root")]).unwrap(),
            Branch::new("main".into()).unwrap(),
            NodeKind::Root,
            PaneId::new("%1".into()).unwrap(),
            None,
        );
        let json = serde_json::to_string(&papers).unwrap();
        assert!(json.contains(r#""parent_inbox":null"#));
    }

    #[test]
    fn version_defaults_when_absent() {
        // papers written by an older binary without `v` still parse
        let json = r#"{"path":["root"],"branch":"main","role":"root","pane":"%1","parent_inbox":null}"#;
        let papers: NodePapers = serde_json::from_str(json).unwrap();
        assert_eq!(papers.v, 1);
    }
}
