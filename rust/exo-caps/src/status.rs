use crate::{NodeKind, NodePath};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Small periodic status snapshot for swarm visibility.
/// Written to `pane-N.status.json` periodically by the node's sidecar.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeStatus {
    pub node: NodePath,
    pub kind: NodeKind,
    pub branch: String,
    pub shutdown_pending: bool,
    /// Direct children and their busy state.
    pub children: Vec<ChildStatus>,
    pub ts: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChildStatus {
    pub name: String,
    pub busy: bool,
}
