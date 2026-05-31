//! The `Bus` cap — runtime-agnostic message delivery (append to the target's ingestion
//! inbox). The Teams-vs-tmux last-hop lives in the *recipient's* inbound loop, so policy
//! never names a delivery mechanism. See docs 02/03.

use crate::error::CapResult;
use crate::types::{AgentName, Message};
use async_trait::async_trait;

/// How policy names a delivery target — **tree-edges only** (no sibling / cross-tree:
/// the messaging structure *is* the tree). `InlineChild` and `WorktreeChild` share the
/// delivery path (name → pane → run-id-keyed inbox) but differ in spawn / papers /
/// teardown (their [`ChildKind`](crate::ChildKind)). `Pane` is not policy-facing — it's
/// an internal resolution target.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Addressee {
    /// Up: my parent's inbox (a path held in my papers).
    Parent,
    /// Down: a worker spawned in MY worktree (ephemeral pane, no PR).
    InlineChild(AgentName),
    /// Down: a child spawned in its OWN worktree (branch + PR).
    WorktreeChild(AgentName),
}

#[async_trait]
pub trait Bus {
    /// Append `msg` to the target's ingestion inbox. Resolution (`Addressee` →
    /// `InboxPath`) is internal to the runtime impl, never exposed to policy.
    async fn deliver(&self, to: Addressee, msg: Message) -> CapResult<()>;
}
