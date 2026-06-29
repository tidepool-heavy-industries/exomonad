//! The `Bus` cap — runtime-agnostic message delivery (append to the target's ingestion
//! inbox). The Teams-vs-tmux last-hop lives in the *recipient's* inbound loop, so policy
//! never names a delivery mechanism.

use crate::fs::Fs;
use crate::types::{AgentName, Message};
use async_trait::async_trait;
use thiserror::Error;

/// How policy names a delivery target — **tree-edges only** (no sibling / cross-tree:
/// the messaging structure *is* the tree). `Pane` is not policy-facing — it's an internal
/// resolution target.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Addressee {
    /// Up: my parent's inbox (a path held in my papers).
    Parent,
    /// Down: any direct child (inline worker or worktree child — delivery is identical).
    Child(AgentName),
}

/// Log-friendly rendering: `parent` or the bare child name — not the `Child(AgentName(..))`
/// Debug wrapping. Use `%addr` at log sites; `?addr` (Debug) is for diagnostics that need the variant.
impl std::fmt::Display for Addressee {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Addressee::Parent => f.write_str("parent"),
            Addressee::Child(name) => f.write_str(name.as_str()),
        }
    }
}

#[derive(Debug, Error)]
pub enum BusError {
    /// The addressee couldn't be resolved to an inbox (unknown child, no parent).
    #[error("cannot resolve {0:?}")]
    Unresolved(Addressee),
    #[error("bus append failed: {detail}")]
    Append { detail: String },
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

/// **Composite cap** — resolution (`Addressee` → `InboxPath`) reads the child ledger and
/// papers through the `Fs` supertrait. The inbox *append* itself is deliberately NOT an `Fs`
/// op (there is no `Fs::append`): the multi-writer PIPE_BUF discipline lives inside the `Bus`
/// impl, where policy can't reach for a raw append that would weaken it.
#[async_trait]
pub trait Bus: Fs {
    /// Append `msg` to the target's ingestion inbox. The runtime stamps the envelope
    /// (`from`/`id`/`ts`/`v` — see [`IngestionEntry`](crate::IngestionEntry)); policy
    /// supplies only the [`Message`]. Resolution (`Addressee` → `InboxPath`) is internal.
    async fn deliver(&self, to: Addressee, msg: Message) -> Result<(), BusError>;
}
