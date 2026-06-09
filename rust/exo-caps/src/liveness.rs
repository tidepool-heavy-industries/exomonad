//! `ChildLiveness` capability — does this node have any still-*working* child?
//!
//! Distinct from [`Topology`](crate::Topology), which reports pane **existence**. A pane existing
//! does NOT mean its agent is working: a Gemini child launched `--prompt-interactive` keeps its
//! pane (and process) alive while it sits idle waiting for input. So aliveness can never prove
//! "busy".
//!
//! Idleness is instead tracked from the messages the sidecar already sees — a child is busy from
//! birth and from every poke (a message delivered down to it), and idle once it reports
//! `ChildIdle`. Pane-death is a one-way override on top of that bit: a dead pane is, definitionally,
//! idle (it's gone — it isn't working), so it forces idle regardless of a stale busy bit. A live
//! pane decides nothing on its own.

use crate::fs::Fs;
use crate::tmux::Tmux;
use async_trait::async_trait;

/// **Composite cap** — the gate reads the child ledger (`Fs`) and probes pane liveness
/// ([`Tmux::list_panes`]); the supertraits name those powers. The busy-bit map itself is
/// impl-internal in-memory state, not a cap.
#[async_trait]
pub trait ChildLiveness: Tmux + Fs {
    /// True if any **direct** child is still working: its busy-bit is set AND its pane is not
    /// known-dead. Best-effort and infallible — a liveness-probe failure is treated as "alive"
    /// (trust the busy-bit), so a transient probe hiccup never manufactures a false idle.
    ///
    /// Only direct children are consulted: the recursion folds through the message tree, because a
    /// child reports `ChildIdle` upward only once its *own* subtree is quiescent. So one bit per
    /// direct child already accounts for everything beneath it.
    async fn any_child_busy(&self) -> bool;
}
