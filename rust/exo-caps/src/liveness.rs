//! `ChildLiveness` capability — does this node have any still-*working* child?
//!
//! This used to be a genuinely distinct question from [`Topology`](crate::Topology)'s pane
//! **existence** (a child agent keeps its pane alive while idling, so aliveness alone can't prove
//! "busy") — a separate busy-bit was tracked from Claude Code's `Stop` hook (busy from birth/poke,
//! idle on a `ChildIdle` report). That bit was removed: `Stop` fires on every turn-end, including a
//! child legitimately yielding to wait on a backgrounded async task, so the bit was routinely wrong
//! (see `rust/exo/CLAUDE.md`). `any_child_busy` now means exactly what `Topology` already tracked:
//! **any direct child's pane currently exists.** Coarser than the old claim (can't distinguish
//! "actively working" from "idle but its pane is still open"), but honest — the old claim wasn't.

use crate::fs::Fs;
use crate::tmux::Tmux;
use async_trait::async_trait;

/// **Composite cap** — reads the child ledger (`Fs`) and probes pane liveness
/// ([`Tmux::list_panes`]); the supertraits name those powers.
#[async_trait]
pub trait ChildLiveness: Tmux + Fs {
    /// True if any **direct** child's pane currently exists. Best-effort and infallible — a
    /// liveness-probe failure is treated as "busy" (never manufacture a false idle from a transient
    /// probe hiccup).
    ///
    /// Direct children only — a shallow check. Its one caller (the cooperative-shutdown `Defer`
    /// response) uses it as cosmetic "some actively working" wording, not as the actual
    /// clear-to-reap gate; that gate is [`Topology`]'s recursive pane walk, which does account for
    /// the whole subtree.
    async fn any_child_busy(&self) -> bool;
}
