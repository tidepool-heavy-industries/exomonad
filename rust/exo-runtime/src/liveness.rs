//! `impl ChildLiveness for Runtime` — the idle gate's view of the direct children.
//!
//! There used to be a busy-bit map here too (mutated at birth / on poke / on a `ChildIdle` report
//! from Claude Code's `Stop` hook), combined with a pane probe. It was removed — `Stop` fires on
//! every turn-end, including a legitimate async-wait yield, so the bit was routinely wrong (see
//! `rust/exo/CLAUDE.md`). `any_child_busy` is now a direct pane-existence probe: the ledger
//! (`children.jsonl`) is the source of truth for *which* children exist; a one-shot tmux probe
//! decides whether each still has a live pane.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{AgentName, ChildLiveness};
use std::collections::HashSet;

/// The pure decision: does any child have a live pane? `alive` is the probe result — `None` means
/// the probe failed (liveness unknown ⇒ assume busy, never force a false idle from a transient
/// hiccup). Split out so the truth table is testable without tmux or the filesystem.
fn any_busy<'a>(
    children: impl Iterator<Item = (&'a AgentName, &'a str)>,
    alive: Option<&HashSet<String>>,
) -> bool {
    children.into_iter().any(|(_name, pane)| match alive {
        Some(set) => set.contains(pane),
        None => true, // probe failed ⇒ unknown ⇒ assume busy
    })
}

#[async_trait]
impl ChildLiveness for Runtime {
    async fn any_child_busy(&self) -> bool {
        let records = match self.read_child_records().await {
            Ok(r) => r,
            Err(e) => {
                // Can't read the ledger → can't tell → assume busy (bias against a false idle).
                tracing::warn!("any_child_busy: could not read child ledger, assuming busy: {e}");
                return true;
            }
        };
        let children = exo_caps::fold_children(&records);
        // Tombstoned children have nothing to probe — tmux recycles pane ids, so their recorded
        // pane may since have been recycled onto a different live agent.
        let live: Vec<_> = children
            .values()
            .filter(|c| !c.state.is_terminal())
            .collect();
        if live.is_empty() {
            return false; // no live children → nothing to wait on
        }

        // Probe pane liveness once ([`exo_caps::Tmux::list_panes`]). A probe failure maps to
        // `None` = liveness unknown ⇒ assume busy — NEVER to an empty set, which would read as
        // "all panes dead" and force a false idle.
        let alive = match exo_caps::Tmux::list_panes(self).await {
            Ok(set) => Some(set),
            Err(e) => {
                tracing::warn!(error = %e, "any_child_busy: pane probe failed; assuming busy");
                None
            }
        };

        any_busy(
            live.iter().map(|c| (&c.name, c.pane.as_str())),
            alive.as_ref(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn an(s: &str) -> AgentName {
        AgentName::new(s.into()).unwrap()
    }

    #[test]
    fn live_pane_is_busy() {
        let a = an("a");
        let alive = HashSet::from(["%1".to_string()]);
        assert!(any_busy([(&a, "%1")].into_iter(), Some(&alive)));
    }

    #[test]
    fn dead_pane_is_idle() {
        let a = an("a");
        let alive = HashSet::new(); // %1 not present → dead
        assert!(!any_busy([(&a, "%1")].into_iter(), Some(&alive)));
    }

    #[test]
    fn probe_failure_assumes_busy() {
        // `None` (probe failed) must never manufacture a false idle.
        let a = an("a");
        assert!(any_busy([(&a, "%1")].into_iter(), None));
    }

    #[test]
    fn any_means_any() {
        let a = an("a");
        let b = an("b");
        let alive = HashSet::from(["%2".to_string()]); // only b's pane is alive
        assert!(any_busy([(&a, "%1"), (&b, "%2")].into_iter(), Some(&alive)));
    }

    #[test]
    fn no_children_alive_is_not_busy() {
        let a = an("a");
        let alive = HashSet::new();
        assert!(!any_busy([(&a, "%1")].into_iter(), Some(&alive)));
    }
}
