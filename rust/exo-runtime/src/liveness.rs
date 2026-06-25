//! `impl ChildLiveness for Runtime` — the idle gate's view of the direct children.
//!
//! Combines two sources: the in-memory busy-bit map (`children_busy`, mutated at birth / on poke /
//! on `ChildIdle`) and a one-shot tmux pane probe. A child counts as busy iff its bit says busy
//! AND its pane is not known-dead. The ledger (`children.jsonl`) is the source of truth for *which*
//! children exist; the bit and the probe decide whether each is working.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{AgentName, ChildLiveness};
use std::collections::{HashMap, HashSet};

/// The pure decision: is any child busy? A child is busy iff its busy-bit is set AND its pane is
/// not known-dead. `alive` is the probe result — `None` means the probe failed (liveness unknown
/// ⇒ trust the bit, never force-idle). A child absent from `bits` defaults to busy (unknown ⇒
/// assume working, the bias against a false idle). Split out so the truth table is testable
/// without tmux or the filesystem.
fn any_busy<'a>(
    children: impl Iterator<Item = (&'a AgentName, &'a str)>,
    bits: &HashMap<AgentName, bool>,
    alive: Option<&HashSet<String>>,
) -> bool {
    children.into_iter().any(|(name, pane)| {
        let pane_alive = match alive {
            Some(set) => set.contains(pane),
            None => true, // probe failed ⇒ unknown ⇒ trust the bit
        };
        let busy_bit = bits.get(name).copied().unwrap_or(true); // unknown ⇒ assume busy
        pane_alive && busy_bit
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
        if children.is_empty() {
            return false; // no children → nothing to wait on
        }

        // Probe pane liveness once ([`exo_caps::Tmux::list_panes`]). A probe failure maps to
        // `None` = liveness unknown ⇒ trust the busy-bit — NEVER to an empty set, which would
        // read as "all panes dead" and force a false idle.
        let alive = match exo_caps::Tmux::list_panes(self).await {
            Ok(set) => Some(set),
            Err(e) => {
                tracing::warn!(error = %e, "any_child_busy: pane probe failed; liveness unknown — trusting busy-bits");
                None
            }
        };
        let bits = self.children_busy.lock().unwrap().clone();

        any_busy(
            children.values().map(|c| (&c.name, c.pane.as_str())),
            &bits,
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
    fn busy_bit_and_live_pane_is_busy() {
        let a = an("a");
        let bits = HashMap::from([(a.clone(), true)]);
        let alive = HashSet::from(["%1".to_string()]);
        assert!(any_busy([(&a, "%1")].into_iter(), &bits, Some(&alive)));
    }

    #[test]
    fn idle_bit_is_idle_even_with_live_pane() {
        // The whole point: a child sits idle with a live pane. Bit wins.
        let a = an("a");
        let bits = HashMap::from([(a.clone(), false)]);
        let alive = HashSet::from(["%1".to_string()]);
        assert!(!any_busy([(&a, "%1")].into_iter(), &bits, Some(&alive)));
    }

    #[test]
    fn dead_pane_forces_idle_despite_busy_bit() {
        // pane-death is the one-way override: a busy bit on a vanished pane reads idle.
        let a = an("a");
        let bits = HashMap::from([(a.clone(), true)]);
        let alive = HashSet::new(); // %1 not present → dead
        assert!(!any_busy([(&a, "%1")].into_iter(), &bits, Some(&alive)));
    }

    #[test]
    fn probe_failure_trusts_the_bit() {
        // `None` (probe failed) must NOT read as all-dead — that would manufacture a false idle.
        let a = an("a");
        let busy = HashMap::from([(a.clone(), true)]);
        assert!(any_busy([(&a, "%1")].into_iter(), &busy, None));
        let idle = HashMap::from([(a.clone(), false)]);
        assert!(!any_busy([(&a, "%1")].into_iter(), &idle, None));
    }

    #[test]
    fn unknown_bit_defaults_to_busy_when_alive() {
        // A child in the ledger but missing from the bit map (e.g. after a sidecar restart) is
        // assumed busy if its pane is alive — bias against a false idle.
        let a = an("a");
        let bits = HashMap::new();
        let alive = HashSet::from(["%1".to_string()]);
        assert!(any_busy([(&a, "%1")].into_iter(), &bits, Some(&alive)));
        // ...but a dead pane still forces idle.
        assert!(!any_busy(
            [(&a, "%1")].into_iter(),
            &bits,
            Some(&HashSet::new())
        ));
    }

    #[test]
    fn any_means_any() {
        let a = an("a");
        let b = an("b");
        let bits = HashMap::from([(a.clone(), false), (b.clone(), true)]);
        let alive = HashSet::from(["%1".to_string(), "%2".to_string()]);
        // a idle, b busy → subtree busy.
        assert!(any_busy(
            [(&a, "%1"), (&b, "%2")].into_iter(),
            &bits,
            Some(&alive)
        ));
    }
}
