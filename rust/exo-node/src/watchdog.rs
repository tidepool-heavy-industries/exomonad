//! **Watchdog loop.** Replaces Claude Code's `Stop` hook as a decision trigger. `Stop` fires on
//! every turn-end — including a completely normal async-wait yield (e.g. a reviewer backgrounding a
//! slow build and waiting on its notification) — so it cannot distinguish "genuinely done" from
//! "paused." A wall-clock interval can. This loop ticks on a fixed interval and gives the domain
//! (via [`Exomonad::handle_tick`]) and the engine's own cooperative-shutdown reap a real, honest
//! signal instead.

use std::sync::Arc;
use std::time::{Duration, Instant};

use exo_caps::Child;
use exo_framework::Exomonad;
use exo_runtime::Runtime;
use tracing::warn;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// How often the watchdog re-checks. Cheap per tick (a domain no-ops unless its own threshold has
/// elapsed; `try_reap` no-ops unless a shutdown is pending) — short enough that a pending
/// cooperative shutdown reaps promptly once the subtree clears.
const TICK_INTERVAL: Duration = Duration::from_secs(60);

/// A single tick finding more than this many simultaneous deaths gets an extra note that this is
/// the shape of a first scan over a ledger that predates the watchdog/`Died` feature, not a fresh
/// wave of failures — the incident this batching exists to fix was exactly that: 98 individually
/// pasted `[CHILD DIED]` notes from one historical ledger scan.
const DEATH_FLOOD_THRESHOLD: usize = 10;

/// Run the watchdog until aborted (by `run_node` when the outbound anchor closes). `elapsed` is
/// measured from this loop's own start, which is ≈ this node's boot time (the sidecar starts all its
/// background loops together at boot) — near enough for a multi-minute abandonment timeout.
pub async fn watch<D: Exomonad<Caps = Runtime>>(ctx: Arc<NodeContext<D>>) -> NodeResult<()> {
    let started = Instant::now();
    let mut interval = tokio::time::interval(TICK_INTERVAL);
    // The first tick fires immediately; skip it so we don't run a same-instant tick at boot.
    interval.tick().await;

    loop {
        interval.tick().await;
        let elapsed = started.elapsed();

        if let Err(e) = D::handle_tick(&*ctx.runtime, ctx.kind, elapsed).await {
            warn!(node = %ctx.runtime.name().as_str(), "watchdog: handle_tick failed: {e}");
        }

        // Announce every child the runtime just detected as dead THIS tick as ONE message — not
        // one paste per corpse (a first scan over a pre-lifecycle-watchdog ledger can find dozens
        // dead at once). The `Died` records are already durable by the time they're returned here
        // (record-then-announce), so a delivery failure is a warn — never abort the tick.
        let dead = ctx.runtime.detect_child_deaths().await;
        if let Some((summary, text)) = render_death_announcement(&dead) {
            if let Err(e) =
                crate::dispatch::deliver_synthetic(&ctx, "watchdog", &summary, &text).await
            {
                warn!(
                    node = %ctx.runtime.name().as_str(),
                    count = dead.len(),
                    "watchdog: failed to announce child death(s) (records are durable; not retried): {e}"
                );
            }
        }

        // Idempotent and independently gated (shutdown_pending + pane-alive subtree-clear via
        // Topology) — safe to re-check on every tick regardless of whether anything changed.
        crate::inbound::try_reap(&ctx).await;
    }
}

/// Render the announcement for the children `detect_child_deaths` found dead THIS tick. `None`
/// for an empty scan — nothing to paste. A single death keeps the original one-child message
/// shape verbatim; more than one batches into ONE `[CHILDREN DIED: N]` message listing each
/// `name (pane)`, one per line, so a tick with N deaths pastes exactly once, not N times.
fn render_death_announcement(dead: &[Child]) -> Option<(String, String)> {
    match dead.len() {
        0 => None,
        1 => {
            let d = &dead[0];
            let summary = format!("[CHILD DIED: {}]", d.name.as_str());
            let text = format!(
                "pane {} no longer exists and the child was never reaped. Its branch and \
                 worktree may still hold unmerged or uncommitted work. Run the `tree` tool to \
                 see where it sat, then either merge what its branch holds or respawn the work.",
                d.pane.as_str()
            );
            Some((summary, text))
        }
        n => {
            let listing = dead
                .iter()
                .map(|d| format!("{} ({})", d.name.as_str(), d.pane.as_str()))
                .collect::<Vec<_>>()
                .join("\n");
            let mut text = format!(
                "{listing}\n\nEach pane no longer exists and the child was never reaped. Their \
                 branches and worktrees may still hold unmerged or uncommitted work. Run the \
                 `tree` tool to see where each sat, then either merge what its branch holds or \
                 respawn the work."
            );
            if n > DEATH_FLOOD_THRESHOLD {
                text.push_str(
                    "\n\nA first scan over a pre-lifecycle ledger announces all historical \
                     corpses at once — this is expected once, not a new wave of failures.",
                );
            }
            Some((format!("[CHILDREN DIED: {n}]"), text))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, ChildKind, ChildState, InboxPath, PaneId};

    fn dead_child(name: &str, pane: &str) -> Child {
        Child {
            name: AgentName::new(name.to_string()).unwrap(),
            kind: ChildKind::Worktree,
            pane: PaneId::new(pane.to_string()).unwrap(),
            inbox: InboxPath::new("/tmp/x.jsonl".into()),
            model_label: None,
            model: None,
            directives_hash: None,
            state: ChildState::Died,
        }
    }

    #[test]
    fn render_death_announcement_empty_is_none() {
        assert_eq!(render_death_announcement(&[]), None);
    }

    #[test]
    fn render_death_announcement_single_keeps_original_shape() {
        let dead = vec![dead_child("leaf-1", "%12")];
        let (summary, text) = render_death_announcement(&dead).unwrap();
        assert_eq!(summary, "[CHILD DIED: leaf-1]");
        assert!(text.contains("pane %12 no longer exists"));
        assert!(text.contains("`tree` tool"));
    }

    #[test]
    fn render_death_announcement_batches_a_few() {
        let dead = vec![dead_child("a", "%1"), dead_child("b", "%2")];
        let (summary, text) = render_death_announcement(&dead).unwrap();
        assert_eq!(summary, "[CHILDREN DIED: 2]");
        assert!(text.contains("a (%1)"));
        assert!(text.contains("b (%2)"));
        assert!(
            !text.contains("pre-lifecycle ledger"),
            "a small batch is not a flood"
        );
    }

    #[test]
    fn render_death_announcement_flood_gets_extra_note() {
        let dead: Vec<Child> = (0..98)
            .map(|i| dead_child(&format!("leaf-{i}"), &format!("%{i}")))
            .collect();
        let (summary, text) = render_death_announcement(&dead).unwrap();
        assert_eq!(summary, "[CHILDREN DIED: 98]");
        assert!(text.contains("leaf-0 (%0)"));
        assert!(text.contains("leaf-97 (%97)"));
        assert!(text.contains("pre-lifecycle ledger"));
    }
}
