//! **Watchdog loop.** Replaces Claude Code's `Stop` hook as a decision trigger. `Stop` fires on
//! every turn-end — including a completely normal async-wait yield (e.g. a reviewer backgrounding a
//! slow build and waiting on its notification) — so it cannot distinguish "genuinely done" from
//! "paused." A wall-clock interval can. This loop ticks on a fixed interval and gives the domain
//! (via [`Exomonad::handle_tick`]) and the engine's own cooperative-shutdown reap a real, honest
//! signal instead.

use std::sync::Arc;
use std::time::{Duration, Instant};

use exo_framework::Exomonad;
use exo_runtime::Runtime;
use tracing::warn;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// How often the watchdog re-checks. Cheap per tick (a domain no-ops unless its own threshold has
/// elapsed; `try_reap` no-ops unless a shutdown is pending) — short enough that a pending
/// cooperative shutdown reaps promptly once the subtree clears.
const TICK_INTERVAL: Duration = Duration::from_secs(60);

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

        // Announce every child the runtime just detected as dead. The `Died` record is already
        // durable by the time it's returned here (record-then-announce), so a delivery failure is
        // a warn — never abort the tick or skip the remaining dead children.
        for dead in ctx.runtime.detect_child_deaths().await {
            let summary = format!("[CHILD DIED: {}]", dead.name.as_str());
            let text = format!(
                "pane {} no longer exists and the child was never reaped. Its branch and \
                 worktree may still hold unmerged or uncommitted work. Run the `tree` tool to \
                 see where it sat, then either merge what its branch holds or respawn the work.",
                dead.pane.as_str()
            );
            if let Err(e) =
                crate::dispatch::deliver_synthetic(&ctx, "watchdog", &summary, &text).await
            {
                warn!(
                    node = %ctx.runtime.name().as_str(),
                    child = dead.name.as_str(),
                    "watchdog: failed to announce child death (record is durable; not retried): {e}"
                );
            }
        }

        // Idempotent and independently gated (shutdown_pending + pane-alive subtree-clear via
        // Topology) — safe to re-check on every tick regardless of whether anything changed.
        crate::inbound::try_reap(&ctx).await;
    }
}
