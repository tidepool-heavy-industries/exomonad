//! **N2b — Inbound loop.** Drive the Bus READ side of this node's own ingestion inbox — the
//! cursor/restart half the `Bus` cap (write side) leaves to the reader. The contract:
//!
//! - **Cursor = byte-offset** in a sibling `pane-N.cursor`. Resume = seek + read forward, O(1).
//! - **Watch via the `notify` crate** (event-driven, never hand-rolled inotify), coalesced through
//!   a [`tokio::sync::Notify`]; on each wake re-read from the cursor. A 15s periodic tick runs the
//!   same re-read as a backstop, so a routing failure (cursor deliberately left unadvanced) is
//!   retried even if no later filesystem write ever wakes the watcher.
//! - **Read only up to the last `\n`** — a torn trailing line is re-read once complete.
//! - **Advance the cursor AFTER a successful last-hop delivery**, written **temp + rename**
//!   (atomic replace — a "small" overwrite is NOT crash-atomic). At-least-once, never dropped/corrupted.
//! - **Missing cursor** (fresh node) → start at current EOF; don't replay history.
//! - Parse each line as [`IngestionEntry`] (tolerant: serde defaults, no `deny_unknown_fields`).
//!
//! Then route each new entry by `kind`:
//! - `Chat` / `Event` → [`crate::dispatch::dispatch`] (N2a last-hop): deliver to the agent's
//!   native interface (Teams inbox or tmux paste), rendered with a `[from: X, kind: Y]` header.
//! - `Control(Shutdown { grace_ms, force })` → the cooperative/forced matrix (see `decide`): a
//!   cooperative request to a node with live children bounces an "are you sure" back to the
//!   requester; a leaf winds down and is reaped on the watchdog's next periodic tick; a forced
//!   request cascades a subtree teardown. The actual self-reap (`try_reap`) only fires when the
//!   subtree is clear (pane-liveness is the sole authority), sending an advisory `Lifecycle::Exiting`
//!   poke up first. A child's `Lifecycle::Exiting` re-triggers its parent's reap check.

use std::fs::File;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::Notify;
use tracing::{error, info, warn};

use chrono::Utc;
use exo_caps::types::ShutdownStatus;
use exo_caps::{
    Addressee, AgentName, Branch, CapResult, ChildLiveness, ChildRecord, ControlKind,
    DomainPayload, IngestionEntry, Lifecycle, Message, MessageBody, MessageKind, Persona, Summary,
    Tmux, Topology, TreeNode,
};
use exo_framework::{Exomonad, SystemCtx, SystemOutcome};

use crate::bootstrap::NodeContext;
use crate::dispatch::{kind_label, persona_label};
use crate::error::NodeResult;

/// Watch the node's own ingestion inbox and route each new entry until shutdown.
#[tracing::instrument(skip(ctx), fields(node = %ctx.runtime.name().as_str()))]
pub async fn watch<D: Exomonad>(ctx: Arc<NodeContext<D>>) -> NodeResult<()> {
    let inbox_path = ctx.own_inbox.as_path().to_path_buf();
    // Append rather than `with_extension` so a multi-dot inbox name can't mis-target the cursor.
    let cursor_path = PathBuf::from(format!("{}.cursor", inbox_path.display()));

    // Initialize cursor
    let mut offset = if cursor_path.exists() {
        match std::fs::read_to_string(&cursor_path) {
            Ok(s) => s.trim().parse::<u64>().unwrap_or_else(|_| {
                warn!("malformed cursor at {:?}, starting at EOF", cursor_path);
                get_eof(&inbox_path)
            }),
            Err(e) => {
                warn!(
                    "failed to read cursor at {:?}: {}, starting at EOF",
                    cursor_path, e
                );
                get_eof(&inbox_path)
            }
        }
    } else {
        let eof = get_eof(&inbox_path);
        // Non-fatal: failing to persist the initial cursor must not stop the node from receiving.
        if let Err(e) = save_cursor(&cursor_path, eof) {
            warn!("failed to persist initial cursor at {:?}: {e}", cursor_path);
        }
        eof
    };

    info!(
        "starting inbound loop for {:?} at offset {}",
        inbox_path, offset
    );

    // Setup notify watcher. `Notify` gives coalescing wakeup semantics (any number of filesystem
    // events between loop iterations collapse into one wake) with no bounded channel to overflow.
    let notify = Arc::new(Notify::new());
    let notify_writer = notify.clone();
    let mut watcher = RecommendedWatcher::new(
        move |res: notify::Result<Event>| {
            if let Ok(event) = res {
                if event.kind.is_modify() || event.kind.is_create() {
                    notify_writer.notify_one();
                }
            }
        },
        Config::default(),
    )
    .map_err(std::io::Error::other)?;

    // Watch the parent directory because watching a file directly can be unreliable
    // with some editors/tools that use rename-over-original.
    if let Some(parent) = inbox_path.parent() {
        watcher
            .watch(parent, RecursiveMode::NonRecursive)
            .map_err(std::io::Error::other)?;
    }

    let handler = RealHandler { ctx: ctx.clone() };
    let node = ctx.runtime.name();

    // Initial pass to catch anything already there. A transient failure (file/cursor IO) must
    // not stop the loop — the next notify wake re-reads from the unchanged offset.
    if let Err(e) = process_inbox(
        node.as_str(),
        &handler,
        &inbox_path,
        &cursor_path,
        &mut offset,
    )
    .await
    {
        warn!(node = %node.as_str(), "inbound initial pass failed (will retry on next event): {e}");
    }

    // A 15s periodic tick retries a delivery failure independent of any future filesystem write —
    // `process_inbox` leaves the cursor unchanged on a routing failure, and without this tick that
    // entry would only ever be retried by a later notify wake that may never come.
    let mut retry_tick = tokio::time::interval(Duration::from_secs(15));
    retry_tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    retry_tick.tick().await; // first tick fires immediately; the initial pass above already ran

    loop {
        tokio::select! {
            _ = notify.notified() => {}
            _ = retry_tick.tick() => {}
        }

        match process_inbox(
            node.as_str(),
            &handler,
            &inbox_path,
            &cursor_path,
            &mut offset,
        )
        .await
        {
            Ok(true) => break, // shutdown received
            Ok(false) => {}
            Err(e) => {
                warn!(node = %node.as_str(), "inbound pass failed (will retry within 15s): {e}")
            }
        }
    }

    Ok(())
}

fn get_eof(path: &Path) -> u64 {
    File::open(path)
        .and_then(|f| f.metadata())
        .map(|m| m.len())
        .unwrap_or(0)
}

fn save_cursor(path: &Path, offset: u64) -> std::io::Result<()> {
    let tmp_path = PathBuf::from(format!("{}.tmp", path.display()));
    {
        let mut f = File::create(&tmp_path)?;
        writeln!(f, "{}", offset)?;
        f.sync_all()?; // Ensure it's on disk before rename
    }
    std::fs::rename(tmp_path, path)
}

#[async_trait]
trait InboundHandler {
    async fn handle(&self, entry: &IngestionEntry) -> NodeResult<Option<bool>>;
}

struct RealHandler<D: Exomonad> {
    ctx: Arc<NodeContext<D>>,
}

/// The engine-side [`SystemCtx`] a domain's `handle_system` operates through: it wraps the live
/// node context, exposing only `own_branch` / `head_sha` / `deliver_parent` / `deliver_to_self`
/// (over the concrete `Runtime` caps + the last-hop dispatch), so the domain handler needs no caps
/// and no last-hop knowledge.
struct NodeSystemCtx<'a, D: Exomonad> {
    ctx: &'a Arc<NodeContext<D>>,
}

#[async_trait]
impl<D: Exomonad> SystemCtx for NodeSystemCtx<'_, D> {
    fn own_branch(&self) -> &Branch {
        self.ctx.runtime.branch()
    }
    async fn head_sha(&self) -> CapResult<String> {
        Ok(exo_caps::Git::head_sha(&*self.ctx.runtime).await?)
    }
    async fn deliver_parent(&self, msg: Message) -> CapResult<()> {
        // `Bus::deliver` logs the authoritative OK line (with `to`/`summary`); only the error path
        // adds context here, so the success case stays a single log line, not three.
        match exo_caps::Bus::deliver(&*self.ctx.runtime, Addressee::Parent, msg).await {
            Ok(()) => Ok(()),
            Err(e) => {
                error!("FAILED to deliver message to parent: {e}");
                Err(exo_caps::CapError::Bus(e))
            }
        }
    }
    async fn deliver_to_self(&self, from: &str, summary: &str, text: &str) -> CapResult<()> {
        // `dispatch` logs the last-hop outcome (tmux paste / Teams inbox); don't double-log the OK.
        match crate::dispatch::deliver_synthetic(self.ctx, from, summary, text).await {
            Ok(()) => Ok(()),
            Err(e) => {
                error!(from = %from, summary = %summary, "FAILED to deliver synthetic message to self: {e}");
                Err(exo_caps::CapError::invalid(
                    "deliver_to_self",
                    e.to_string(),
                ))
            }
        }
    }
    async fn read_file(&self, path: &Path) -> CapResult<Option<Vec<u8>>> {
        match exo_caps::Fs::read(&*self.ctx.runtime, path).await {
            Ok(bytes) => Ok(Some(bytes)),
            Err(exo_caps::FsError::At { source, .. })
                if source.kind() == std::io::ErrorKind::NotFound =>
            {
                Ok(None)
            }
            Err(e) => {
                error!("FAILED to read reviews at {:?}: {e}", path);
                Err(exo_caps::CapError::Fs(e))
            }
        }
    }
    async fn write_file(&self, path: &Path, bytes: &[u8]) -> CapResult<()> {
        match exo_caps::Fs::write_atomic(&*self.ctx.runtime, path, bytes).await {
            Ok(()) => Ok(()),
            Err(e) => {
                error!("FAILED to persist reviews at {:?}: {e}", path);
                Err(exo_caps::CapError::Fs(e))
            }
        }
    }
}

#[async_trait]
impl<D: Exomonad> InboundHandler for RealHandler<D> {
    #[tracing::instrument(skip(self, entry), fields(node = %self.ctx.runtime.name().as_str(), from = %persona_label(&entry.from), kind = %kind_label(&entry.msg.kind)))]
    async fn handle(&self, entry: &IngestionEntry) -> NodeResult<Option<bool>> {
        match &entry.msg.kind {
            // Both chat and event notifications are delivered to the agent's native interface.
            MessageKind::Chat | MessageKind::Event => {
                crate::dispatch::dispatch(&self.ctx, entry).await?;
                Ok(Some(false))
            }
            MessageKind::Control(ControlKind::Shutdown { grace_ms, force }) => {
                self.handle_shutdown(*grace_ms, *force).await
            }
            // Engine-owned lifecycle signals — the sidecar acts on them itself. The enclosing span
            // already carries `from`/`kind`; the per-arm logs in `handle_lifecycle` carry the variant.
            MessageKind::Lifecycle(lc) => {
                self.handle_lifecycle(entry, lc).await?;
                Ok(Some(false))
            }
            // Domain-opaque inter-node payload — typed back to the concrete `D::System` and
            // handed to `D::handle_system` (the one place the erased wire payload is deserialized).
            MessageKind::Domain(payload) => {
                self.handle_domain(&entry.from, payload).await?;
                Ok(Some(false))
            }
        }
    }
}

impl<D: Exomonad> RealHandler<D> {
    /// Handle an engine-owned [`Lifecycle`] signal (sidecar-side; never injected into the LLM
    /// except via the render helpers). These are the closed set the engine acts on itself. Takes
    /// the whole entry, not just its `from`: an arm that both records a fact AND still shows the
    /// message to the LLM needs the original envelope (`ts`/`id`) to re-dispatch it unchanged.
    #[tracing::instrument(skip(self, entry, lc), fields(from = %persona_label(&entry.from), kind = "lifecycle"))]
    async fn handle_lifecycle(&self, entry: &IngestionEntry, lc: &Lifecycle) -> NodeResult<()> {
        let from = &entry.from;
        match lc {
            // A child is about to reap itself — advisory only, receipt doesn't prove the pane is
            // gone. Re-evaluate our own pending shutdown now (covers the common case where the
            // child's pane is already down), and once more after a short delay to cover the
            // window where it's still dying. Pane-liveness (topology) is the sole authority for
            // "this child is gone" — the watchdog tick remains the backstop either way.
            Lifecycle::Exiting { .. } => {
                info!(outcome = "child_exiting", "handling child exit poke");
                try_reap(&self.ctx).await;
                let ctx = self.ctx.clone();
                tokio::spawn(async move {
                    tokio::time::sleep(Duration::from_secs(5)).await;
                    try_reap(&ctx).await;
                });
                Ok(())
            }
            // A child reports `branch@sha` awaiting our merge. Record-then-show: append a
            // `ChildRecord::Submitted` to our own ledger (so the pending-merge queue survives a
            // context window), THEN still render the `[READY]` prose to our LLM exactly as
            // before — recording never replaces showing. Only a report from one of THIS node's
            // own direct children is ever recorded; anything else is rendered only.
            Lifecycle::Submitted {
                branch,
                sha,
                reviewed,
            } => {
                match from {
                    Persona::Agent(child) => {
                        // Deliberately test `topology()`'s children list, not `Runtime::resolve_edge`:
                        // `resolve_edge` returns `None` for a tombstoned child, but a tombstoned
                        // child's submission is still a real fact worth recording. `topology`'s
                        // children list keeps tombstones.
                        let is_own_child = match self.ctx.runtime.topology().await {
                            Ok(view) => view.node.children.iter().any(|c| c.name == *child),
                            Err(e) => {
                                warn!(
                                    outcome = "submitted_render_only",
                                    child = child.as_str(),
                                    "Lifecycle::Submitted: topology read failed ({e}); rendering only, no ledger row"
                                );
                                false
                            }
                        };
                        if should_record_submission(from, is_own_child) {
                            self.ctx
                                .runtime
                                .append_child_record(&ChildRecord::Submitted {
                                    child: child.clone(),
                                    branch: branch.clone(),
                                    sha: sha.clone(),
                                    reviewed: *reviewed,
                                    at: Some(Utc::now()),
                                })
                                .await
                                .map_err(|e| std::io::Error::other(e.to_string()))?;
                        } else {
                            warn!(
                                outcome = "submitted_render_only",
                                child = child.as_str(),
                                "Lifecycle::Submitted from a name that is not one of my direct children; rendering only, no ledger row"
                            );
                        }
                    }
                    Persona::Synthetic(name) => {
                        warn!(
                            outcome = "submitted_render_only",
                            from = name.as_str(),
                            "Lifecycle::Submitted from a non-agent persona; rendering only, no ledger row"
                        );
                    }
                }
                self.redispatch_as_chat(entry).await
            }
            // A child replied to a shutdown we sent it. Render it to our LLM; never tear it down.
            Lifecycle::ShutdownResponse {
                status,
                live_children,
                busy,
                reason,
            } => {
                info!(outcome = "shutdown_response", status = ?status, "handling shutdown response");
                self.render_shutdown_response(from, status, live_children, *busy, reason)
                    .await
            }
        }
    }

    /// Handle a domain-opaque [`MessageKind::Domain`] payload — the **one place** the erased bus
    /// payload is deserialized back to the concrete `D::System` and handed to `D::handle_system`.
    /// The engine performs the lifecycle action the domain returns: [`SystemOutcome::ReclaimSender`]
    /// tears down the sender (e.g. a one-shot reviewer — the engine owns `kill_pane`/`reclaim`).
    /// An undeserializable payload is logged + skipped (tolerant, like a malformed bus line).
    #[tracing::instrument(skip(self, payload), fields(from = %persona_label(from), kind = "domain"))]
    async fn handle_domain(&self, from: &Persona, payload: &DomainPayload) -> NodeResult<()> {
        let system: D::System = match serde_json::from_str(&payload.0) {
            Ok(v) => v,
            Err(e) => {
                // Truncate: a malformed Domain payload is often the multi-KB findings JSON, and a
                // failed parse is exactly when it's large/garbled — don't splat the whole blob.
                // (`from` is already on the span.)
                let preview: String = payload.0.chars().take(200).collect();
                let truncated = payload.0.len() > preview.len();
                warn!(
                    "FAILED to deserialize domain payload: {e}. Raw payload ({} bytes){}: {preview}",
                    payload.0.len(),
                    if truncated { ", first 200 chars" } else { "" }
                );
                return Ok(());
            }
        };
        let sctx = NodeSystemCtx { ctx: &self.ctx };
        let outcome = D::handle_system::<NodeSystemCtx<D>>(&sctx, from, &system)
            .await
            .map_err(|e| std::io::Error::other(e.to_string()))?;
        info!(?outcome, "handle_system completed");
        match outcome {
            SystemOutcome::Done => {}
            SystemOutcome::ReclaimSender => {
                if let Persona::Agent(sender) = from {
                    if let Err(e) = exo_caps::Spawner::kill_pane(&*self.ctx.runtime, sender).await {
                        warn!(
                            "sender teardown: kill_pane({}) failed: {e}",
                            sender.as_str()
                        );
                    }
                    if let Err(e) =
                        exo_caps::Spawner::reclaim_worktree(&*self.ctx.runtime, sender).await
                    {
                        warn!(
                            "sender teardown: reclaim_worktree({}) failed: {e}",
                            sender.as_str()
                        );
                    }
                }
            }
        }
        Ok(())
    }

    /// Show a `Lifecycle` entry to this node's LLM as an ordinary chat line, preserving the
    /// original envelope (`from`/`ts`/`id`) and prose — the sidecar acted on the typed payload,
    /// and the human-readable body is what the agent still needs to read.
    async fn redispatch_as_chat(&self, entry: &IngestionEntry) -> NodeResult<()> {
        crate::dispatch::dispatch(&self.ctx, &rewrite_kind_to_chat(entry)).await
    }

    /// Render a child's [`ShutdownResponse`](exo_caps::Lifecycle::ShutdownResponse) into THIS
    /// node's LLM as a chat line, attributed to the child (`from`). Never tears anyone down.
    async fn render_shutdown_response(
        &self,
        from: &Persona,
        status: &ShutdownStatus,
        live_children: &[AgentName],
        busy: bool,
        reason: &str,
    ) -> NodeResult<()> {
        let summary = match status {
            ShutdownStatus::Deferred => "[shutdown deferred]",
            ShutdownStatus::Accepted => "[shutdown accepted]",
        };
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            id: Some(uuid::Uuid::new_v4().to_string()),
            spill: None,
            from: from.clone(),
            msg: Message {
                text: MessageBody::new(format_shutdown_response(
                    status,
                    live_children,
                    busy,
                    reason,
                ))
                .map_err(|e| std::io::Error::other(e.to_string()))?,
                summary: Summary::new(summary.to_string())
                    .map_err(|e| std::io::Error::other(e.to_string()))?,
                kind: MessageKind::Chat,
                reply_to: None,
            },
        };
        crate::dispatch::dispatch(&self.ctx, &entry).await
    }

    /// Deliver a structured shutdown reply up to this node's parent (the shutdown requester). The
    /// requester's sidecar renders it to a chat line (see `render_shutdown_response`).
    async fn respond_shutdown(
        &self,
        status: ShutdownStatus,
        live_children: Vec<AgentName>,
        busy: bool,
        reason: &str,
    ) -> NodeResult<()> {
        let msg = Message {
            text: MessageBody::new("shutdown response".to_string())
                .map_err(|e| std::io::Error::other(e.to_string()))?,
            summary: Summary::new("[shutdown response]".to_string())
                .map_err(|e| std::io::Error::other(e.to_string()))?,
            kind: MessageKind::Lifecycle(Lifecycle::ShutdownResponse {
                status,
                live_children,
                busy,
                reason: reason.to_string(),
            }),
            reply_to: None,
        };
        exo_caps::Bus::deliver(&*self.ctx.runtime, Addressee::Parent, msg)
            .await
            .map_err(|e| std::io::Error::other(e.to_string()))?;
        Ok(())
    }

    /// Handle a `Control(Shutdown)` per the cooperative/forced matrix (see [`decide`]).
    #[tracing::instrument(skip(self), fields(node = %self.ctx.runtime.name().as_str(), grace_ms = grace_ms, force = force))]
    async fn handle_shutdown(&self, grace_ms: u32, force: bool) -> NodeResult<Option<bool>> {
        let live = match live_children(&self.ctx).await {
            Some(l) => l,
            None => {
                // Couldn't read our subtree → take no destructive action; bounce to the requester.
                warn!(
                    outcome = "deferred",
                    "couldn't read subtree; deferring shutdown"
                );
                self.respond_shutdown(
                    ShutdownStatus::Deferred,
                    vec![],
                    false,
                    "couldn't read my subtree (topology error) — not shutting down. Retry shortly.",
                )
                .await?;
                return Ok(Some(false));
            }
        };

        let action = decide(force, live.is_empty());
        info!(outcome = ?action, "routing shutdown request");

        match action {
            // Forced leaf — reap now (grace applied in try_reap).
            ShutdownAction::ReapNow => {
                self.ctx.set_shutdown_pending(grace_ms);
                Ok(Some(try_reap(&self.ctx).await))
            }
            // Cooperative leaf — wrap up, reap on the watchdog's next periodic tick.
            ShutdownAction::GracefulPending => {
                self.ctx.set_shutdown_pending(grace_ms);
                crate::dispatch::deliver_synthetic(
                    &self.ctx,
                    "shutdown",
                    "[shutdown requested]",
                    "[shutdown requested] Finish your work now — you will be reaped on a periodic check once your subtree is clear; this is a timeout, not an idle-detector.",
                )
                .await?;
                self.respond_shutdown(ShutdownStatus::Accepted, vec![], false, "")
                    .await?;
                Ok(Some(false))
            }
            // Cooperative + live children — bounce an "are you sure" back to the requester.
            ShutdownAction::Defer => {
                let busy = ChildLiveness::any_child_busy(&*self.ctx.runtime).await;
                self.respond_shutdown(ShutdownStatus::Deferred, live.clone(), busy, "")
                    .await?;
                Ok(Some(false))
            }
            // Forced + live children — sidecar cascades a forced teardown, reaps self when clear.
            ShutdownAction::Cascade => {
                self.ctx.set_shutdown_pending(grace_ms);
                let _ = crate::dispatch::deliver_synthetic(
                    &self.ctx,
                    "shutdown",
                    "[shutdown]",
                    "[shutdown] Forced teardown of your subtree in progress.",
                )
                .await;
                for an in &live {
                    let Some(addr) = self.ctx.runtime.resolve_edge(an).await else {
                        warn!(
                            "cascade shutdown: cannot resolve child '{}'; skipping",
                            an.as_str()
                        );
                        continue;
                    };
                    match shutdown_message(grace_ms) {
                        Ok(msg) => {
                            if let Err(e) =
                                exo_caps::Bus::deliver(&*self.ctx.runtime, addr, msg).await
                            {
                                warn!("cascade shutdown: deliver to '{}' failed: {e}", an.as_str());
                            }
                        }
                        Err(e) => warn!("cascade shutdown: build message failed: {e}"),
                    }
                }
                self.respond_shutdown(
                    ShutdownStatus::Accepted,
                    vec![],
                    false,
                    "forced teardown of my subtree is in progress.",
                )
                .await?;
                Ok(Some(false))
            }
        }
    }
}

/// The pure decision behind the `Lifecycle::Submitted` handler: given the sender persona and
/// whether the named child (already resolved against `topology()`'s children list) is one of
/// this node's own direct children, should the report be appended to the ledger? Recording
/// requires BOTH an `Agent` sender and direct-child membership; every other combination renders
/// only. Split out so the truth table is unit-testable without a live `NodeContext`/`Runtime`.
fn should_record_submission(from: &Persona, is_own_child: bool) -> bool {
    matches!(from, Persona::Agent(_)) && is_own_child
}

/// Rewrite an entry's `kind` to `Chat`, preserving `from`/`ts`/`id`/`spill` and the message body
/// untouched — the pure transform behind [`RealHandler::redispatch_as_chat`], split out so it's
/// unit-testable without a live `NodeContext`.
fn rewrite_kind_to_chat(entry: &IngestionEntry) -> IngestionEntry {
    IngestionEntry {
        msg: Message {
            kind: MessageKind::Chat,
            ..entry.msg.clone()
        },
        ..entry.clone()
    }
}

/// What to do with a `Control(Shutdown)`, by `force` × whether the subtree is empty.
#[derive(Debug, PartialEq, Eq)]
enum ShutdownAction {
    /// forced leaf → reap immediately.
    ReapNow,
    /// cooperative leaf → wrap up, reap on the watchdog's next periodic tick.
    GracefulPending,
    /// cooperative with live children → bounce "are you sure" to the requester.
    Defer,
    /// forced with live children → cascade a forced teardown.
    Cascade,
}

fn decide(force: bool, childless: bool) -> ShutdownAction {
    match (force, childless) {
        (true, true) => ShutdownAction::ReapNow,
        (false, true) => ShutdownAction::GracefulPending,
        (false, false) => ShutdownAction::Defer,
        (true, false) => ShutdownAction::Cascade,
    }
}

/// Build the chat text a requester sees for a child's [`ShutdownResponse`](exo_caps::Lifecycle::ShutdownResponse).
/// Pure (no IO) so it unit-tests like `decide`. The dispatch header already attributes the line to
/// the child, so the wording is first-person (the child speaking).
fn format_shutdown_response(
    status: &ShutdownStatus,
    live_children: &[AgentName],
    busy: bool,
    reason: &str,
) -> String {
    match status {
        ShutdownStatus::Deferred => {
            if live_children.is_empty() {
                format!("[shutdown deferred] {reason}")
            } else {
                let work = if busy { " (some actively working)" } else { "" };
                format!(
                    "[shutdown deferred] I have {} live child(ren): {}{work}. They'd be orphaned. \
                     Re-send shutdown with force:true to tear down the whole subtree, or shut them \
                     down individually first.",
                    live_children.len(),
                    live_children
                        .iter()
                        .map(|n| n.as_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
        }
        ShutdownStatus::Accepted => {
            if reason.is_empty() {
                "[shutdown accepted] Finishing up; reap happens on the next periodic check."
                    .to_string()
            } else {
                format!("[shutdown accepted] {reason}")
            }
        }
    }
}

fn any_live(n: &TreeNode) -> bool {
    n.pane_alive || n.children.iter().any(any_live)
}

/// Names of this node's direct children that still have a live pane (recursively). `None` if the
/// topology read failed (caller must not treat that as "childless").
async fn live_children<D: Exomonad>(ctx: &Arc<NodeContext<D>>) -> Option<Vec<AgentName>> {
    match ctx.runtime.topology().await {
        Ok(view) => Some(
            view.node
                .children
                .iter()
                .filter(|c| any_live(c))
                .map(|c| c.name.clone())
                .collect(),
        ),
        Err(e) => {
            warn!("shutdown: topology read failed: {e}");
            None
        }
    }
}

/// Live direct children per pane-liveness (topology) — the sole authority for "this child is
/// gone". On a topology error, returns a non-empty sentinel so `try_reap` errs toward NOT reaping.
async fn remaining_live_children<D: Exomonad>(ctx: &Arc<NodeContext<D>>) -> Vec<AgentName> {
    match ctx.runtime.topology().await {
        Ok(view) => view
            .node
            .children
            .iter()
            .filter(|c| any_live(c))
            .map(|c| c.name.clone())
            .collect(),
        Err(_) => vec![AgentName::new("<topology-error>".to_string())
            .expect("static sentinel is a valid AgentName")],
    }
}

/// Build a forced child-shutdown message (used by the cascade).
fn shutdown_message(grace_ms: u32) -> NodeResult<Message> {
    Ok(Message {
        text: MessageBody::new("forced subtree shutdown".to_string())
            .map_err(|e| std::io::Error::other(e.to_string()))?,
        summary: Summary::new("[shutdown]".to_string())
            .map_err(|e| std::io::Error::other(e.to_string()))?,
        kind: MessageKind::Control(ControlKind::Shutdown {
            grace_ms,
            force: true,
        }),
        reply_to: None,
    })
}

/// Reap this node iff it's shutdown-pending and its subtree is clear: send the advisory
/// `Lifecycle::Exiting` poke up, wait the grace backstop, then kill its own pane (ending the
/// process). Returns whether it reaped. Idempotent and safe to call at any time; a no-op when not
/// pending or when children remain (per pane-liveness). Callers: the watchdog tick (unconditional,
/// every interval — the backstop), and the inbound loop's `Lifecycle::Exiting` handler (an
/// immediate re-check plus a delayed one, both best-effort early triggers).
pub(crate) async fn try_reap<D: Exomonad>(ctx: &Arc<NodeContext<D>>) -> bool {
    let grace = *ctx.shutdown_pending.lock().unwrap();
    let Some(grace_ms) = grace else {
        return false; // not shutting down
    };
    if !remaining_live_children(ctx).await.is_empty() {
        return false; // subtree not clear yet
    }
    info!(node = %ctx.runtime.name().as_str(), outcome = "reaping", "try_reap: shutdown pending and subtree clear — reaping self");
    // Poke the parent to re-check its own pending shutdown (advisory only — receipt doesn't prove
    // our pane is gone yet). Root has no parent — that delivery just errors, which is fine.
    let exiting = Message {
        text: MessageBody::new("[exiting] shutdown".to_string())
            .ok()
            .unwrap_or_else(|| MessageBody::new("exiting".to_string()).unwrap()),
        summary: Summary::new("[exiting]".to_string()).unwrap(),
        kind: MessageKind::Lifecycle(Lifecycle::Exiting {
            reason: "shutdown".to_string(),
        }),
        reply_to: None,
    };
    if let Err(e) = exo_caps::Bus::deliver(&*ctx.runtime, Addressee::Parent, exiting).await {
        warn!(node = %ctx.runtime.name().as_str(), "try_reap: Exiting poke to parent failed (ok for root): {e}");
    }
    if grace_ms > 0 {
        tokio::time::sleep(Duration::from_millis(grace_ms as u64)).await;
    }
    // Bounded retry around the own-pane kill — a self-reap that loses the race with tmux settling
    // would otherwise leave a zombie pane. Best-effort: a final failure is surfaced (return false)
    // so the caller doesn't claim a reap that didn't happen, but never escalated to a panic.
    let kill = exo_runtime::retry_teardown("self_kill_pane", ctx.runtime.name().as_str(), || {
        Tmux::kill_pane(&*ctx.runtime, &ctx.own_pane)
    })
    .await;
    if let Err(e) = kill {
        warn!(node = %ctx.runtime.name().as_str(), "try_reap: kill_pane failed after retries: {e}");
        return false;
    }
    true
}

/// Returns true if shutdown was requested
async fn process_inbox<H: InboundHandler>(
    node: &str,
    handler: &H,
    inbox_path: &Path,
    cursor_path: &Path,
    offset: &mut u64,
) -> NodeResult<bool> {
    let mut file = match File::open(inbox_path) {
        Ok(f) => f,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(e) => return Err(e.into()),
    };

    let file_len = file.metadata()?.len();
    if *offset >= file_len {
        return Ok(false);
    }

    file.seek(SeekFrom::Start(*offset))?;

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    // Find the last newline to avoid processing torn lines
    let last_newline = match buffer.iter().rposition(|&b| b == b'\n') {
        Some(pos) => pos,
        None => return Ok(false), // No complete lines
    };

    let complete_data = &buffer[..=last_newline];

    for line_bytes in complete_data.split(|&b| b == b'\n') {
        if line_bytes.is_empty() {
            continue;
        }

        let line_len = line_bytes.len() as u64;
        let parsed: IngestionEntry = match serde_json::from_slice(line_bytes) {
            Ok(e) => e,
            Err(e) => {
                warn!(node = %node, "failed to parse ingestion entry: {}", e);
                // Advance past malformed line
                *offset += line_len + 1;
                if let Err(e) = save_cursor(cursor_path, *offset) {
                    warn!(node = %node, "failed to persist cursor (will retry next wake): {e}");
                }
                continue;
            }
        };

        // Claim-check: if this line is a spill pointer, load the full (oversized) entry from its
        // side-file. A normal entry passes through untouched.
        let entry = match resolve_spilled(parsed) {
            Ok(e) => e,
            Err(e) => {
                warn!(node = %node, "failed to resolve spilled entry, skipping: {e}");
                *offset += line_len + 1;
                if let Err(e) = save_cursor(cursor_path, *offset) {
                    warn!(node = %node, "failed to persist cursor (will retry next wake): {e}");
                }
                continue;
            }
        };

        match handler.handle(&entry).await {
            Ok(Some(true)) => {
                // Shutdown
                *offset += line_len + 1;
                if let Err(e) = save_cursor(cursor_path, *offset) {
                    warn!(node = %node, "failed to persist cursor (will retry next wake): {e}");
                }
                return Ok(true);
            }
            Ok(_) => {
                // Success (or no-op), advance cursor. The cursor advances ONLY after a successful
                // last-hop delivery, so redelivery is at-least-once BY DESIGN — a retried line
                // arrives with its ORIGINAL `id`. `IngestionEntry::id` is reference-only: it names
                // a message for logs/`reply_to`, never a dedup key. No code anywhere treats a
                // repeated id as "already seen" — doing so would silently drop the very retry this
                // cursor protocol exists to guarantee.
                *offset += line_len + 1;
                if let Err(e) = save_cursor(cursor_path, *offset) {
                    warn!(node = %node, "failed to persist cursor (will retry next wake): {e}");
                }
            }
            Err(e) => {
                error!(node = %node, "failed to route entry: {}. will retry on next wake", e);
                // DO NOT advance cursor. Break batch to retry later.
                return Ok(false);
            }
        }
    }

    Ok(false)
}

/// Resolve a claim-check pointer (see [`IngestionEntry::spill`]): if `spill` is set, load + parse the
/// full entry from its side-file; otherwise pass the entry through. The loaded entry carries `spill:
/// None`, so this never recurses. The side-file is left in place during the run — leaving it is
/// what keeps an at-least-once re-read idempotent. Dead runs' side-files (and the whole home-dir
/// run state) are reclaimed by `exo doctor --fix`'s run-artifact GC pass, never by the sidecar.
fn resolve_spilled(entry: IngestionEntry) -> std::io::Result<IngestionEntry> {
    match &entry.spill {
        None => Ok(entry),
        Some(path) => {
            let bytes = std::fs::read(path)?;
            serde_json::from_slice(&bytes).map_err(std::io::Error::other)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use exo_caps::{AgentName, Message, MessageBody, Persona, Summary};
    use std::fs::OpenOptions;
    use std::sync::Mutex;

    fn sample_entry(text: &str) -> IngestionEntry {
        IngestionEntry {
            v: 1,
            ts: Utc::now(),
            id: None,
            spill: None,
            from: Persona::Agent(AgentName::new("rev".into()).unwrap()),
            msg: Message {
                text: MessageBody::new(text.to_string()).unwrap(),
                summary: Summary::new("s".into()).unwrap(),
                kind: MessageKind::Chat,
                reply_to: None,
            },
        }
    }

    #[test]
    fn should_record_submission_truth_table() {
        let agent = Persona::Agent(AgentName::new("dev-0".into()).unwrap());
        let synthetic = Persona::Synthetic(exo_caps::SyntheticName::new("github".into()).unwrap());

        assert!(should_record_submission(&agent, true));
        assert!(!should_record_submission(&agent, false));
        assert!(!should_record_submission(&synthetic, true));
        assert!(!should_record_submission(&synthetic, false));
    }

    #[test]
    fn rewrite_kind_to_chat_preserves_envelope_and_rewrites_only_kind() {
        let mut entry = sample_entry("branch@sha ready");
        entry.id = Some("11111111-2222-3333-4444-555555555555".into());
        entry.msg.kind = MessageKind::Lifecycle(Lifecycle::Submitted {
            branch: Branch::new("root.dev-0".into()).unwrap(),
            sha: "deadbeef".into(),
            reviewed: true,
        });

        let chat = rewrite_kind_to_chat(&entry);

        assert_eq!(chat.from, entry.from);
        assert_eq!(chat.ts, entry.ts);
        assert_eq!(chat.id, entry.id);
        assert_eq!(chat.msg.text, entry.msg.text);
        assert_eq!(chat.msg.summary, entry.msg.summary);
        assert_eq!(chat.msg.kind, MessageKind::Chat);
    }

    #[test]
    fn resolve_spilled_loads_full_entry_from_side_file() {
        let dir = tempfile::tempdir().unwrap();
        // A "spilled" full entry written to a side-file (as the bus would).
        let full = sample_entry("a very large verdict payload");
        let path = dir.path().join("spill-1.json");
        std::fs::write(&path, serde_json::to_vec(&full).unwrap()).unwrap();

        // A pointer line that references it (stub body; real content is in the file).
        let mut pointer = sample_entry("[spilled]");
        pointer.spill = Some(path.to_string_lossy().into_owned());

        let resolved = resolve_spilled(pointer).unwrap();
        assert!(resolved.spill.is_none());
        assert_eq!(resolved.msg, full.msg);
    }

    #[test]
    fn resolve_spilled_passes_through_a_normal_entry() {
        let entry = sample_entry("inline");
        let resolved = resolve_spilled(entry.clone()).unwrap();
        assert_eq!(resolved, entry);
    }

    #[test]
    fn shutdown_response_render_text() {
        let deferred = format_shutdown_response(
            &ShutdownStatus::Deferred,
            &[
                AgentName::new("a".to_string()).unwrap(),
                AgentName::new("b".to_string()).unwrap(),
            ],
            true,
            "",
        );
        assert!(deferred.contains("2 live child(ren): a, b"));
        assert!(deferred.contains("force:true"));
        assert!(deferred.contains("actively working"));
        let topo = format_shutdown_response(&ShutdownStatus::Deferred, &[], false, "boom");
        assert_eq!(topo, "[shutdown deferred] boom");
        let accepted = format_shutdown_response(&ShutdownStatus::Accepted, &[], false, "");
        assert!(accepted.starts_with("[shutdown accepted]"));
    }

    #[test]
    fn shutdown_decision_matrix() {
        // force × childless → action. The whole behavioural spec in one table.
        assert_eq!(decide(false, true), ShutdownAction::GracefulPending); // polite leaf
        assert_eq!(decide(false, false), ShutdownAction::Defer); // polite + subtree → "are you sure"
        assert_eq!(decide(true, true), ShutdownAction::ReapNow); // forced leaf
        assert_eq!(decide(true, false), ShutdownAction::Cascade); // forced + subtree → teardown
    }
    use tempfile::tempdir;

    struct MockHandler {
        delivered: Arc<Mutex<Vec<IngestionEntry>>>,
        fail_on: Option<String>,
    }

    #[async_trait]
    impl InboundHandler for MockHandler {
        async fn handle(&self, entry: &IngestionEntry) -> NodeResult<Option<bool>> {
            if let Some(fail_text) = &self.fail_on {
                if entry.msg.text.as_str() == fail_text {
                    return Err(std::io::Error::other("mock failure").into());
                }
            }
            self.delivered.lock().unwrap().push(entry.clone());
            Ok(Some(false))
        }
    }

    fn write_entry(path: &Path, text: &str) {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(AgentName::new("test".to_string()).unwrap()),
            id: None,
            spill: None,
            msg: Message {
                text: MessageBody::new(text.to_string()).unwrap(),
                summary: Summary::new("test".to_string()).unwrap(),
                kind: MessageKind::Chat,
                reply_to: None,
            },
        };
        let mut line = serde_json::to_vec(&entry).unwrap();
        line.push(b'\n');
        let mut f = OpenOptions::new()
            .append(true)
            .create(true)
            .open(path)
            .unwrap();
        f.write_all(&line).unwrap();
    }

    #[tokio::test]
    async fn test_process_inbox_basic() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = dir.path().join("pane-1.cursor");
        let mut offset = 0;

        write_entry(&inbox_path, "one");
        write_entry(&inbox_path, "two");
        write_entry(&inbox_path, "three");

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };

        process_inbox(
            "test-node",
            &handler,
            &inbox_path,
            &cursor_path,
            &mut offset,
        )
        .await
        .unwrap();

        let d = delivered.lock().unwrap();
        assert_eq!(d.len(), 3);
        assert_eq!(d[0].msg.text.as_str(), "one");
        assert_eq!(d[2].msg.text.as_str(), "three");
        assert_eq!(offset, get_eof(&inbox_path));
    }

    #[tokio::test]
    async fn test_process_inbox_torn_line() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = dir.path().join("pane-1.cursor");
        let mut offset = 0;

        write_entry(&inbox_path, "one");
        // Write partial line without newline
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(AgentName::new("test".to_string()).unwrap()),
            id: None,
            spill: None,
            msg: Message {
                text: MessageBody::new("partial".to_string()).unwrap(),
                summary: Summary::new("test".to_string()).unwrap(),
                kind: MessageKind::Chat,
                reply_to: None,
            },
        };
        let line = serde_json::to_vec(&entry).unwrap();
        let mut f = OpenOptions::new()
            .append(true)
            .create(true)
            .open(&inbox_path)
            .unwrap();
        f.write_all(&line).unwrap();

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };

        process_inbox(
            "test-node",
            &handler,
            &inbox_path,
            &cursor_path,
            &mut offset,
        )
        .await
        .unwrap();

        let d = delivered.lock().unwrap();
        assert_eq!(d.len(), 1);
        assert_eq!(d[0].msg.text.as_str(), "one");
        // Cursor should be at the end of the first line
        let first_line_len =
            File::open(&inbox_path).unwrap().metadata().unwrap().len() - line.len() as u64;
        assert_eq!(offset, first_line_len);
    }

    #[tokio::test]
    async fn test_process_inbox_at_least_once() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = dir.path().join("pane-1.cursor");
        let mut offset = 0;

        write_entry(&inbox_path, "one");
        write_entry(&inbox_path, "two");

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: Some("two".to_string()),
        };

        // Should deliver "one", fail on "two", and NOT advance cursor past "two"
        process_inbox(
            "test-node",
            &handler,
            &inbox_path,
            &cursor_path,
            &mut offset,
        )
        .await
        .unwrap();

        {
            let d = delivered.lock().unwrap();
            assert_eq!(d.len(), 1);
            assert_eq!(d[0].msg.text.as_str(), "one");
        }

        // Offset should be after "one" but before "two"
        // Let's find real offset
        let f = File::open(&inbox_path).unwrap();
        let mut reader = std::io::BufReader::new(f);
        let mut line = String::new();
        std::io::BufRead::read_line(&mut reader, &mut line).unwrap();
        let expected_offset = line.len() as u64;
        assert_eq!(offset, expected_offset);

        // Second pass with NO failure
        let handler2 = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };
        process_inbox(
            "test-node",
            &handler2,
            &inbox_path,
            &cursor_path,
            &mut offset,
        )
        .await
        .unwrap();

        {
            let d = delivered.lock().unwrap();
            assert_eq!(d.len(), 2);
            assert_eq!(d[1].msg.text.as_str(), "two");
        }
        assert_eq!(offset, get_eof(&inbox_path));
    }

    #[tokio::test]
    async fn test_missing_cursor_starts_at_eof() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = inbox_path.with_extension("cursor");

        write_entry(&inbox_path, "pre-existing");

        // Simulate watch(ctx) start
        let mut offset = get_eof(&inbox_path);
        save_cursor(&cursor_path, offset).unwrap();

        write_entry(&inbox_path, "new");

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };

        process_inbox(
            "test-node",
            &handler,
            &inbox_path,
            &cursor_path,
            &mut offset,
        )
        .await
        .unwrap();

        let d = delivered.lock().unwrap();
        assert_eq!(d.len(), 1);
        assert_eq!(d[0].msg.text.as_str(), "new");
    }

    #[tokio::test]
    async fn test_cursor_durability_across_restart() {
        let dir = tempdir().unwrap();
        let inbox_path = dir.path().join("pane-1.jsonl");
        let cursor_path = inbox_path.with_extension("cursor");
        let mut offset = 0;

        // 1. Process N entries
        write_entry(&inbox_path, "one");
        write_entry(&inbox_path, "two");

        let delivered = Arc::new(Mutex::new(Vec::new()));
        let handler = MockHandler {
            delivered: delivered.clone(),
            fail_on: None,
        };

        process_inbox(
            "test-node",
            &handler,
            &inbox_path,
            &cursor_path,
            &mut offset,
        )
        .await
        .unwrap();

        {
            let d = delivered.lock().unwrap();
            assert_eq!(d.len(), 2);
        }

        // 2. Simulate restart: reload offset from cursor file
        let mut new_offset = std::fs::read_to_string(&cursor_path)
            .unwrap()
            .trim()
            .parse::<u64>()
            .unwrap();
        assert_eq!(new_offset, offset);
        assert!(new_offset > 0);

        // 3. Append M more entries
        write_entry(&inbox_path, "three");
        write_entry(&inbox_path, "four");

        let delivered2 = Arc::new(Mutex::new(Vec::new()));
        let handler2 = MockHandler {
            delivered: delivered2.clone(),
            fail_on: None,
        };

        // 4. Process again, should only get the M new ones
        process_inbox(
            "test-node",
            &handler2,
            &inbox_path,
            &cursor_path,
            &mut new_offset,
        )
        .await
        .unwrap();

        {
            let d = delivered2.lock().unwrap();
            assert_eq!(d.len(), 2);
            assert_eq!(d[0].msg.text.as_str(), "three");
            assert_eq!(d[1].msg.text.as_str(), "four");
        }
    }
}
