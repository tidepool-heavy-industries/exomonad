//! **N2b — Inbound loop.** Drive the Bus READ side of this node's own ingestion inbox — the
//! cursor/restart half the `Bus` cap (write side) leaves to the reader. The contract:
//!
//! - **Cursor = byte-offset** in a sibling `pane-N.cursor`. Resume = seek + read forward, O(1).
//! - **Watch via the `notify` crate** (event-driven, never a poll loop, never hand-rolled
//!   inotify); on each wake re-read from the cursor (absorbs coalesced events).
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
//!   requester; a leaf winds down and is reaped on its next idle; a forced request cascades a
//!   subtree teardown. The actual self-reap (`try_reap`) only fires when the subtree is clear,
//!   signalling `ChildExited` up first. A child's `ChildExited` re-triggers its parent's reap.

use std::fs::File;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::mpsc;
use tracing::{error, info, warn};

use chrono::Utc;
use exo_caps::{
    Addressee, AgentName, ChildLiveness, ControlKind, IngestionEntry, Message, MessageBody,
    MessageKind, Persona, Summary, SyntheticName, SystemMessage, Tmux, Topology, TreeNode,
};

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Watch the node's own ingestion inbox and route each new entry until shutdown.
pub async fn watch(ctx: Arc<NodeContext>) -> NodeResult<()> {
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

    // Setup notify watcher
    let (tx, mut rx) = mpsc::channel(100);
    let mut watcher = RecommendedWatcher::new(
        move |res: notify::Result<Event>| {
            if let Ok(event) = res {
                if event.kind.is_modify() || event.kind.is_create() {
                    let _ = tx.blocking_send(());
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

    // Initial pass to catch anything already there. A transient failure (file/cursor IO) must
    // not stop the loop — the next notify wake re-reads from the unchanged offset.
    if let Err(e) = process_inbox(&handler, &inbox_path, &cursor_path, &mut offset).await {
        warn!("inbound initial pass failed (will retry on next event): {e}");
    }

    while let Some(()) = rx.recv().await {
        // Drain any coalesced events
        while rx.try_recv().is_ok() {}

        match process_inbox(&handler, &inbox_path, &cursor_path, &mut offset).await {
            Ok(true) => break, // shutdown received
            Ok(false) => {}
            Err(e) => warn!("inbound pass failed (will retry on next event): {e}"),
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

struct RealHandler {
    ctx: Arc<NodeContext>,
}

#[async_trait]
impl InboundHandler for RealHandler {
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
            // System signals are consumed by the sidecar, never delivered to the LLM directly.
            MessageKind::System(system) => {
                self.handle_system(&entry.from, system).await?;
                Ok(Some(false))
            }
        }
    }
}

impl RealHandler {
    /// Route a [`SystemMessage`] (sidecar-side; never injected into the LLM directly).
    ///
    /// A **review verdict** comes from a one-shot reviewer that is this node's own `Worktree`
    /// child — it's done after the verdict, and its branch never merges, so `teardown-on-merge`
    /// would miss it. Apply the verdict, then reclaim that reviewer here, best-effort, regardless
    /// of outcome. A **`ChildIdle`** comes from a LIVE child finishing a turn — render it and do
    /// NOT tear the child down.
    async fn handle_system(&self, from: &Persona, system: &SystemMessage) -> NodeResult<()> {
        match system {
            // A child yielded control. Flip its busy-bit to idle (the idle gate reads this), then
            // render a concise line for this node's LLM; never tear the child down. (v1: no
            // dedupe — volume is accepted; the refine-later seam is here.)
            SystemMessage::ChildIdle { summary } => {
                if let Persona::Agent(name) = from {
                    self.ctx.runtime.mark_child_idle(name);
                }
                self.render_child_idle(from, summary).await
            }
            // A child reaped itself (its shutdown completed). Record it in the authoritative
            // exited-set, then re-evaluate our own pending shutdown — if it was the last child, we
            // reap ourselves now (which may kill our pane and end the process).
            SystemMessage::ChildExited { .. } => {
                if let Persona::Agent(name) = from {
                    self.ctx
                        .exited_children
                        .lock()
                        .unwrap()
                        .insert(name.as_str().to_string());
                }
                try_reap(&self.ctx).await;
                Ok(())
            }
            // Review verdicts: apply, then reclaim the one-shot reviewer (verdict-only teardown).
            SystemMessage::ReviewApproved { .. }
            | SystemMessage::ReviewDenied { .. }
            | SystemMessage::ReviewChanges { .. } => {
                let result = self.apply_verdict(system).await;
                if let Persona::Agent(reviewer) = from {
                    if let Err(e) = exo_caps::Spawner::kill_pane(&*self.ctx.runtime, reviewer).await
                    {
                        warn!(
                            "reviewer teardown: kill_pane({}) failed: {e}",
                            reviewer.as_str()
                        );
                    }
                    if let Err(e) =
                        exo_caps::Spawner::reclaim_worktree(&*self.ctx.runtime, reviewer).await
                    {
                        warn!(
                            "reviewer teardown: reclaim_worktree({}) failed: {e}",
                            reviewer.as_str()
                        );
                    }
                }
                result
            }
        }
    }

    /// Render a concise \"child yielded control\" line into THIS node's LLM. The sender is a LIVE
    /// child (not a one-shot reviewer), so it is NOT torn down. Preserves the child's identity as
    /// `from` so the dispatch header attributes the line correctly.
    async fn render_child_idle(&self, from: &Persona, summary: &str) -> NodeResult<()> {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: from.clone(),
            msg: Message {
                text: MessageBody::new(format!("[child idle] {summary}"))
                    .map_err(|e| std::io::Error::other(e.to_string()))?,
                summary: Summary::new("[child idle]".into())
                    .map_err(|e| std::io::Error::other(e.to_string()))?,
                kind: MessageKind::Chat,
            },
        };
        crate::dispatch::dispatch(&self.ctx, &entry).await
    }

    /// Act on a review verdict (escalate `[READY]` on a matching approval; wake the LLM on
    /// deny/changes). The sender's lifecycle is handled by [`handle_system`](Self::handle_system).
    async fn apply_verdict(&self, system: &SystemMessage) -> NodeResult<()> {
        match system {
            SystemMessage::ReviewApproved { branch, sha } => {
                // The approval must be for THIS node's branch at its CURRENT commit. A mismatched
                // branch (with the right sha) must not escalate [READY] for my branch, and a stale
                // sha (work committed after the review) needs a fresh review.
                let my_branch = self.ctx.runtime.branch().clone();
                if branch.as_str() != my_branch.as_str() {
                    warn!(
                        "approval names branch {} but my branch is {} — ignoring",
                        branch.as_str(),
                        my_branch.as_str()
                    );
                    return Ok(());
                }
                let head = exo_caps::Git::head_sha(&*self.ctx.runtime)
                    .await
                    .map_err(|e| std::io::Error::other(e.to_string()))?;
                if &head != sha {
                    warn!(
                        "stale approval for {} @ {} (HEAD is {}) — ignoring",
                        branch.as_str(),
                        sha,
                        head
                    );
                    return Ok(());
                }
                // Escalate [READY] to the parent — sidecar-side, no LLM turn.
                let text = format!(
                    "[READY] branch `{}` was approved by review and is ready for merge.",
                    my_branch.as_str()
                );
                let summary = format!("[READY] {}", my_branch.as_str());
                let msg = Message {
                    text: MessageBody::new(text).map_err(|e| std::io::Error::other(e.to_string()))?,
                    summary: Summary::new(summary)
                        .map_err(|e| std::io::Error::other(e.to_string()))?,
                    kind: MessageKind::Chat,
                };
                exo_caps::Bus::deliver(&*self.ctx.runtime, Addressee::Parent, msg)
                    .await
                    .map_err(|e| std::io::Error::other(e.to_string()))?;
                info!(
                    "review approved for {} — escalated [READY] to parent",
                    my_branch.as_str()
                );
                Ok(())
            }
            SystemMessage::ReviewDenied { message, .. } => {
                self.deliver_to_llm(&format!(
                    "[REVIEW: changes requested] Your branch was not approved. Address this feedback, commit, then call submit_branch again:\n{}",
                    message
                )).await
            }
            SystemMessage::ReviewChanges {
                changes_branch,
                message,
                ..
            } => {
                self.deliver_to_llm(&format!(
                    "[REVIEW: proposed changes] The reviewer committed improvements on branch `{}`. Merge it with the `merge` tool to incorporate, then call submit_branch again:\n{}",
                    changes_branch.as_str(), message
                )).await
            }
            // `ChildIdle`/`ChildExited` are intercepted in `handle_system`, never routed here.
            SystemMessage::ChildIdle { .. } | SystemMessage::ChildExited { .. } => {
                unreachable!("ChildIdle/ChildExited handled in handle_system, never apply_verdict")
            }
        }
    }

    /// Inject a message into THIS node's own LLM conversation via the last-hop dispatch, attributed
    /// to a synthetic sender.
    async fn deliver_to_llm(&self, text: &str) -> NodeResult<()> {
        self.deliver_to_self("reviewer", "[REVIEW]", text).await
    }

    /// Render `text` into THIS node's own LLM, attributed to synthetic `from` with `summary`.
    async fn deliver_to_self(&self, from: &str, summary: &str, text: &str) -> NodeResult<()> {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Synthetic(
                SyntheticName::new(from.to_string())
                    .map_err(|e| std::io::Error::other(e.to_string()))?,
            ),
            msg: Message {
                text: MessageBody::new(text.to_string())
                    .map_err(|e| std::io::Error::other(e.to_string()))?,
                summary: Summary::new(summary.to_string())
                    .map_err(|e| std::io::Error::other(e.to_string()))?,
                kind: MessageKind::Chat,
            },
        };
        crate::dispatch::dispatch(&self.ctx, &entry).await
    }

    /// Deliver a chat message up to this node's parent (the shutdown requester).
    async fn notify_parent_chat(&self, text: &str, summary: &str) -> NodeResult<()> {
        let msg = Message {
            text: MessageBody::new(text.to_string())
                .map_err(|e| std::io::Error::other(e.to_string()))?,
            summary: Summary::new(summary.to_string())
                .map_err(|e| std::io::Error::other(e.to_string()))?,
            kind: MessageKind::Chat,
        };
        exo_caps::Bus::deliver(&*self.ctx.runtime, Addressee::Parent, msg)
            .await
            .map_err(|e| std::io::Error::other(e.to_string()))?;
        Ok(())
    }

    /// Handle a `Control(Shutdown)` per the cooperative/forced matrix (see [`decide`]).
    async fn handle_shutdown(&self, grace_ms: u32, force: bool) -> NodeResult<Option<bool>> {
        let live = match live_children(&self.ctx).await {
            Some(l) => l,
            None => {
                // Couldn't read our subtree → take no destructive action; bounce to the requester.
                self.notify_parent_chat(
                    "[shutdown deferred] couldn't read my subtree (topology error) — not shutting down. Retry shortly.",
                    "[shutdown deferred]",
                )
                .await?;
                return Ok(Some(false));
            }
        };

        match decide(force, live.is_empty()) {
            // Forced leaf — reap now (grace applied in try_reap).
            ShutdownAction::ReapNow => {
                self.ctx.set_shutdown_pending(grace_ms);
                Ok(Some(try_reap(&self.ctx).await))
            }
            // Cooperative leaf — wrap up, reap on next idle (the stop hook drives try_reap).
            ShutdownAction::GracefulPending => {
                self.ctx.set_shutdown_pending(grace_ms);
                self.deliver_to_self(
                    "shutdown",
                    "[shutdown requested]",
                    "[shutdown requested] Finish your work and yield — you'll be reaped when you go idle.",
                )
                .await?;
                Ok(Some(false))
            }
            // Cooperative + live children — bounce an "are you sure" back to the requester.
            ShutdownAction::Defer => {
                let busy = ChildLiveness::any_child_busy(&*self.ctx.runtime).await;
                let work = if busy { " (some actively working)" } else { "" };
                self.notify_parent_chat(
                    &format!(
                        "[shutdown deferred] I have {} live child(ren): {}{work}. They'd be orphaned. \
                         Re-send shutdown with force:true to tear down the whole subtree, or shut them \
                         down individually first.",
                        live.len(),
                        live.join(", "),
                    ),
                    "[shutdown deferred]",
                )
                .await?;
                Ok(Some(false))
            }
            // Forced + live children — sidecar cascades a forced teardown, reaps self when clear.
            ShutdownAction::Cascade => {
                self.ctx.set_shutdown_pending(grace_ms);
                let _ = self
                    .deliver_to_self(
                        "shutdown",
                        "[shutdown]",
                        "[shutdown] Forced teardown of your subtree in progress.",
                    )
                    .await;
                for name in &live {
                    let Ok(an) = AgentName::new(name.clone()) else {
                        continue;
                    };
                    let Some(addr) = self.ctx.runtime.resolve_edge(&an).await else {
                        warn!("cascade shutdown: cannot resolve child '{name}'; skipping");
                        continue;
                    };
                    match shutdown_message(grace_ms) {
                        Ok(msg) => {
                            if let Err(e) =
                                exo_caps::Bus::deliver(&*self.ctx.runtime, addr, msg).await
                            {
                                warn!("cascade shutdown: deliver to '{name}' failed: {e}");
                            }
                        }
                        Err(e) => warn!("cascade shutdown: build message failed: {e}"),
                    }
                }
                Ok(Some(false))
            }
        }
    }
}

/// What to do with a `Control(Shutdown)`, by `force` × whether the subtree is empty.
#[derive(Debug, PartialEq, Eq)]
enum ShutdownAction {
    /// forced leaf → reap immediately.
    ReapNow,
    /// cooperative leaf → wrap up, reap on next idle.
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

fn any_live(n: &TreeNode) -> bool {
    n.pane_alive || n.children.iter().any(any_live)
}

/// Names of this node's direct children that still have a live pane (recursively). `None` if the
/// topology read failed (caller must not treat that as "childless").
async fn live_children(ctx: &Arc<NodeContext>) -> Option<Vec<String>> {
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

/// Live direct children NOT yet known-exited (the authoritative gone-set). On a topology error,
/// returns a non-empty sentinel so `try_reap` errs toward NOT reaping.
async fn remaining_live_children(ctx: &Arc<NodeContext>) -> Vec<String> {
    let exited = ctx.exited_children.lock().unwrap().clone();
    match ctx.runtime.topology().await {
        Ok(view) => view
            .node
            .children
            .iter()
            .filter(|c| any_live(c))
            .map(|c| c.name.clone())
            .filter(|n| !exited.contains(n))
            .collect(),
        Err(_) => vec!["<topology-error>".to_string()],
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
    })
}

/// Reap this node iff it's shutdown-pending and its subtree is clear: signal `ChildExited` up,
/// wait the grace backstop, then kill its own pane (ending the process). Returns whether it
/// reaped. Idempotent and safe to call on any stop / child-exit; a no-op when not pending or when
/// children remain. Called from the stop-hook path (idle) and on inbound `ChildExited`.
pub(crate) async fn try_reap(ctx: &Arc<NodeContext>) -> bool {
    let grace = *ctx.shutdown_pending.lock().unwrap();
    let Some(grace_ms) = grace else {
        return false; // not shutting down
    };
    if !remaining_live_children(ctx).await.is_empty() {
        return false; // subtree not clear yet
    }
    info!("try_reap: shutdown pending and subtree clear — reaping self");
    // Tell the parent we're gone (authoritative trigger for its own pending shutdown). Root has no
    // parent — that delivery just errors, which is fine.
    let exited = Message {
        text: MessageBody::new("[exited] shutdown".to_string())
            .ok()
            .unwrap_or_else(|| MessageBody::new("exited".to_string()).unwrap()),
        summary: Summary::new("[exited]".to_string()).unwrap(),
        kind: MessageKind::System(SystemMessage::ChildExited {
            reason: "shutdown".to_string(),
        }),
    };
    if let Err(e) = exo_caps::Bus::deliver(&*ctx.runtime, Addressee::Parent, exited).await {
        warn!("try_reap: ChildExited to parent failed (ok for root): {e}");
    }
    if grace_ms > 0 {
        tokio::time::sleep(Duration::from_millis(grace_ms as u64)).await;
    }
    if let Err(e) = Tmux::kill_pane(&*ctx.runtime, &ctx.own_pane).await {
        warn!("try_reap: kill_pane failed: {e}");
        return false;
    }
    true
}

/// Returns true if shutdown was requested
async fn process_inbox<H: InboundHandler>(
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
        let entry: IngestionEntry = match serde_json::from_slice(line_bytes) {
            Ok(e) => e,
            Err(e) => {
                warn!("failed to parse ingestion entry: {}", e);
                // Advance past malformed line
                *offset += line_len + 1;
                if let Err(e) = save_cursor(cursor_path, *offset) {
                    warn!("failed to persist cursor (will retry next wake): {e}");
                }
                continue;
            }
        };

        match handler.handle(&entry).await {
            Ok(Some(true)) => {
                // Shutdown
                *offset += line_len + 1;
                if let Err(e) = save_cursor(cursor_path, *offset) {
                    warn!("failed to persist cursor (will retry next wake): {e}");
                }
                return Ok(true);
            }
            Ok(_) => {
                // Success (or no-op), advance cursor
                *offset += line_len + 1;
                if let Err(e) = save_cursor(cursor_path, *offset) {
                    warn!("failed to persist cursor (will retry next wake): {e}");
                }
            }
            Err(e) => {
                error!("failed to route entry: {}. will retry on next wake", e);
                // DO NOT advance cursor. Break batch to retry later.
                return Ok(false);
            }
        }
    }

    Ok(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use exo_caps::{AgentName, Message, MessageBody, Persona, Summary};
    use std::fs::OpenOptions;
    use std::sync::Mutex;

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
            msg: Message {
                text: MessageBody::new(text.to_string()).unwrap(),
                summary: Summary::new("test".to_string()).unwrap(),
                kind: MessageKind::Chat,
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

        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
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
            msg: Message {
                text: MessageBody::new("partial".to_string()).unwrap(),
                summary: Summary::new("test".to_string()).unwrap(),
                kind: MessageKind::Chat,
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

        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
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
        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
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
        process_inbox(&handler2, &inbox_path, &cursor_path, &mut offset)
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

        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
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

        process_inbox(&handler, &inbox_path, &cursor_path, &mut offset)
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
        process_inbox(&handler2, &inbox_path, &cursor_path, &mut new_offset)
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
