//! **N6 — outbound Teams bridge (Claude nodes only).**
//!
//! [`dispatch`](crate::dispatch) makes a bus message *arrive* as a native `<teammate-message>` by
//! writing this node's team **lead** inbox. This is the reverse direction: when the agent itself
//! *sends* to a teammate — native `SendMessage`, or a `shutdown_request` — Claude Code appends to
//! `~/.claude/teams/{team}/inboxes/{recipient}.json`. We watch those **member** inboxes (all except
//! our own lead inbox), classify each new message, map the recipient name to a tree-edge
//! [`Addressee`](exo_caps::Addressee), and forward it onto the exomonad bus. Native Teams thus
//! becomes a real cross-node channel for Claude nodes, with **no new tools** — the agent just uses
//! the team tools it already has.
//!
//! No roster is authored: a child becomes a known teammate the moment it `notify_parent`s/idles
//! (the inbound path), so the parent can address it back. Idempotency uses a **sidecar-owned**
//! processed-count cursor ([`paths::team_cursor_path`](exo_caps::paths::team_cursor_path)); we only
//! ever *read* CC's inboxes (CC is their concurrent writer). Claude-only + Linux-only (the team is
//! resolved via [`exo_scry::resolve_self`], which walks to the parent `claude` process).

use std::sync::Arc;

use exo_caps::{AgentType, RoleKind};
use exo_framework::Exomonad;

use crate::bootstrap::NodeContext;
use crate::error::NodeResult;

/// Grace applied to a Teams `shutdown_request` bridged onto the bus as `Control(Shutdown)`. The
/// native request carries no grace; this mirrors the `send_message` tool's shutdown default.
#[cfg(target_os = "linux")]
const SHUTDOWN_GRACE_MS: u32 = 5000;

/// Watch this node's own team inboxes and forward the agent's outbound teammate messages onto the
/// bus. A no-op for non-Claude nodes (no CC team) and on non-Linux (no `resolve_self`).
pub async fn watch<D: Exomonad>(ctx: Arc<NodeContext<D>>) -> NodeResult<()> {
    if ctx.kind.agent_type() != AgentType::Claude {
        return Ok(());
    }
    #[cfg(target_os = "linux")]
    {
        linux::run(ctx).await
    }
    #[cfg(not(target_os = "linux"))]
    {
        let _ = ctx;
        Ok(())
    }
}

#[cfg(target_os = "linux")]
mod linux {
    use super::SHUTDOWN_GRACE_MS;
    use crate::bootstrap::NodeContext;
    use crate::error::{NodeError, NodeResult};
    use exo_caps::{AgentName, Bus, ControlKind, Message, MessageBody, MessageKind, Spawner, Summary};
    use exo_framework::Exomonad;
    use exo_scry::inbox::InboxMessage;
    use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
    use serde_json::Value;
    use std::collections::HashMap;
    use std::path::{Path, PathBuf};
    use std::sync::Arc;
    use std::time::Duration;
    use tokio::sync::mpsc;
    use tracing::{debug, info, warn};

    /// How the outbound bridge handles one teammate message.
    #[derive(Debug)]
    enum Action {
        /// Plain message → bus `Chat`.
        Chat,
        /// `{type:"shutdown_request"}` → bus `Control(Shutdown)`.
        Shutdown,
        /// A structured message we deliberately don't bridge (e.g. `task_assignment`).
        Skip(String),
    }

    fn classify(text: &str) -> Action {
        match serde_json::from_str::<Value>(text.trim()) {
            Ok(v) => match v.get("type").and_then(|t| t.as_str()) {
                Some("shutdown_request") => Action::Shutdown,
                // JSON without a `type` is treated as a plain (if odd) chat body.
                None => Action::Chat,
                Some(other) => Action::Skip(other.to_string()),
            },
            // Not JSON → an ordinary text message.
            Err(_) => Action::Chat,
        }
    }

    #[tracing::instrument(skip(ctx), fields(node = %ctx.runtime.name().as_str()))]
    pub async fn run<D: Exomonad>(ctx: Arc<NodeContext<D>>) -> NodeResult<()> {
        let node = ctx.runtime.name();
        let Some((team, lead)) = wait_for_team(node.as_str()).await else {
            return Ok(()); // no team ever appeared — native outbound stays off, MCP tools still work
        };

        let inboxes_dir = exo_scry::teams::teams_root()
            .map_err(|e| NodeError::Scry(e.to_string()))?
            .join(&team)
            .join("inboxes");
        // `notify` cannot watch a missing dir; the agent will write into it on first SendMessage.
        std::fs::create_dir_all(&inboxes_dir)?;
        info!(team = %team, dir = %inboxes_dir.display(), "teamout: watching team inboxes");

        let home = std::env::var("HOME").map_err(|_| NodeError::MissingContext("HOME"))?;
        let cursor_path =
            exo_caps::paths::team_cursor_path(Path::new(&home), &ctx.run_id, &ctx.own_pane);
        let mut cursor = load_cursor(&cursor_path);

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
        watcher
            .watch(&inboxes_dir, RecursiveMode::NonRecursive)
            .map_err(std::io::Error::other)?;

        // Initial pass for anything already present (respecting the persisted cursor).
        info!(node = %ctx.runtime.name().as_str(), team = %team, "teamout: starting initial bridge reconciliation");
        reconcile(
            &ctx,
            &team,
            lead.as_deref(),
            &inboxes_dir,
            &mut cursor,
            &cursor_path,
        )
        .await;

        while let Some(()) = rx.recv().await {
            while rx.try_recv().is_ok() {} // drain coalesced events
            debug!(node = %ctx.runtime.name().as_str(), "teamout: notify wake; reconciling inboxes");
            reconcile(
                &ctx,
                &team,
                lead.as_deref(),
                &inboxes_dir,
                &mut cursor,
                &cursor_path,
            )
            .await;
        }
        Ok(())
    }

    /// Poll `resolve_self` until our team exists (the agent runs `TeamCreate` shortly after boot).
    /// Returns `(team_name, lead_member_name)`. Gives up after ~10 min so a node that never makes a
    /// team doesn't spin forever.
    async fn wait_for_team(node: &str) -> Option<(String, Option<String>)> {
        for attempt in 0..200u32 {
            match exo_scry::resolve_self() {
                Ok(Some(t)) => {
                    info!(
                        node = %node,
                        team = %t.team.0,
                        "teamout: resolved own team '{}' (attempt {attempt})",
                        t.team.0
                    );
                    return Some((t.team.0, t.lead_inbox));
                }
                Ok(None) => {}
                Err(e) if attempt == 0 => {
                    warn!(node = %node, "teamout: resolve_self error (will retry): {e}")
                }
                Err(_) => {}
            }
            tokio::time::sleep(Duration::from_secs(3)).await;
        }
        warn!(
            node = %node,
            "teamout: no team resolved after waiting; native Teams outbound disabled for this node"
        );
        None
    }

    /// Read every member inbox (except our own lead inbox), forward entries past the cursor.
    async fn reconcile<D: Exomonad>(
        ctx: &Arc<NodeContext<D>>,
        team: &str,
        lead: Option<&str>,
        inboxes_dir: &Path,
        cursor: &mut HashMap<String, usize>,
        cursor_path: &Path,
    ) {
        let entries = match std::fs::read_dir(inboxes_dir) {
            Ok(e) => e,
            Err(e) => {
                warn!(node = %ctx.runtime.name().as_str(), "teamout: read_dir {} failed: {e}", inboxes_dir.display());
                return;
            }
        };
        let mut dirty = false;
        for ent in entries.flatten() {
            let path = ent.path();
            if path.extension().and_then(|s| s.to_str()) != Some("json") {
                continue; // skip .lock / .tmp / non-inbox files
            }
            let Some(member) = path
                .file_stem()
                .and_then(|s| s.to_str())
                .map(str::to_string)
            else {
                continue;
            };
            if Some(member.as_str()) == lead {
                continue; // our own (inbound) inbox — written by dispatch, read by the agent
            }
            let msgs = match exo_scry::inbox::read_inbox(team, &member) {
                Ok(m) => m,
                Err(e) => {
                    warn!(node = %ctx.runtime.name().as_str(), member = %member, "teamout: read_inbox failed: {e}");
                    continue;
                }
            };
            let start = cursor.get(&member).copied().unwrap_or(0);
            if msgs.len() <= start {
                continue;
            }
            debug!(node = %ctx.runtime.name().as_str(), member = %member, count = msgs.len() - start, "teamout: forwarding new messages from inbox");
            for msg in &msgs[start..] {
                forward(ctx, &member, msg).await;
            }
            cursor.insert(member, msgs.len());
            dirty = true;
        }
        if dirty {
            save_cursor(cursor_path, cursor);
        }
    }

    /// Bridge one teammate message onto the bus. Always advances the cursor (caller side) — a
    /// message we can't deliver is dropped with a warning rather than retried forever.
    #[tracing::instrument(skip(ctx, msg), fields(node = %ctx.runtime.name().as_str(), to = %member))]
    async fn forward<D: Exomonad>(ctx: &Arc<NodeContext<D>>, member: &str, msg: &InboxMessage) {
        let Ok(name) = AgentName::new(member.to_string()) else {
            warn!(
                outcome = "dropped_invalid_name",
                "teamout: invalid teammate name; dropping"
            );
            return;
        };
        let Some(addressee) = ctx.runtime.resolve_edge(&name).await else {
            warn!(
                outcome = "dropped_not_edge",
                "teamout: not a tree edge (child/parent); dropping"
            );
            return;
        };

        let action = classify(&msg.text);

        // Inline-child shutdown: the worker has no team and can't self-reap cooperatively.
        // The parent kills the pane directly instead of forwarding a Control(Shutdown) onto
        // the bus (which would never be received — the inline node has no teamout bridge).
        if matches!(action, Action::Shutdown) {
            if let exo_caps::Addressee::InlineChild(ref child_name) = addressee {
                match Spawner::kill_pane(&*ctx.runtime, child_name).await {
                    Ok(_) => info!(
                        outcome = "killed_pane",
                        to = %member,
                        "teamout: inline child shutdown → parent kill_pane"
                    ),
                    Err(e) => warn!(
                        outcome = "error_kill_pane",
                        to = %member,
                        "teamout: kill_pane for inline child failed: {e}"
                    ),
                }
                return;
            }
        }

        let (text, summary, kind) = match action {
            Action::Chat => (
                msg.text.clone(),
                make_summary(msg, "(teams message)"),
                MessageKind::Chat,
            ),
            Action::Shutdown => (
                "shutdown requested via teams".to_string(),
                make_summary(msg, "[shutdown]"),
                // Native CC shutdown_request has no force field → always cooperative.
                MessageKind::Control(ControlKind::Shutdown {
                    grace_ms: SHUTDOWN_GRACE_MS,
                    force: false,
                }),
            ),
            Action::Skip(t) => {
                warn!(outcome = "skipped", kind = %t, "teamout: unhandled teams message type; dropping");
                return;
            }
        };

        let body = match MessageBody::new(text) {
            Ok(b) => b,
            Err(e) => {
                warn!(
                    outcome = "error_body",
                    "teamout: message body invalid ({e}); dropping"
                );
                return;
            }
        };
        let message = Message {
            text: body,
            summary,
            kind,
        };
        match Bus::deliver(&*ctx.runtime, addressee, message).await {
            Ok(_) => {
                info!(outcome = "forwarded", action = ?classify(&msg.text), "teamout: message bridged to bus")
            }
            Err(e) => warn!(outcome = "error_bus", "teamout: bus deliver failed: {e}"),
        }
    }

    /// A valid single-line [`Summary`] from the message (its own summary, else its text), falling
    /// back to a constant so this never fails.
    fn make_summary(msg: &InboxMessage, fallback: &str) -> Summary {
        let raw = if msg.summary.trim().is_empty() {
            msg.text.as_str()
        } else {
            msg.summary.as_str()
        };
        let oneline: String = raw
            .lines()
            .next()
            .unwrap_or(fallback)
            .chars()
            .take(200)
            .collect();
        let pick = if oneline.trim().is_empty() {
            fallback.to_string()
        } else {
            oneline
        };
        Summary::new(pick).unwrap_or_else(|_| Summary::new(fallback.to_string()).unwrap())
    }

    fn load_cursor(path: &Path) -> HashMap<String, usize> {
        std::fs::read(path)
            .ok()
            .and_then(|b| serde_json::from_slice(&b).ok())
            .unwrap_or_default()
    }

    fn save_cursor(path: &Path, cursor: &HashMap<String, usize>) {
        // Best-effort, but NOT silent: a failed cursor write means we re-forward already-bridged
        // outbound messages on the next run (silent duplicates). Log each step's failure so that's
        // diagnosable, not a mystery.
        if let Some(parent) = path.parent() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                warn!("teamout cursor: mkdir {} failed: {e}", parent.display());
            }
        }
        let bytes = match serde_json::to_vec(cursor) {
            Ok(b) => b,
            Err(e) => {
                warn!("teamout cursor: serialize failed: {e}");
                return;
            }
        };
        let tmp = PathBuf::from(format!("{}.tmp", path.display()));
        if let Err(e) = std::fs::write(&tmp, &bytes) {
            warn!("teamout cursor: write {} failed: {e}", tmp.display());
            return;
        }
        if let Err(e) = std::fs::rename(&tmp, path) {
            warn!(
                "teamout cursor: rename {} -> {} failed: {e}",
                tmp.display(),
                path.display()
            );
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn classify_plain_text_is_chat() {
            assert!(matches!(classify("hello there"), Action::Chat));
        }

        #[test]
        fn classify_shutdown_request() {
            let t = r#"{"type":"shutdown_request","requestId":"x","from":"team-lead"}"#;
            assert!(matches!(classify(t), Action::Shutdown));
        }

        #[test]
        fn classify_other_structured_is_skipped() {
            let t = r#"{"type":"task_assignment","taskId":"1"}"#;
            assert!(matches!(classify(t), Action::Skip(s) if s == "task_assignment"));
        }

        #[test]
        fn classify_json_without_type_is_chat() {
            assert!(matches!(classify(r#"{"note":"hi"}"#), Action::Chat));
        }
    }
}
