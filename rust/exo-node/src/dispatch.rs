//! **N2a — Last-hop dispatch.** Delivers an ingestion entry into this node's own agent over the
//! **listen wake channel**: the entry is rendered with a `[from: X, kind: Y]` header and handed
//! to the attached `exo listen` Monitor client ([`crate::listen`]), whose stdout becomes a
//! harness notification that wakes the agent. This is the single last hop for every node kind.
//!
//! **No listener ⇒ the entry queues.** Dispatch errs, the inbound cursor stays pinned, and the
//! bus retries until a client attaches and acks — so messages sent before the agent arms (or
//! re-arms) its Monitor are delivered late, never dropped. There is no tmux-paste delivery:
//! pasting into the pane typed over the human at the root and was indistinguishable from user
//! input (tmux survives for spawning and observability only). CC Agent Teams native delivery
//! was removed even earlier (a solo session-lead never drains its teammate inbox as of CC
//! 2.1.178, GH#26426) — exo owns its delivery channel end to end.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use chrono::Utc;
use exo_caps::{
    Fs, IngestionEntry, Message, MessageBody, MessageKind, Persona, Summary, SyntheticName,
};
use exo_framework::Exomonad;
use tracing::{debug, info, warn};

use crate::bootstrap::NodeContext;
use crate::error::{NodeError, NodeResult};
use crate::listen::ListenDeliverError;

static SPILL_COUNTER: AtomicU64 = AtomicU64::new(0);

/// A full render at most this big (and [`MAX_INLINE_LISTEN_LINES`] lines) goes over the wake
/// channel inline; anything larger becomes a one-line `@`-ref to a spill file. The binding
/// constraint is notification volume, not bytes — the harness batches one buffered write into
/// one notification, but a monitor that floods lines gets auto-stopped, and an oversized body
/// bloats the recipient's context where an `@`-ref lets it choose. Bodies cap at 4 KiB
/// (`MessageBody`), so inline covers the common case (`[READY]`, status notes) outright.
const MAX_INLINE_LISTEN_BYTES: usize = 2048;
const MAX_INLINE_LISTEN_LINES: usize = 12;

/// Deliver one ingestion entry into this node's own agent (the runtime-specific last hop).
///
/// `Ok` means the attached `exo listen` client flushed the payload to its stdout (acked) — the
/// caller may advance the inbound cursor. `Err` leaves the cursor pinned: with no listener the
/// entry is *queued, not failed* (the expected state before the agent's first-action Monitor
/// arm), and the listen server pings the inbound wake on attach so the backlog drains at once.
#[tracing::instrument(skip(ctx, entry), fields(node = %ctx.runtime.name().as_str(), from = %persona_label(&entry.from), kind = %kind_label(&entry.msg.kind)))]
pub async fn dispatch<D: Exomonad>(
    ctx: &Arc<NodeContext<D>>,
    entry: &IngestionEntry,
) -> NodeResult<()> {
    let payload = prepare_listen_payload(ctx, entry).await;
    match ctx.listener.try_deliver(&payload).await {
        Ok(()) => {
            info!(outcome = "listen", "dispatched via listen channel");
            Ok(())
        }
        Err(ListenDeliverError::NoListener) => {
            debug!(outcome = "queued", "no listener attached; entry stays queued");
            Err(NodeError::NoListener)
        }
        Err(e) => {
            warn!("listen delivery failed ({e}); entry stays queued for retry");
            Err(NodeError::Delivery(format!("listen delivery failed: {e}")))
        }
    }
}

/// Inject a synthetic (sidecar-authored) message into THIS node's own LLM — the shared path for
/// engine-internal renders (shutdown prompts, watchdog death notes) and the domain's
/// `SystemCtx::deliver_to_self`. Attributed to a synthetic `from`.
///
/// This **appends to the node's own inbox** (same append+spill discipline as `Bus::deliver`,
/// via `Runtime::append_entry`) rather than calling [`dispatch`] directly: everything the agent
/// must see flows through one cursor-backed path, so a synthetic note that lands while no
/// listener is attached queues and replays like any bus message instead of being warn-and-dropped.
pub(crate) async fn deliver_synthetic<D: Exomonad>(
    ctx: &Arc<NodeContext<D>>,
    from: &str,
    summary: &str,
    text: &str,
) -> NodeResult<()> {
    let entry = IngestionEntry {
        v: 1,
        ts: Utc::now(),
        id: Some(uuid::Uuid::new_v4().to_string()),
        spill: None,
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
            reply_to: None,
        },
    };
    ctx.runtime
        .append_entry(&ctx.own_inbox, entry)
        .await
        .map_err(|e| NodeError::Delivery(format!("self-append failed: {e}")))?;
    // The inbox filesystem event would wake the inbound loop anyway; the explicit ping makes
    // delivery prompt rather than watcher-latency-bound.
    ctx.inbox_wake.notify_one();
    Ok(())
}

/// Short, log-friendly label for a persona: the bare agent/synthetic name, no `Agent(AgentName(..))`
/// Debug wrapping. Borrows from the persona, so it's free to record into a tracing span field.
pub(crate) fn persona_label(persona: &Persona) -> &str {
    match persona {
        Persona::Agent(name) => name.as_str(),
        Persona::Synthetic(name) => name.as_str(),
    }
}

/// Short, log-friendly discriminant for a message kind — NEVER includes the payload. Recording a
/// `MessageKind` via Debug would splat the whole `Domain(DomainPayload("..."))` blob (often a
/// multi-KB findings JSON) into every nested span line; this is the one-word stand-in.
pub(crate) fn kind_label(kind: &MessageKind) -> &'static str {
    match kind {
        MessageKind::Chat => "chat",
        MessageKind::Event => "event",
        MessageKind::Control(_) => "control",
        MessageKind::Lifecycle(_) => "lifecycle",
        MessageKind::Domain(_) => "domain",
    }
}

fn render_persona(persona: &Persona) -> String {
    persona_label(persona).to_string()
}

/// Render the last-hop header. `id` is omitted entirely (not a placeholder) when the entry
/// carries none — a pre-`id`-field or externally-written line has nothing to name, and a header
/// with no `id:` segment is itself the signal, not a sentinel string a reader might mistake for a
/// real id. `re:` is appended only when the entry answers another message.
fn render_entry(entry: &IngestionEntry) -> String {
    let id_segment = match &entry.id {
        Some(id) => format!(", id: {id}"),
        None => String::new(),
    };
    let reply_segment = match &entry.msg.reply_to {
        Some(reply_to) => format!(", re: {reply_to}"),
        None => String::new(),
    };
    format!(
        "[from: {}, kind: {}{}{}]\n\n{}\n{}",
        render_persona(&entry.from),
        kind_label(&entry.msg.kind),
        id_segment,
        reply_segment,
        entry.msg.summary.as_str(),
        entry.msg.text.as_str()
    )
}

/// Does this full render go over the wake channel inline, or as an `@`-ref to a spill file?
fn listen_inline(full_render: &str) -> bool {
    full_render.len() <= MAX_INLINE_LISTEN_BYTES
        && full_render.lines().count() <= MAX_INLINE_LISTEN_LINES
}

/// Build the single-line `@`-reference that points at the spilled file. MUST be one line.
fn render_atref(persona: &str, summary: &str, rel_path: &str) -> String {
    let snippet: String = summary.chars().filter(|c| *c != '\n').take(80).collect();
    format!(
        "New message from {} — read @{} and act on it. ({})",
        persona, rel_path, snippet
    )
}

/// Render an entry for the wake channel: the full `[from, kind, id]` render when small enough
/// to sit in the agent's context outright, otherwise spilled to `.exo/tmp/` (GC'd by
/// `exo doctor` once this pid is gone) behind a one-line `@`-ref. A spill-write failure
/// degrades to sending the full render inline — delivery beats tidiness.
async fn prepare_listen_payload<D: Exomonad>(
    ctx: &Arc<NodeContext<D>>,
    entry: &IngestionEntry,
) -> String {
    let full_render = render_entry(entry);
    if listen_inline(&full_render) {
        return full_render;
    }

    let persona = render_persona(&entry.from);
    let summary = entry.msg.summary.as_str();
    let id = SPILL_COUNTER.fetch_add(1, Ordering::Relaxed);
    let rel = format!(".exo/tmp/inbox-{}-{}.md", std::process::id(), id);
    let path = ctx.runtime.working_dir().join(&rel);
    match Fs::write_atomic(&*ctx.runtime, &path, full_render.as_bytes()).await {
        Ok(()) => render_atref(&persona, summary, &rel),
        Err(e) => {
            warn!(node = %ctx.runtime.name().as_str(), "failed to spill large payload to {rel}, sending inline (degraded): {e}");
            full_render
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use exo_caps::{AgentName, Message, MessageBody, Summary};

    #[test]
    fn test_render_entry() {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(AgentName::new("alice".to_string()).unwrap()),
            id: None,
            spill: None,
            msg: Message {
                text: MessageBody::new("Hello world".to_string()).unwrap(),
                summary: Summary::new("Greeting".to_string()).unwrap(),
                kind: MessageKind::Chat,
                reply_to: None,
            },
        };

        let rendered = render_entry(&entry);
        assert!(rendered.contains("[from: alice, kind: chat]"));
        assert!(rendered.contains("\n\nGreeting\nHello world"));
    }

    #[test]
    fn test_render_entry_with_id() {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(AgentName::new("alice".to_string()).unwrap()),
            id: Some("11111111-2222-3333-4444-555555555555".to_string()),
            spill: None,
            msg: Message {
                text: MessageBody::new("Hello world".to_string()).unwrap(),
                summary: Summary::new("Greeting".to_string()).unwrap(),
                kind: MessageKind::Chat,
                reply_to: None,
            },
        };

        let rendered = render_entry(&entry);
        assert!(rendered
            .contains("[from: alice, kind: chat, id: 11111111-2222-3333-4444-555555555555]"));
        assert!(!rendered.contains(", re:"));
    }

    #[test]
    fn test_render_entry_with_reply_to() {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(AgentName::new("alice".to_string()).unwrap()),
            id: Some("aaaa".to_string()),
            spill: None,
            msg: Message {
                text: MessageBody::new("Hello world".to_string()).unwrap(),
                summary: Summary::new("Greeting".to_string()).unwrap(),
                kind: MessageKind::Chat,
                reply_to: Some("bbbb".to_string()),
            },
        };

        let rendered = render_entry(&entry);
        assert!(rendered.contains("[from: alice, kind: chat, id: aaaa, re: bbbb]"));
    }

    #[test]
    fn test_render_entry_no_id_omits_segment() {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Agent(AgentName::new("alice".to_string()).unwrap()),
            id: None,
            spill: None,
            msg: Message {
                text: MessageBody::new("Hello world".to_string()).unwrap(),
                summary: Summary::new("Greeting".to_string()).unwrap(),
                kind: MessageKind::Chat,
                reply_to: None,
            },
        };

        let rendered = render_entry(&entry);
        assert!(rendered.contains("[from: alice, kind: chat]"));
        assert!(!rendered.contains("id:"));
    }

    #[test]
    fn test_render_entry_event() {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Synthetic(exo_caps::SyntheticName::new("github".to_string()).unwrap()),
            id: None,
            spill: None,
            msg: Message {
                text: MessageBody::new("PR #1 Approved".to_string()).unwrap(),
                summary: Summary::new("[PR READY]".to_string()).unwrap(),
                kind: MessageKind::Event,
                reply_to: None,
            },
        };

        let rendered = render_entry(&entry);
        assert!(rendered.contains("[from: github, kind: event]"));
        assert!(rendered.contains("\n\n[PR READY]\nPR #1 Approved"));
    }

    #[test]
    fn test_listen_inline_small_render() {
        // The common case — header + summary + short body — goes inline.
        assert!(listen_inline(
            "[from: alice, kind: chat]\n\n[READY]\nbranch root.dev-0 at abc123"
        ));
    }

    #[test]
    fn test_listen_inline_byte_edge() {
        let at_cap = "a".repeat(MAX_INLINE_LISTEN_BYTES);
        assert!(listen_inline(&at_cap));
        let over_cap = "a".repeat(MAX_INLINE_LISTEN_BYTES + 1);
        assert!(!listen_inline(&over_cap));
    }

    #[test]
    fn test_listen_inline_line_edge() {
        let at_cap = vec!["x"; MAX_INLINE_LISTEN_LINES].join("\n");
        assert!(listen_inline(&at_cap));
        let over_cap = vec!["x"; MAX_INLINE_LISTEN_LINES + 1].join("\n");
        assert!(!listen_inline(&over_cap));
    }

    #[test]
    fn test_render_atref() {
        let atref = render_atref("alice", "Greeting\nNext line", ".exo/tmp/file.md");
        assert!(atref.contains(".exo/tmp/file.md"));
        assert!(!atref.contains('\n'));
        assert!(atref.contains("Greeting"));
    }
}
