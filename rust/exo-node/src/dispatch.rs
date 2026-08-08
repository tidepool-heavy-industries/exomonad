//! **N2a — Last-hop dispatch.** Delivers an ingestion entry into this node's own agent by
//! tmux-pasting it into the agent's pane (buffer pattern), rendered with a `[from: X, kind: Y]`
//! header. This is the single last hop for every node kind (Claude, Shoal companion, inline worker).
//!
//! CC Agent Teams native delivery was removed: as of Claude Code 2.1.178 a solo session-lead never
//! drains its teammate inbox, so writing `~/.claude/teams/<team>/inboxes/team-lead.json` silently
//! stranded every message (GH#26426). exo owns its delivery channel — the durable bus carries the
//! message sidecar→sidecar, and this module injects it into the live `claude` via tmux.
//!
//! This is pure last-hop dispatch; it focuses on the agent-facing write for entries
//! that have already been routed to this node.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use chrono::Utc;
use exo_caps::{
    Fs, IngestionEntry, Message, MessageBody, MessageKind, Persona, Summary, SyntheticName, Tmux,
};
use exo_framework::Exomonad;
use tracing::{error, info, warn};

use crate::bootstrap::NodeContext;
use crate::error::{NodeError, NodeResult};

static SPILL_COUNTER: AtomicU64 = AtomicU64::new(0);
const MAX_INLINE_PASTE_BYTES: usize = 480;

/// Deliver one ingestion entry into this node's own agent (the runtime-specific last hop).
#[tracing::instrument(skip(ctx, entry), fields(node = %ctx.runtime.name().as_str(), from = %persona_label(&entry.from), kind = %kind_label(&entry.msg.kind)))]
pub async fn dispatch<D: Exomonad>(
    ctx: &Arc<NodeContext<D>>,
    entry: &IngestionEntry,
) -> NodeResult<()> {
    info!(outcome = "tmux_paste", "dispatching via tmux paste");
    let rendered = prepare_tmux_payload(ctx, entry).await;
    match Tmux::paste(&*ctx.runtime, &ctx.own_pane, &rendered).await {
        // `outcome = "tmux_paste"` above already records the attempt; don't double-log OK.
        Ok(()) => Ok(()),
        Err(e) => {
            error!("FAILED to dispatch via tmux paste: {e}");
            Err(NodeError::Delivery(format!("Tmux paste failed: {}", e)))
        }
    }
}

/// Inject a synthetic (sidecar-authored) message into THIS node's own LLM via the last-hop
/// dispatch — the shared path for engine-internal renders (shutdown prompts) and the domain's
/// `SystemCtx::deliver_to_self`. Attributed to a synthetic `from`.
pub(crate) async fn deliver_synthetic<D: Exomonad>(
    ctx: &Arc<NodeContext<D>>,
    from: &str,
    summary: &str,
    text: &str,
) -> NodeResult<()> {
    let entry = IngestionEntry {
        v: 1,
        ts: Utc::now(),
        id: None,
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
    dispatch(ctx, &entry).await
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

fn render_entry(entry: &IngestionEntry) -> String {
    format!(
        "[from: {}, kind: {}]\n\n{}\n{}",
        render_persona(&entry.from),
        kind_label(&entry.msg.kind),
        entry.msg.summary.as_str(),
        entry.msg.text.as_str()
    )
}

/// What to actually push over a tmux paste for one entry.
#[derive(Debug, PartialEq, Eq)]
enum PastePlan {
    /// Safe to paste directly: a single line, no shell-mode trigger.
    Inline(String),
    /// Body must be spilled to a file; this is the full text to write.
    Spill { file_body: String },
}

/// Decide inline-vs-spill. A message is safe to paste inline only if it is a single short line
/// with no body. Everything else (any text body, multi-line, or oversized) spills to a file so we
/// can deliver a one-line `@`-reference instead.
fn plan_paste(persona: &str, summary: &str, text: &str, full_render: &str) -> PastePlan {
    let oneline = format!("[from {}] {}", persona, summary);
    let can_inline = text.trim().is_empty()
        && !oneline.contains('\n')
        && oneline.len() <= MAX_INLINE_PASTE_BYTES;
    if can_inline {
        PastePlan::Inline(oneline)
    } else {
        PastePlan::Spill {
            file_body: full_render.to_string(),
        }
    }
}

/// Build the single-line `@`-reference paste that points at the spilled file. MUST be one line.
fn render_atref(persona: &str, summary: &str, rel_path: &str) -> String {
    let snippet: String = summary.chars().filter(|c| *c != '\n').take(80).collect();
    format!(
        "New message from {} — read @{} and act on it. ({})",
        persona, rel_path, snippet
    )
}

async fn prepare_tmux_payload<D: Exomonad>(
    ctx: &Arc<NodeContext<D>>,
    entry: &IngestionEntry,
) -> String {
    let full_render = render_entry(entry);
    let persona = render_persona(&entry.from);
    let summary = entry.msg.summary.as_str();
    let text = entry.msg.text.as_str();

    match plan_paste(&persona, summary, text, &full_render) {
        PastePlan::Inline(s) => s,
        PastePlan::Spill { file_body } => {
            let id = SPILL_COUNTER.fetch_add(1, Ordering::Relaxed);
            let rel = format!(".exo/tmp/inbox-{}-{}.md", std::process::id(), id);
            let path = ctx.runtime.working_dir().join(&rel);
            match Fs::write_atomic(&*ctx.runtime, &path, file_body.as_bytes()).await {
                Ok(()) => render_atref(&persona, summary, &rel),
                Err(e) => {
                    warn!(node = %ctx.runtime.name().as_str(), "failed to spill large paste to {rel}, pasting inline (degraded): {e}");
                    file_body
                }
            }
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
    fn test_plan_paste_inline() {
        let plan = plan_paste("alice", "hello", "", "full");
        if let PastePlan::Inline(s) = plan {
            assert_eq!(s, "[from alice] hello");
            assert!(!s.contains('\n'));
        } else {
            panic!("expected inline");
        }
    }

    #[test]
    fn test_plan_paste_spill_on_body() {
        let plan = plan_paste("alice", "hello", "body", "full");
        assert_eq!(
            plan,
            PastePlan::Spill {
                file_body: "full".to_string()
            }
        );
    }

    #[test]
    fn test_plan_paste_oversized() {
        let long_summary = "a".repeat(MAX_INLINE_PASTE_BYTES + 1);
        let plan = plan_paste("alice", &long_summary, "", "full");
        assert_eq!(
            plan,
            PastePlan::Spill {
                file_body: "full".to_string()
            }
        );
    }

    #[test]
    fn test_render_atref() {
        let atref = render_atref("alice", "Greeting\nNext line", ".exo/tmp/file.md");
        assert!(atref.contains(".exo/tmp/file.md"));
        assert!(!atref.contains('\n'));
        assert!(atref.contains("Greeting"));
    }
}
