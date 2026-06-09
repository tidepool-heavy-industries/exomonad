//! **N2a — Last-hop dispatch.** Routes ingestion entries into the agent based on its
//! `agent_type` and CC team membership. This module handles the final delivery of messages
//! to the agent's native interface.
//!
//! Delivery mechanisms:
//! - **Claude Code in a team**: Writes to the CC Teams inbox, which is then picked up
//!   by the InboxPoller and delivered as a `<teammate-message>`.
//! - **Claude Code (no team) or Gemini**: Uses tmux injection (buffer pattern) to paste
//!   the message directly into the agent's pane, rendered with a `[from: X, kind: Y]` header.
//!
//! This is pure last-hop dispatch; it focuses on the agent-facing write for entries
//! that have already been routed to this node.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use chrono::Utc;
use exo_caps::{
    AgentType, Fs, IngestionEntry, Message, MessageBody, MessageKind, Persona, RoleKind, Summary,
    SyntheticName, Tmux,
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
    let agent_type = ctx.kind.agent_type();

    // Resolve THIS agent's own team — but ONLY for a Claude node, since `decide_lasthop` consults
    // the team only for Claude (a Gemini leaf always tmux-pastes). Resolving for Gemini would walk
    // `/proc` looking for a `claude` ancestor it never has, then discard the `None` — and log a
    // spurious WARN on every single delivery. So skip it entirely for non-Claude.
    //
    // `resolve_self_or_portable` tries `resolve_self` first — it walks from the sidecar up to its
    // parent `claude` process and reads that process's inotify-bound `tasks/{team}` dir, finding
    // the agent's own (solo) team without needing a `tmux_pane_id` (which CC never writes into its
    // team config; that's why `resolve_by_pane` always missed and native delivery never fired). On
    // its failure (no team, or a transient `/proc`/config race) it falls back to the portable
    // cwd→transcript path before giving up. On non-Linux the portable cwd reader is unavailable, so
    // this yields `None` (wired but untested off-Linux). For a Claude node, resolution failure is
    // non-fatal but noteworthy: fall back to paste, and WARN so a Claude node silently degrading to
    // paste is visible, not a mystery.
    let active_team = if agent_type == AgentType::Claude {
        match exo_scry::resolve_self_or_portable() {
            Ok(team) => team,
            Err(e) => {
                warn!(node = %ctx.runtime.name().as_str(), "team resolution failed; falling back to tmux paste for this delivery: {e}");
                None
            }
        }
    } else {
        None
    };

    let lasthop = decide_lasthop(agent_type, active_team);
    match lasthop {
        LastHop::TeamsInbox { team, to } => {
            info!(outcome = "teams_inbox", team = %team, to = %to, "dispatching via Teams inbox");
            let persona_str = render_persona(&entry.from);
            match exo_scry::inbox::send_message(
                &team,
                &to,
                &persona_str,
                entry.msg.text.as_str(),
                entry.msg.summary.as_str(),
            ) {
                // The `outcome = "teams_inbox"` line above already records the attempt; on success
                // there's nothing to add, so don't double-log. Only the failure path is noteworthy.
                Ok(_) => Ok(()),
                Err(e) => {
                    error!("FAILED to dispatch via Teams inbox: {e}");
                    Err(NodeError::Scry(e.to_string()))
                }
            }
        }
        LastHop::TmuxPaste => {
            info!(outcome = "tmux_paste", "dispatching via tmux paste");
            let rendered = prepare_tmux_payload(ctx, agent_type, entry).await;
            match Tmux::paste(&*ctx.runtime, &ctx.own_pane, &rendered).await {
                // `outcome = "tmux_paste"` above already records the attempt; don't double-log OK.
                Ok(()) => Ok(()),
                Err(e) => {
                    error!("FAILED to dispatch via tmux paste: {e}");
                    Err(NodeError::Scry(format!("Tmux paste failed: {}", e)))
                }
            }
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
    dispatch(ctx, &entry).await
}

#[derive(Debug, PartialEq, Eq)]
enum LastHop {
    TeamsInbox { team: String, to: String },
    TmuxPaste,
}

fn decide_lasthop(agent_type: AgentType, active_team: Option<exo_scry::ActiveTeam>) -> LastHop {
    // A node delivers into its OWN agent's conversation. For a Claude node that leads a team
    // (solo-team-per-session — `resolve_self` resolves the team the agent is bound to, where it
    // IS the lead), write that team's lead inbox: the agent's own CC InboxPoller renders it as
    // a native `<teammate-message>`. With no team (a Gemini leaf, or before `TeamCreate`), the
    // sidecar tmux-pastes instead. Messaging is tree-edges only (the Bus); nothing reads
    // another node's inbox — the lead inbox here is the agent's *own*.
    if agent_type == AgentType::Claude {
        if let Some(team) = active_team {
            if let Some(to) = team.lead_inbox {
                return LastHop::TeamsInbox {
                    team: team.team.0,
                    to,
                };
            }
        }
    }
    LastHop::TmuxPaste
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
/// with no body — and, for Gemini, contains no `!` (which would flip the CLI into shell mode).
/// Everything else (any text body, multi-line, oversized, or Gemini+`!`) spills to a file so we
/// can deliver a one-line `@`-reference instead.
fn plan_paste(
    persona: &str,
    summary: &str,
    text: &str,
    full_render: &str,
    is_gemini: bool,
) -> PastePlan {
    let oneline = format!("[from {}] {}", persona, summary);
    let can_inline = text.trim().is_empty()
        && !oneline.contains('\n')
        && oneline.len() <= MAX_INLINE_PASTE_BYTES
        && !(is_gemini && oneline.contains('!'));
    if can_inline {
        PastePlan::Inline(oneline)
    } else {
        PastePlan::Spill {
            file_body: full_render.to_string(),
        }
    }
}

/// Build the single-line `@`-reference paste that points at the spilled file. MUST be one line
/// with no `!` (Gemini shell-mode trigger).
fn render_atref(persona: &str, summary: &str, rel_path: &str) -> String {
    let snippet: String = summary
        .chars()
        .filter(|c| *c != '\n' && *c != '!')
        .take(80)
        .collect();
    format!(
        "New message from {} — read @{} and act on it. ({})",
        persona, rel_path, snippet
    )
}

async fn prepare_tmux_payload<D: Exomonad>(
    ctx: &Arc<NodeContext<D>>,
    agent_type: AgentType,
    entry: &IngestionEntry,
) -> String {
    let full_render = render_entry(entry);
    let persona = render_persona(&entry.from);
    let summary = entry.msg.summary.as_str();
    let text = entry.msg.text.as_str();
    let is_gemini = agent_type == AgentType::Gemini;

    match plan_paste(&persona, summary, text, &full_render, is_gemini) {
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
            msg: Message {
                text: MessageBody::new("Hello world".to_string()).unwrap(),
                summary: Summary::new("Greeting".to_string()).unwrap(),
                kind: MessageKind::Chat,
            },
        };

        let rendered = render_entry(&entry);
        assert!(rendered.contains("[from: alice, kind: chat]"));
        assert!(rendered.contains("\n\nGreeting\nHello world"));
    }

    #[test]
    fn test_decide_lasthop_gemini() {
        let hop = decide_lasthop(AgentType::Gemini, None);
        assert_eq!(hop, LastHop::TmuxPaste);
    }

    #[test]
    fn test_decide_lasthop_claude_no_team() {
        let hop = decide_lasthop(AgentType::Claude, None);
        assert_eq!(hop, LastHop::TmuxPaste);
    }

    /// `resolve_self` resolves the agent's own team with `me: None` and `lead_inbox` set →
    /// native delivery to the agent's own (lead) inbox.
    #[test]
    fn test_decide_lasthop_claude_team_native() {
        use exo_scry::identity::TeamName;
        use std::path::PathBuf;

        let active_team = exo_scry::ActiveTeam {
            claude_pid: None,
            team: TeamName("myteam".to_string()),
            tasks_dir: PathBuf::from("/tmp"),
            lead_inbox: Some("myteam-lead".to_string()),
            lead_session_id: None,
            me: None,
        };

        let hop = decide_lasthop(AgentType::Claude, Some(active_team));
        assert_eq!(
            hop,
            LastHop::TeamsInbox {
                team: "myteam".to_string(),
                to: "myteam-lead".to_string()
            }
        );
    }

    /// A team with no lead inbox (nothing to write) → paste.
    #[test]
    fn test_decide_lasthop_claude_no_lead_inbox_pastes() {
        use exo_scry::identity::TeamName;
        use std::path::PathBuf;

        let active_team = exo_scry::ActiveTeam {
            claude_pid: None,
            team: TeamName("myteam".to_string()),
            tasks_dir: PathBuf::from("/tmp"),
            lead_inbox: None,
            lead_session_id: None,
            me: None,
        };

        let hop = decide_lasthop(AgentType::Claude, Some(active_team));
        assert_eq!(hop, LastHop::TmuxPaste);
    }

    #[test]
    fn test_render_entry_event() {
        let entry = IngestionEntry {
            v: 1,
            ts: Utc::now(),
            from: Persona::Synthetic(exo_caps::SyntheticName::new("github".to_string()).unwrap()),
            msg: Message {
                text: MessageBody::new("PR #1 Approved".to_string()).unwrap(),
                summary: Summary::new("[PR READY]".to_string()).unwrap(),
                kind: MessageKind::Event,
            },
        };

        let rendered = render_entry(&entry);
        assert!(rendered.contains("[from: github, kind: event]"));
        assert!(rendered.contains("\n\n[PR READY]\nPR #1 Approved"));
    }

    #[test]
    fn test_plan_paste_inline() {
        let plan = plan_paste("alice", "hello", "", "full", false);
        if let PastePlan::Inline(s) = plan {
            assert_eq!(s, "[from alice] hello");
            assert!(!s.contains('\n'));
        } else {
            panic!("expected inline");
        }
    }

    #[test]
    fn test_plan_paste_spill_on_body() {
        let plan = plan_paste("alice", "hello", "body", "full", false);
        assert_eq!(
            plan,
            PastePlan::Spill {
                file_body: "full".to_string()
            }
        );
    }

    #[test]
    fn test_plan_paste_gemini_shell_trigger() {
        // contains '!' -> spill for gemini
        let plan = plan_paste("alice", "bang!", "", "full", true);
        assert_eq!(
            plan,
            PastePlan::Spill {
                file_body: "full".to_string()
            }
        );

        // but inline for others
        let plan2 = plan_paste("alice", "bang!", "", "full", false);
        assert!(matches!(plan2, PastePlan::Inline(_)));
    }

    #[test]
    fn test_plan_paste_oversized() {
        let long_summary = "a".repeat(MAX_INLINE_PASTE_BYTES + 1);
        let plan = plan_paste("alice", &long_summary, "", "full", false);
        assert_eq!(
            plan,
            PastePlan::Spill {
                file_body: "full".to_string()
            }
        );
    }

    #[test]
    fn test_render_atref() {
        let atref = render_atref("alice", "Greeting!\nNext line", ".exo/tmp/file.md");
        assert!(atref.contains(".exo/tmp/file.md"));
        assert!(!atref.contains('!'));
        assert!(!atref.contains('\n'));
        assert!(atref.contains("Greeting"));
    }
}
