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

use std::sync::Arc;

use exo_caps::{AgentType, IngestionEntry, MessageKind, Persona, Tmux};
use tracing::warn;

use crate::bootstrap::NodeContext;
use crate::error::{NodeError, NodeResult};

/// Deliver one ingestion entry into this node's own agent (the runtime-specific last hop).
pub async fn dispatch(ctx: &Arc<NodeContext>, entry: &IngestionEntry) -> NodeResult<()> {
    let agent_type = ctx.kind.agent_type();

    // Resolve THIS agent's own team. `resolve_self` walks from the sidecar up to its parent
    // `claude` process and reads that process's inotify-bound `tasks/{team}` dir — so it finds
    // the agent's own (solo) team without needing a `tmux_pane_id` (which CC never writes into
    // its team config; that's why `resolve_by_pane` always missed and native delivery never
    // fired). Resolution failure is non-fatal: fall back to paste rather than wedge delivery —
    // but a transient error (a `/proc` race, a half-written team config) is NOT the same as
    // "no team": log it, so a Claude node silently degrading to paste is visible, not a mystery.
    #[cfg(target_os = "linux")]
    let active_team = match exo_scry::resolve_self() {
        Ok(team) => team,
        Err(e) => {
            warn!("resolve_self failed; falling back to tmux paste for this delivery: {e}");
            None
        }
    };
    #[cfg(not(target_os = "linux"))]
    let active_team = None;

    match decide_lasthop(agent_type, active_team) {
        LastHop::TeamsInbox { team, to } => {
            let persona_str = render_persona(&entry.from);
            exo_scry::inbox::send_message(
                &team,
                &to,
                &persona_str,
                entry.msg.text.as_str(),
                entry.msg.summary.as_str(),
            )
            .map_err(|e| NodeError::Scry(e.to_string()))?;
            Ok(())
        }
        LastHop::TmuxPaste => {
            let rendered = render_entry(entry);
            Tmux::paste(&*ctx.runtime, &ctx.own_pane, &rendered)
                .await
                .map_err(|e| NodeError::Scry(format!("Tmux paste failed: {}", e)))?;
            Ok(())
        }
    }
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

fn render_persona(persona: &Persona) -> String {
    match persona {
        Persona::Agent(name) => name.as_str().to_string(),
        Persona::Synthetic(name) => name.as_str().to_string(),
    }
}

fn render_entry(entry: &IngestionEntry) -> String {
    let persona = render_persona(&entry.from);
    let kind_str = match &entry.msg.kind {
        MessageKind::Chat => "chat",
        MessageKind::Event => "event",
        MessageKind::Control(_) => "control",
    };

    format!(
        "[from: {}, kind: {}]\n\n{}\n{}",
        persona,
        kind_str,
        entry.msg.summary.as_str(),
        entry.msg.text.as_str()
    )
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
}
