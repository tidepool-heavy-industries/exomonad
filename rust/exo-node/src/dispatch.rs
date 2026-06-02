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

use crate::bootstrap::NodeContext;
use crate::error::{NodeError, NodeResult};

/// Deliver one ingestion entry into this node's own agent (the runtime-specific last hop).
pub async fn dispatch(ctx: &Arc<NodeContext>, entry: &IngestionEntry) -> NodeResult<()> {
    let agent_type = ctx.kind.agent_type();

    // Resolve CC membership
    let active_team = exo_scry::resolve_by_pane(ctx.own_pane.as_str())
        .map_err(|e| NodeError::Scry(e.to_string()))?;

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
    // Native `<teammate-message>` delivery only works for the team LEAD: its own CC session
    // polls its inbox. A synthetic member (any spawned child) is not natively polled by
    // anyone, so writing its Teams inbox would go nowhere — deliver via tmux paste, which the
    // sidecar controls. (The sidecar separately watches a synthetic member's Teams inbox to
    // catch native `teams-mcp` sends — see inbound.rs.)
    if agent_type == AgentType::Claude {
        if let Some(team) = active_team {
            if let Some(me) = &team.me {
                let is_lead = team.lead_inbox.as_deref() == Some(me.name.as_str());
                if is_lead {
                    return LastHop::TeamsInbox {
                        team: team.team.0,
                        to: me.name.clone(),
                    };
                }
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

    fn teammate(name: &str) -> exo_scry::teams::Teammate {
        exo_scry::teams::Teammate {
            agent_id: format!("agent-{name}"),
            name: name.to_string(),
            agent_type: "worker".to_string(),
            model: "sonnet".to_string(),
            cwd: "/".to_string(),
            tmux_pane_id: "%1".to_string(),
            backend_type: "mcp".to_string(),
            is_active: None,
        }
    }

    #[test]
    fn test_decide_lasthop_claude_lead_gets_native() {
        use exo_scry::identity::TeamName;
        use std::path::PathBuf;

        // me IS the lead (name matches lead_inbox) → native Teams delivery.
        let active_team = exo_scry::ActiveTeam {
            claude_pid: None,
            team: TeamName("myteam".to_string()),
            tasks_dir: PathBuf::from("/tmp"),
            lead_inbox: Some("lead".to_string()),
            lead_session_id: None,
            me: Some(teammate("lead")),
        };

        let hop = decide_lasthop(AgentType::Claude, Some(active_team));
        assert_eq!(
            hop,
            LastHop::TeamsInbox {
                team: "myteam".to_string(),
                to: "lead".to_string()
            }
        );
    }

    #[test]
    fn test_decide_lasthop_claude_synthetic_member_pastes() {
        use exo_scry::identity::TeamName;
        use std::path::PathBuf;

        // me is a non-lead (synthetic) member → not natively polled → tmux paste.
        let active_team = exo_scry::ActiveTeam {
            claude_pid: None,
            team: TeamName("myteam".to_string()),
            tasks_dir: PathBuf::from("/tmp"),
            lead_inbox: Some("lead".to_string()),
            lead_session_id: None,
            me: Some(teammate("bob")),
        };

        let hop = decide_lasthop(AgentType::Claude, Some(active_team));
        assert_eq!(hop, LastHop::TmuxPaste);
    }

    #[test]
    fn test_decide_lasthop_claude_no_me_pastes() {
        use exo_scry::identity::TeamName;
        use std::path::PathBuf;

        // Resolved a team but no own member entry → can't be the lead → paste.
        let active_team = exo_scry::ActiveTeam {
            claude_pid: None,
            team: TeamName("myteam".to_string()),
            tasks_dir: PathBuf::from("/tmp"),
            lead_inbox: Some("lead".to_string()),
            lead_session_id: None,
            me: None,
        };

        let hop = decide_lasthop(AgentType::Claude, Some(active_team));
        assert_eq!(hop, LastHop::TmuxPaste);
    }

    #[test]
    fn test_decide_lasthop_claude_in_team_no_recipients() {
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
