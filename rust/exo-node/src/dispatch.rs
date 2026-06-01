//! **N2a — Last-hop dispatch.** Route one consumed ingestion entry INTO this agent, by the
//! node's own `agent_type` (= `kind.agent_type()`) + CC team membership (resolved via
//! `exo-scry`):
//!
//! | this node | mechanism |
//! |---|---|
//! | CC, in a team | write the CC Teams inbox → InboxPoller → `<teammate-message>` |
//! | CC, no team   | tmux-paste into its own pane |
//! | gemini        | tmux-paste into its own pane |
//!
//! For the tmux-paste path, render the entry with a `[from: X, kind: Y]` header (the input
//! box *is* the receive channel for non-CC runtimes). Reuse exomonad-core's tmux injection
//! (buffer pattern) + CC-inbox delivery — adapt, don't rewrite.
//!
//! **Status: stub (N2a leaf fills this).** Acceptance: a `Chat` entry delivered to a gemini
//! node lands pasted-with-header in its pane; a CC-in-team node's entry lands in its Teams
//! inbox. The dispatch is pure last-hop — `kind`-based routing (event/control) is N2b's job;
//! this only does the agent-facing write for entries N2b decides to deliver.

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
    if agent_type == AgentType::Claude {
        if let Some(team) = active_team {
            let to = if let Some(me) = team.me {
                Some(me.name)
            } else {
                team.lead_inbox
            };
            if let Some(to) = to {
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

    #[test]
    fn test_decide_lasthop_claude_in_team() {
        use exo_scry::identity::TeamName;
        use exo_scry::teams::Teammate;
        use std::path::PathBuf;

        let active_team = exo_scry::ActiveTeam {
            claude_pid: None,
            team: TeamName("myteam".to_string()),
            tasks_dir: PathBuf::from("/tmp"),
            lead_inbox: Some("lead".to_string()),
            lead_session_id: None,
            me: Some(Teammate {
                agent_id: "agent1".to_string(),
                name: "bob".to_string(),
                agent_type: "worker".to_string(),
                model: "sonnet".to_string(),
                cwd: "/".to_string(),
                tmux_pane_id: "%1".to_string(),
                backend_type: "mcp".to_string(),
                is_active: None,
            }),
        };

        let hop = decide_lasthop(AgentType::Claude, Some(active_team));
        assert_eq!(
            hop,
            LastHop::TeamsInbox {
                team: "myteam".to_string(),
                to: "bob".to_string()
            }
        );
    }
}
