//! Orchestration: `ProbeTarget` → Claude pid → active team dir → identity.

use crate::error::Result;
use crate::identity::{ActiveTeam, Pid, TeamName};
use crate::signal::ActiveTeamSignal;
use crate::target::ProbeTarget;
use crate::{proc, teams, tmux};

/// Resolve the Claude Code process id for a target.
fn claude_pid_for(target: &ProbeTarget) -> Result<Pid> {
    match target {
        ProbeTarget::SelfProcess => proc::find_claude_ancestor(proc::self_pid()),
        ProbeTarget::Pid(pid) => proc::find_claude_ancestor(*pid),
        ProbeTarget::TmuxPane(pane) => {
            let pane_pid = tmux::pane_pid(pane)?;
            proc::find_claude_descendant(pane_pid)
        }
    }
}

/// Resolve the active team for a target using the given signal strategy.
pub fn resolve_with<S: ActiveTeamSignal>(
    target: ProbeTarget,
    signal: &S,
) -> Result<Option<ActiveTeam>> {
    let claude_pid = claude_pid_for(&target)?;
    let tasks_root = teams::tasks_root()?;
    let Some(dir) = signal.active_team_dir(claude_pid, &tasks_root)? else {
        return Ok(None);
    };
    let team_name = dir
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or_default()
        .to_string();
    // Best-effort enrichment from the team config (the routing target).
    let cfg = teams::load_team_config(&team_name).ok();
    Ok(Some(ActiveTeam {
        claude_pid,
        team: TeamName(team_name),
        tasks_dir: dir,
        lead_inbox: cfg.as_ref().and_then(|c| c.lead_inbox.clone()),
        lead_session_id: cfg.and_then(|c| c.lead_session_id),
    }))
}
