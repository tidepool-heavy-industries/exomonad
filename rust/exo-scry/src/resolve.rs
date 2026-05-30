//! Orchestration: `ProbeTarget` → Claude pid → active team dir → identity.

use crate::error::{Result, ScryError};
use crate::identity::{ActiveTeam, Pid, TeamName};
use crate::signal::ActiveTeamSignal;
use crate::target::ProbeTarget;
use crate::{proc, teams, transcript, tmux};

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

/// Resolve the active team via the **cwd → transcript** signal: find the
/// target's Claude session, then its active session UUID via the most recent
/// transcript in its cwd's project dir, then the team. Portable in principle
/// (cwd is readable on every Unix); this Linux build reads cwd from `/proc`.
///
/// Fails loud with [`ScryError::AmbiguousCwd`] when more than one live Claude
/// session shares the cwd — it cannot map a cwd to one session, and silently
/// picking the newest transcript would be a coin flip. On Linux, resolve such a
/// pid via the inotify path ([`resolve_with`]) instead.
pub fn resolve_via_transcript(target: ProbeTarget) -> Result<Option<ActiveTeam>> {
    let claude_pid = claude_pid_for(&target)?;
    let cwd = proc::process_cwd(claude_pid.0)?;

    let siblings = proc::claude_pids_with_cwd(&cwd)?;
    if siblings.len() > 1 {
        return Err(ScryError::AmbiguousCwd { cwd, pids: siblings });
    }

    let project_dir = transcript::project_dir(&teams::projects_root()?, &cwd);
    let Some(uuid) = transcript::newest_session(&project_dir)? else {
        return Ok(None);
    };
    // Reuse the portable config scan, then stamp the evidence pid.
    let mut team = crate::resolve_by_session(&uuid)?;
    if let Some(t) = team.as_mut() {
        t.claude_pid = Some(claude_pid);
    }
    Ok(team)
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
    let cfg = teams::load_team(&team_name).ok();
    Ok(Some(ActiveTeam {
        claude_pid: Some(claude_pid),
        team: TeamName(team_name),
        tasks_dir: dir,
        lead_inbox: cfg.as_ref().and_then(|c| c.lead_inbox().map(str::to_string)),
        lead_session_id: cfg.and_then(|c| c.lead_session_id().map(str::to_string)),
        me: None,
    }))
}
