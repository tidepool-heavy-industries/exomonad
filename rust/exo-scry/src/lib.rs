//! exo-scry — derive a Claude Code session's active team from live OS state
//! (process tree + inotify watches + on-disk team configs) with **zero
//! registration**. The active team is *observed*, not recorded, so it can't go
//! stale, can't be lost on restart, and can't collide across sessions (the PID
//! anchors it).
//!
//! ## Two resolution paths
//!
//! - [`resolve_active_team`] — **process-based**, Linux-only. Given a pid or
//!   tmux pane, it walks the process tree to the Claude Code session and reads
//!   the kernel's inotify bookkeeping (`/proc/{pid}/fdinfo`) to see which
//!   `tasks/{team}` dir that session is actively watching. This is the only way
//!   to resolve a *third party's* team (you can't learn another process's
//!   session UUID on Linux — it's absent from its fds, env, and watches).
//!
//! - [`resolve_by_session`] — **UUID-based**, fully portable. Given a session
//!   UUID, it scans team configs for the one whose `leadSessionId` matches. No
//!   `/proc`, no inotify. This is the path for self/sidecar contexts (Claude
//!   Code hands a process its own `session_id`) and for non-Linux platforms.
//!   CC enforces one team per leader, so the match is unambiguous.
//!
//! Both observe live, on-disk state — nothing is registered, so neither can go
//! stale or be lost on restart.

pub mod error;
pub mod identity;
pub mod inbox;
pub mod signal;
pub mod target;
pub mod teams;
pub mod transcript;

#[cfg(target_os = "linux")]
pub mod inotify;
#[cfg(target_os = "linux")]
pub mod pathmap;
#[cfg(target_os = "linux")]
pub mod proc;
#[cfg(target_os = "linux")]
pub mod resolve;
#[cfg(target_os = "linux")]
pub mod tmux;

pub use error::{Result, ScryError};
pub use identity::{ActiveTeam, Pid, TeamName};
pub use signal::ActiveTeamSignal;
pub use target::ProbeTarget;

/// Resolve the active team for a known session UUID by scanning team configs.
///
/// Fully portable (no `/proc`, no inotify) — this is the signal for self/sidecar
/// contexts, where Claude Code hands the process its own `session_id`, and for
/// non-Linux platforms where the watch signal is unavailable. Returns `None`
/// when no live team has this session as its lead.
pub fn resolve_by_session(session_id: &str) -> Result<Option<ActiveTeam>> {
    let Some(team) = teams::find_team_by_session(session_id)? else {
        return Ok(None);
    };
    let tasks_dir = teams::tasks_root()?.join(&team);
    // A config whose task dir is gone is a half-removed team — treat as inactive.
    if !tasks_dir.is_dir() {
        return Ok(None);
    }
    let cfg = teams::load_team(&team)?;
    Ok(Some(ActiveTeam {
        claude_pid: None,
        team: TeamName(team),
        tasks_dir,
        lead_inbox: cfg.lead_inbox().map(str::to_string),
        lead_session_id: cfg.lead_session_id().map(str::to_string),
        me: None,
    }))
}

/// Resolve **this process's own** active team — the entry point for a sidecar /
/// MCP server that wants its identity without choosing a strategy. Resolve
/// lazily per call (never cache): the process may have started before any team
/// existed. On Linux this uses the inotify signal, which is robust even when
/// several sessions share a cwd.
#[cfg(target_os = "linux")]
pub fn resolve_self() -> Result<Option<ActiveTeam>> {
    resolve_active_team(ProbeTarget::SelfProcess)
}

/// Resolve the active team for `target` using the default platform signal.
#[cfg(target_os = "linux")]
pub fn resolve_active_team(target: ProbeTarget) -> Result<Option<ActiveTeam>> {
    resolve::resolve_with(target, &signal::InotifyWatchSignal)
}

/// Resolve the active team via the portable **cwd → transcript** signal.
///
/// Works on Linux and (with a libproc cwd reader) macOS. Assumes one live Claude
/// session per cwd; if several share it, returns [`ScryError::AmbiguousCwd`]
/// rather than guess. On Linux, [`resolve_active_team`] (inotify) resolves that
/// ambiguous case from the per-process watch.
#[cfg(target_os = "linux")]
pub fn resolve_via_transcript(target: ProbeTarget) -> Result<Option<ActiveTeam>> {
    resolve::resolve_via_transcript(target)
}

/// Resolve **this process's own** active team, trying the most robust signal
/// first and falling back to the portable path before giving up — the entry
/// point for a sidecar's last-hop dispatch.
///
/// 1. **Inotify watch signal** ([`resolve_self`], Linux) — the primary path,
///    robust even when several sessions share a cwd.
/// 2. **Portable cwd→transcript fallback** ([`resolve_via_transcript`], which
///    itself ends in the portable [`resolve_by_session`] config scan) — tried
///    only when the watch signal yields no team or errors transiently. This is
///    the rung that lets a non-inotify build still find the team.
///
/// The fallback is portable *by design* (cwd→transcript→config-scan, no
/// inotify), but its cwd reader is currently Linux-only, so on non-Linux this
/// resolves to `None`: that path is **wired but untested**.
pub fn resolve_self_or_portable() -> Result<Option<ActiveTeam>> {
    #[cfg(target_os = "linux")]
    {
        // resolve_self stays the primary path; the portable fallback runs only on
        // its failure (no team found, or a transient `/proc`/config race).
        match resolve_self() {
            Ok(Some(team)) => Ok(Some(team)),
            Ok(None) | Err(_) => resolve_via_transcript(ProbeTarget::SelfProcess),
        }
    }
    #[cfg(not(target_os = "linux"))]
    {
        Ok(None)
    }
}
