//! exo-scry — derive a Claude Code session's active team from live OS state
//! (process tree + inotify watches + on-disk team configs) with **zero
//! registration**. The active team is *observed*, not recorded, so it can't go
//! stale, can't be lost on restart, and can't collide across sessions (the PID
//! anchors it).
//!
//! ## Platform
//!
//! The *watch* signal ([`signal::InotifyWatchSignal`]) is Linux-only — it reads
//! the kernel's inotify bookkeeping from `/proc/{pid}/fdinfo`. The
//! [`signal::ActiveTeamSignal`] trait is the seam where a portable signal
//! (e.g. matching the session UUID against team configs) plugs in for macOS;
//! the [`teams`] config layer is already portable.

pub mod error;
pub mod identity;
pub mod signal;
pub mod target;
pub mod teams;

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

/// Resolve the active team for `target` using the default platform signal.
#[cfg(target_os = "linux")]
pub fn resolve_active_team(target: ProbeTarget) -> Result<Option<ActiveTeam>> {
    resolve::resolve_with(target, &signal::InotifyWatchSignal)
}
