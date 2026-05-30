//! Domain types. Newtypes over primitives so a pid can't be confused with an
//! inode and a team name can't be confused with an inbox name (parse, don't
//! validate — once constructed, they're meaningful).

use serde::Serialize;
use std::path::PathBuf;

/// An OS process id. Linux/`procfs` use `i32`; we keep that width.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Pid(pub i32);

impl std::fmt::Display for Pid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A Claude Teams team name (1:1 with its task-list directory name).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TeamName(pub String);

impl std::fmt::Display for TeamName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The fully-resolved answer: the active team of a Claude Code session, plus
/// the evidence trail (which process, which on-disk dir) for debuggability.
#[derive(Debug, Clone, Serialize)]
pub struct ActiveTeam {
    /// The Claude Code process this team belongs to.
    pub claude_pid: Pid,
    /// The team name (= watched `tasks/{team}` dir name).
    pub team: TeamName,
    /// The team's task-list directory that produced the signal.
    pub tasks_dir: PathBuf,
    /// The lead member's inbox name (where `notify_parent`-to-root should write).
    pub lead_inbox: Option<String>,
    /// CC-assigned lead session UUID — the globally-unique team handle.
    pub lead_session_id: Option<String>,
}
