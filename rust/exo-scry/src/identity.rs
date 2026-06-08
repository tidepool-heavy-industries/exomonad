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
    /// The Claude Code process this team belongs to. `Some` for process-based
    /// resolution (the inotify path); `None` for UUID-based resolution, which
    /// identifies the team without a process handle.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub claude_pid: Option<Pid>,
    /// The team name (= watched `tasks/{team}` dir name).
    pub team: TeamName,
    /// The team's task-list directory that produced the signal.
    pub tasks_dir: PathBuf,
    /// The lead member's inbox name (where `notify_parent`-to-root should write).
    pub lead_inbox: Option<String>,
    /// CC-assigned lead session UUID — the globally-unique team handle.
    pub lead_session_id: Option<String>,
    /// The caller's own member entry, when resolved by tmux pane (i.e. the
    /// caller is a teammate). `None` for watch/UUID resolution.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub me: Option<crate::teams::Teammate>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pid_display() {
        assert_eq!(format!("{}", Pid(42)), "42");
    }

    #[test]
    fn team_name_display() {
        assert_eq!(format!("{}", TeamName("alpha".into())), "alpha");
    }

    #[test]
    fn active_team_serde_skips_none_optionals() {
        let at = ActiveTeam {
            claude_pid: None,
            team: TeamName("t".into()),
            tasks_dir: PathBuf::from("/x"),
            lead_inbox: Some("lead".into()),
            lead_session_id: None,
            me: None,
        };
        let val: serde_json::Value = serde_json::to_value(&at).unwrap();
        let obj = val.as_object().unwrap();

        assert!(obj.contains_key("team"));
        assert!(obj.contains_key("tasks_dir"));
        assert!(obj.contains_key("lead_inbox"));

        // These should be skipped because they are None and have skip_serializing_if
        assert!(!obj.contains_key("claude_pid"));
        assert!(!obj.contains_key("me"));

        // This one is None but DOES NOT have skip_serializing_if
        assert!(obj.contains_key("lead_session_id"));
        assert!(obj["lead_session_id"].is_null());
    }
}
