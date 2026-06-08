//! Error type for exo-scry.
//!
//! Every systems-level failure mode is a distinct, inspectable variant — no
//! stringly-typed soup, and no panics on the happy path's edges (a process
//! exiting mid-probe is `ProcessGone`, not a crash).

use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ScryError {
    /// `/proc` is not available — exo-scry's resolution is Linux-only.
    #[error("/proc is unavailable (exo-scry requires Linux): {0}")]
    ProcUnavailable(String),

    /// The target process disappeared (exited) while we were inspecting it.
    #[error("process {0} is gone (it exited during the probe)")]
    ProcessGone(i32),

    /// We lack permission to read the target process's `/proc` entries.
    #[error("permission denied reading process {0}")]
    PermissionDenied(i32),

    /// No Claude Code process found in the requested direction of `start`.
    #[error(
        "no Claude Code process found in the {direction} of pid {start} (walked {walked} hops)"
    )]
    NoClaudeProcess {
        start: i32,
        direction: &'static str,
        walked: usize,
    },

    /// A `tmux` query failed (tmux not running, pane gone, etc.).
    #[error("tmux query failed: {0}")]
    Tmux(String),

    /// More than one live Claude session shares this working directory, so the
    /// transcript signal cannot map the process to a single session. On Linux,
    /// resolve by pid via the inotify path instead (each session's watch is its
    /// own); on macOS this is unresolvable from OS state alone.
    #[error("ambiguous: {} live Claude sessions share cwd {} (pids: {pids:?})", pids.len(), cwd.display())]
    AmbiguousCwd { cwd: PathBuf, pids: Vec<i32> },

    /// `$HOME` could not be resolved, so `~/.claude` is unknown.
    #[error("cannot resolve $HOME to locate ~/.claude")]
    HomeUnknown,

    /// A team's `config.json` could not be read.
    #[error("failed to read team config {}: {source}", path.display())]
    TeamConfigRead {
        path: PathBuf,
        source: std::io::Error,
    },

    /// A team's `config.json` was malformed.
    #[error("malformed team config {}: {source}", path.display())]
    TeamConfigParse {
        path: PathBuf,
        source: serde_json::Error,
    },

    /// A JSON file under `~/.claude` (e.g. an inbox) could not be parsed.
    #[error("malformed JSON {}: {source}", path.display())]
    Json {
        path: PathBuf,
        source: serde_json::Error,
    },

    /// Catch-all for unexpected I/O.
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
}

impl ScryError {
    /// Map a `procfs::ProcError` against the pid it concerned, collapsing the
    /// "process vanished" and "not permitted" cases into typed variants.
    pub(crate) fn from_proc(pid: i32, e: procfs::ProcError) -> Self {
        match e {
            procfs::ProcError::NotFound(_) => ScryError::ProcessGone(pid),
            procfs::ProcError::PermissionDenied(_) => ScryError::PermissionDenied(pid),
            procfs::ProcError::Io(io, _) => ScryError::Io(io),
            other => ScryError::ProcUnavailable(other.to_string()),
        }
    }
}

pub type Result<T> = std::result::Result<T, ScryError>;

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn from_proc_maps_notfound_to_process_gone() {
        let e = procfs::ProcError::NotFound(None);
        let scry_e = ScryError::from_proc(7, e);
        match scry_e {
            ScryError::ProcessGone(pid) => assert_eq!(pid, 7),
            _ => panic!("Expected ProcessGone, got {:?}", scry_e),
        }
    }

    #[test]
    fn from_proc_maps_permission_denied() {
        let e = procfs::ProcError::PermissionDenied(None);
        let scry_e = ScryError::from_proc(7, e);
        match scry_e {
            ScryError::PermissionDenied(pid) => assert_eq!(pid, 7),
            _ => panic!("Expected PermissionDenied, got {:?}", scry_e),
        }
    }

    #[test]
    fn from_proc_other_is_proc_unavailable() {
        // Incomplete exists in procfs 0.17 and is neither NotFound, PermissionDenied, nor Io
        let e = procfs::ProcError::Incomplete(None);
        let scry_e = ScryError::from_proc(7, e);
        match scry_e {
            ScryError::ProcUnavailable(_) => {}
            _ => panic!("Expected ProcUnavailable, got {:?}", scry_e),
        }
    }

    #[test]
    fn display_messages() {
        assert!(ScryError::ProcessGone(9).to_string().contains("9"));
        assert!(ScryError::ProcessGone(9).to_string().contains("gone"));

        let e = ScryError::NoClaudeProcess {
            start: 3,
            direction: "ancestry",
            walked: 5,
        };
        assert!(e.to_string().contains("ancestry"));
        assert!(e.to_string().contains("5"));

        let e = ScryError::AmbiguousCwd {
            cwd: PathBuf::from("/w"),
            pids: vec![1, 2],
        };
        let s = e.to_string();
        assert!(s.contains("2"));
        assert!(s.contains("/w"));
    }
}
