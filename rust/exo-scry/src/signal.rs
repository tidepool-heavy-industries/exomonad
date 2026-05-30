//! The one genuinely unportable bit: "which `tasks/{team}` dir is this Claude
//! session actively bound to." On Linux we read it for free from the kernel's
//! inotify bookkeeping. Other platforms need a different `ActiveTeamSignal`
//! impl (e.g. matching the session UUID against team configs) — this trait is
//! that seam.

use crate::error::Result;
use crate::identity::Pid;
use std::path::{Path, PathBuf};

/// A strategy for deriving the active `tasks/{team}` directory of a Claude Code
/// process. Implementations *observe* live state; none of them register
/// anything.
pub trait ActiveTeamSignal {
    fn active_team_dir(&self, claude_pid: Pid, tasks_root: &Path) -> Result<Option<PathBuf>>;
}

#[cfg(target_os = "linux")]
pub use linux::InotifyWatchSignal;

#[cfg(target_os = "linux")]
mod linux {
    use super::*;
    use crate::{inotify, pathmap};

    /// Linux fast-path: Claude Code holds a persistent inotify watch on its
    /// active `tasks/{team}` dir. We read the watched inodes from the kernel's
    /// fdinfo and match them against the inodes of the candidate dirs.
    pub struct InotifyWatchSignal;

    impl ActiveTeamSignal for InotifyWatchSignal {
        fn active_team_dir(&self, claude_pid: Pid, tasks_root: &Path) -> Result<Option<PathBuf>> {
            let inodes = inotify::watched_inodes(claude_pid.0)?;
            Ok(pathmap::match_dir_by_inode(tasks_root, &inodes)?)
        }
    }
}
