//! `impl Log for Runtime` — structured logging to a file at the git worktree-root.
//!
//! **Leaf R3.** Sync trait (no async). Write to a log file under `self.working_dir`
//! (the worktree-root, the one dir not removed on reclaim). Simplest correct impl:
//! append a line via `std::fs::OpenOptions::append` (a sync `info`/`error` is fine —
//! it's not in an async fn). Mirror exomonad-core `services/log.rs` formatting.

use crate::runtime::Runtime;
use exo_caps::Log;

impl Log for Runtime {
    fn info(&self, _msg: &str) {
        todo!("R3: append an info line to the worktree-root log file under self.working_dir")
    }

    fn error(&self, _msg: &str) {
        todo!("R3: append an error line to the worktree-root log file under self.working_dir")
    }
}
