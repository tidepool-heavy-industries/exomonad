//! `impl Log for Runtime` — structured logging to a file at the git worktree-root.
//!
//! **Leaf R3.** Sync trait (no async). Write to a log file under `self.working_dir`
//! (the worktree-root, the one dir not removed on reclaim). Simplest correct impl:
//! append a line via `std::fs::OpenOptions::append` (a sync `info`/`error` is fine —
//! it's not in an async fn). Mirror exomonad-core `services/log.rs` formatting.

use crate::runtime::Runtime;
use exo_caps::Log;

use std::fs::OpenOptions;
use std::io::Write;

impl Log for Runtime {
    fn info(&self, msg: &str) {
        let path = self.working_dir().join("exo-runtime.log");
        let _ = (|| -> std::io::Result<()> {
            let mut file = OpenOptions::new().create(true).append(true).open(path)?;
            writeln!(file, "[INFO] {}", msg)
        })();
    }

    fn error(&self, msg: &str) {
        let path = self.working_dir().join("exo-runtime.log");
        let _ = (|| -> std::io::Result<()> {
            let mut file = OpenOptions::new().create(true).append(true).open(path)?;
            writeln!(file, "[ERROR] {}", msg)
        })();
    }
}
