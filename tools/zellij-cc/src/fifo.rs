use anyhow::{Context, Result};
use nix::sys::stat::Mode;
use nix::unistd::mkfifo;
use std::path::{Path, PathBuf};

/// Guard that removes a FIFO on drop, ensuring cleanup even on error paths.
pub struct FifoGuard {
    path: PathBuf,
}

impl FifoGuard {
    pub fn new(path: PathBuf) -> Result<Self> {
        // Remove any stale FIFO from previous runs (e.g., after crash)
        if path.exists() {
            std::fs::remove_file(&path).ok();
        }
        mkfifo(&path, Mode::S_IRUSR | Mode::S_IWUSR)
            .with_context(|| format!("Failed to create FIFO at {}", path.display()))?;
        Ok(Self { path })
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for FifoGuard {
    fn drop(&mut self) {
        if let Err(err) = std::fs::remove_file(&self.path) {
            eprintln!("warning: failed to remove FIFO {}: {err}", self.path.display());
        }
    }
}
