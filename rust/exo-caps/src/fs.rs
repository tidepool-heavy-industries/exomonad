//! `Fs` capability — file IO (papers, side-files for oversized message bodies).
//! `write_atomic` is temp+rename. Deliberately NO `append`: the two append disciplines
//! (single-writer ledger, multi-writer PIPE_BUF bus) live inside the `Spawner`/`Bus`
//! impls — a raw policy-reachable append would weaken them.

use async_trait::async_trait;
use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum FsError {
    #[error("fs {op} failed at {path}: {source}")]
    At {
        op: &'static str,
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[async_trait]
pub trait Fs {
    async fn read(&self, path: &Path) -> Result<Vec<u8>, FsError>;
    /// Atomic write (temp + rename) — so a reader never sees a half-written file.
    async fn write_atomic(&self, path: &Path, bytes: &[u8]) -> Result<(), FsError>;
}
