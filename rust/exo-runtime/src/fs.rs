//! `impl Fs for Runtime` — file IO (papers, side-files for oversized message bodies).
//!
//! **Leaf R3.** Trivial std: `tokio::fs::read` for `read`; `write_atomic` = write to a
//! sibling temp file then `rename` (atomic replace on a local fs), via `tokio::fs`. Do
//! NOT use blocking `std::fs` in the async body.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Fs, FsError};
use std::path::Path;

#[async_trait]
impl Fs for Runtime {
    async fn read(&self, path: &Path) -> Result<Vec<u8>, FsError> {
        tokio::fs::read(path).await.map_err(|e| FsError::At {
            op: "read",
            path: path.display().to_string(),
            source: e,
        })
    }

    async fn write_atomic(&self, path: &Path, bytes: &[u8]) -> Result<(), FsError> {
        let parent = path.parent().ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::InvalidInput, "path has no parent")
        })?;
        let file_name = path
            .file_name()
            .ok_or_else(|| {
                std::io::Error::new(std::io::ErrorKind::InvalidInput, "path has no file name")
            })?
            .to_string_lossy();
        let tmp_path = parent.join(format!("{}.tmp", file_name));

        tokio::fs::write(&tmp_path, bytes).await.map_err(|e| FsError::At {
            op: "write_atomic (write tmp)",
            path: tmp_path.display().to_string(),
            source: e,
        })?;

        tokio::fs::rename(&tmp_path, path).await.map_err(|e| FsError::At {
            op: "write_atomic (rename)",
            path: path.display().to_string(),
            source: e,
        })
    }
}
