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
    async fn read(&self, _path: &Path) -> Result<Vec<u8>, FsError> {
        todo!("R3: tokio::fs::read(path), map io error to FsError::At with op=read")
    }

    async fn write_atomic(&self, _path: &Path, _bytes: &[u8]) -> Result<(), FsError> {
        todo!("R3: write sibling temp then tokio::fs::rename over path (atomic replace)")
    }
}
