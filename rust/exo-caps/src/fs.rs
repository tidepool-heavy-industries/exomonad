//! `Fs` capability — file IO (papers, side-files for oversized message bodies).
//! `write_atomic` is temp+rename. Signatures firm up in Wave 1.

use crate::error::CapResult;
use async_trait::async_trait;
use std::path::Path;

#[async_trait]
pub trait Fs {
    async fn read(&self, path: &Path) -> CapResult<Vec<u8>>;
    /// Atomic write (temp + rename) — so a reader never sees a half-written file.
    async fn write_atomic(&self, path: &Path, bytes: &[u8]) -> CapResult<()>;
}
