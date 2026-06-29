use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};

static COUNTER: AtomicU64 = AtomicU64::new(0);

/// Write `bytes` to `path` atomically: create parent dirs, write to a sibling temp file
/// (`{name}.{pid}.{n}.tmp`), then rename into place.
pub(crate) async fn atomic_write(path: &Path, bytes: &[u8]) -> std::io::Result<()> {
    let parent = path.parent().ok_or_else(|| {
        std::io::Error::new(std::io::ErrorKind::InvalidInput, "path has no parent")
    })?;

    tokio::fs::create_dir_all(parent).await?;

    let file_name = path
        .file_name()
        .ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::InvalidInput, "path has no file name")
        })?
        .to_string_lossy();

    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    let tmp_path = parent.join(format!("{}.{}.{}.tmp", file_name, std::process::id(), id));

    tokio::fs::write(&tmp_path, bytes).await?;
    tokio::fs::rename(&tmp_path, path).await
}
