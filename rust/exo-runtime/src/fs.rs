//! `impl Fs for Runtime` — file IO (papers, side-files for oversized message bodies).
//! `read` is `tokio::fs::read`; `write_atomic` delegates to `util::atomic_write`
//! (atomic replace on a local fs), creating parent dirs first. Do NOT use blocking
//! `std::fs` in the async body.

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
        crate::util::atomic_write(path, bytes)
            .await
            .map_err(|e| FsError::At {
                op: "write_atomic",
                path: path.display().to_string(),
                source: e,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::{AgentName, Branch, NodePath, PaneId};
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_write_atomic_creates_parent_dir() {
        let dir = tempdir().unwrap();
        let nested_path = dir.path().join("a/b/c.txt");

        let node_path = NodePath::new(vec![AgentName::new("test".into()).unwrap()]).unwrap();
        let runtime = Runtime::new(
            node_path,
            Branch::new("main".into()).unwrap(),
            dir.path().to_path_buf(),
            None,
            "run-1".into(),
            "session-1".into(),
            PaneId::new("%1".into()).unwrap(),
            exo_caps::ChildKind::Worktree,
        );

        let content = b"hello world";
        runtime
            .write_atomic(&nested_path, content)
            .await
            .expect("should succeed");

        assert!(nested_path.exists());
        let read_back = tokio::fs::read(&nested_path).await.unwrap();
        assert_eq!(read_back, content);

        // Verify parent dirs were created
        assert!(dir.path().join("a/b").is_dir());
    }
}
