//! Filesystem service for WASM host functions.
//!
//! Provides file read/write operations for hooks and MCP tools that need
//! to access file contents (e.g., reading transcript files, writing context).

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use tokio::fs;

// ============================================================================
// Service
// ============================================================================

/// Input for reading a file.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct ReadFileInput {
    /// Path to the file (absolute or relative to project_dir)
    pub path: String,
    /// Maximum bytes to read (0 = unlimited)
    #[serde(default)]
    pub max_bytes: usize,
}

/// Result of reading a file.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ReadFileOutput {
    /// File contents (UTF-8)
    pub content: String,
    /// Number of bytes read
    pub bytes_read: usize,
    /// Whether the file was truncated
    pub truncated: bool,
}

/// Input for writing a file.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct WriteFileInput {
    /// Path to the file (absolute or relative to project_dir)
    pub path: String,
    /// Content to write
    pub content: String,
    /// Whether to create parent directories
    #[serde(default = "default_true")]
    pub create_parents: bool,
}

fn default_true() -> bool {
    true
}

/// Result of writing a file.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct WriteFileOutput {
    /// Number of bytes written
    pub bytes_written: usize,
    /// Absolute path of the written file
    pub path: String,
}

// ============================================================================
// Service
// ============================================================================

/// Filesystem service for file operations.
pub struct FileSystemService {
    /// Project root directory (for resolving relative paths)
    project_dir: PathBuf,
}

impl FileSystemService {
    /// Create a new filesystem service.
    pub fn new(project_dir: PathBuf) -> Self {
        Self { project_dir }
    }

    /// Create from current working directory.
    pub fn from_cwd() -> Result<Self> {
        let project_dir = std::env::current_dir().context("Failed to get current directory")?;
        Ok(Self { project_dir })
    }

    /// Resolve a path (absolute or relative to project_dir).
    fn resolve_path(&self, path: &str) -> PathBuf {
        let p = PathBuf::from(path);
        if p.is_absolute() {
            p
        } else {
            self.project_dir.join(p)
        }
    }

    /// Read a file.
    #[tracing::instrument(skip(self))]
    pub async fn read_file(&self, input: &ReadFileInput) -> Result<ReadFileOutput> {
        let path = self.resolve_path(&input.path);

        let content = fs::read_to_string(&path)
            .await
            .with_context(|| format!("Failed to read file: {}", path.display()))?;

        let bytes_read = content.len();
        let (content, truncated) = if input.max_bytes > 0 && bytes_read > input.max_bytes {
            let truncated_content: String = content.chars().take(input.max_bytes).collect();
            (truncated_content, true)
        } else {
            (content, false)
        };

        Ok(ReadFileOutput {
            content,
            bytes_read,
            truncated,
        })
    }

    /// Write a file.
    #[tracing::instrument(skip(self))]
    pub async fn write_file(&self, input: &WriteFileInput) -> Result<WriteFileOutput> {
        let path = self.resolve_path(&input.path);

        if input.create_parents {
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).await.with_context(|| {
                    format!(
                        "Failed to create parent directories for: {}",
                        path.display()
                    )
                })?;
            }
        }

        let bytes_written = input.content.len();
        fs::write(&path, &input.content)
            .await
            .with_context(|| format!("Failed to write file: {}", path.display()))?;

        Ok(WriteFileOutput {
            bytes_written,
            path: path.to_string_lossy().to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_read_write_file() {
        let dir = tempdir().unwrap();
        let service = FileSystemService::new(dir.path().to_path_buf());

        // Write a file
        let write_input = WriteFileInput {
            path: "test.txt".to_string(),
            content: "Hello, World!".to_string(),
            create_parents: true,
        };
        let write_result = service.write_file(&write_input).await.unwrap();
        assert_eq!(write_result.bytes_written, 13);

        // Read it back
        let read_input = ReadFileInput {
            path: "test.txt".to_string(),
            max_bytes: 0,
        };
        let read_result = service.read_file(&read_input).await.unwrap();
        assert_eq!(read_result.content, "Hello, World!");
        assert!(!read_result.truncated);
    }

    #[tokio::test]
    async fn test_read_with_truncation() {
        let dir = tempdir().unwrap();
        let service = FileSystemService::new(dir.path().to_path_buf());

        // Write a file
        let write_input = WriteFileInput {
            path: "long.txt".to_string(),
            content: "Hello, World! This is a longer message.".to_string(),
            create_parents: true,
        };
        service.write_file(&write_input).await.unwrap();

        // Read with truncation
        let read_input = ReadFileInput {
            path: "long.txt".to_string(),
            max_bytes: 5,
        };
        let read_result = service.read_file(&read_input).await.unwrap();
        assert_eq!(read_result.content, "Hello");
        assert!(read_result.truncated);
    }

    #[tokio::test]
    async fn test_create_parent_directories() {
        let dir = tempdir().unwrap();
        let service = FileSystemService::new(dir.path().to_path_buf());

        // Write to nested path
        let write_input = WriteFileInput {
            path: "a/b/c/test.txt".to_string(),
            content: "nested".to_string(),
            create_parents: true,
        };
        let result = service.write_file(&write_input).await.unwrap();
        assert!(result.path.ends_with("a/b/c/test.txt"));

        // Verify file exists
        let read_input = ReadFileInput {
            path: "a/b/c/test.txt".to_string(),
            max_bytes: 0,
        };
        let read_result = service.read_file(&read_input).await.unwrap();
        assert_eq!(read_result.content, "nested");
    }
}
