//! Filesystem effect handler for the `fs.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::fs`.

use crate::services::filesystem::FileSystemService;
use async_trait::async_trait;
use exomonad_core::effects::{
    dispatch_fs_effect, EffectError, EffectHandler, EffectResult, FilesystemEffects,
};
use exomonad_proto::effects::fs::*;
use std::sync::Arc;

/// Filesystem effect handler.
///
/// Handles all effects in the `fs.*` namespace by delegating to
/// the generated `dispatch_filesystem_effect` function.
pub struct FsHandler {
    service: Arc<FileSystemService>,
}

impl FsHandler {
    pub fn new(service: Arc<FileSystemService>) -> Self {
        Self { service }
    }
}

#[async_trait]
impl EffectHandler for FsHandler {
    fn namespace(&self) -> &str {
        "fs"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_fs_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl FilesystemEffects for FsHandler {
    async fn read_file(&self, req: ReadFileRequest) -> EffectResult<ReadFileResponse> {
        let max_bytes = if req.max_bytes <= 0 {
            1_048_576 // 1MB default
        } else {
            req.max_bytes as usize
        };

        let input = crate::services::filesystem::ReadFileInput {
            path: req.path,
            max_bytes,
        };

        let result = self
            .service
            .read_file(&input)
            .await
            .map_err(|e| EffectError::custom("fs_error", e.to_string()))?;

        Ok(ReadFileResponse {
            content: result.content,
            bytes_read: result.bytes_read as i64,
            truncated: result.truncated,
            total_size: 0, // Service doesn't return total size yet
        })
    }

    async fn write_file(&self, req: WriteFileRequest) -> EffectResult<WriteFileResponse> {
        let input = crate::services::filesystem::WriteFileInput {
            path: req.path.clone(),
            content: req.content,
            create_parents: req.create_parents,
        };

        let result = self
            .service
            .write_file(&input)
            .await
            .map_err(|e| EffectError::custom("fs_error", e.to_string()))?;

        Ok(WriteFileResponse {
            bytes_written: result.bytes_written as i64,
            path: result.path,
        })
    }

    async fn file_exists(&self, req: FileExistsRequest) -> EffectResult<FileExistsResponse> {
        let path = std::path::Path::new(&req.path);
        let exists = path.exists();
        let is_file = path.is_file();
        let is_directory = path.is_dir();

        Ok(FileExistsResponse {
            exists,
            is_file,
            is_directory,
        })
    }

    async fn list_directory(
        &self,
        req: ListDirectoryRequest,
    ) -> EffectResult<ListDirectoryResponse> {
        let path = std::path::Path::new(&req.path);
        if !path.is_dir() {
            return Err(EffectError::not_found(format!(
                "Directory not found: {}",
                req.path
            )));
        }

        let mut entries = Vec::new();
        let mut read_dir = tokio::fs::read_dir(path)
            .await
            .map_err(|e| EffectError::custom("fs_error", e.to_string()))?;

        while let Some(entry) = read_dir
            .next_entry()
            .await
            .map_err(|e| EffectError::custom("fs_error", e.to_string()))?
        {
            let name = entry.file_name().to_string_lossy().to_string();
            if !req.include_hidden && name.starts_with('.') {
                continue;
            }

            let metadata = entry
                .metadata()
                .await
                .map_err(|e| EffectError::custom("fs_error", e.to_string()))?;

            let (size, modified_at) = if req.include_metadata {
                let size = metadata.len() as i64;
                let modified = metadata
                    .modified()
                    .ok()
                    .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                    .map(|d| d.as_secs() as i64)
                    .unwrap_or(0);
                (size, modified)
            } else {
                (0, 0)
            };

            entries.push(FileEntry {
                name,
                is_directory: metadata.is_dir(),
                size,
                modified_at,
            });
        }

        let count = entries.len() as i32;
        Ok(ListDirectoryResponse { entries, count })
    }

    async fn delete_file(&self, req: DeleteFileRequest) -> EffectResult<DeleteFileResponse> {
        let path = std::path::Path::new(&req.path);
        if !path.exists() {
            return Ok(DeleteFileResponse { deleted: false });
        }

        if path.is_dir() {
            if req.recursive {
                tokio::fs::remove_dir_all(path)
                    .await
                    .map_err(|e| EffectError::custom("fs_error", e.to_string()))?;
            } else {
                tokio::fs::remove_dir(path)
                    .await
                    .map_err(|e| EffectError::custom("fs_error", e.to_string()))?;
            }
        } else {
            tokio::fs::remove_file(path)
                .await
                .map_err(|e| EffectError::custom("fs_error", e.to_string()))?;
        }

        Ok(DeleteFileResponse { deleted: true })
    }
}
