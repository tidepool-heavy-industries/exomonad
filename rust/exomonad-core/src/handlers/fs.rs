//! Filesystem effect handler for the `fs.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::fs`.

use crate::effects::{
    dispatch_fs_effect, EffectError, EffectResult, FilesystemEffects, ResultExt,
};
use crate::services::filesystem::FileSystemService;
use async_trait::async_trait;
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

crate::impl_pass_through_handler!(FsHandler, "fs", dispatch_fs_effect);

#[async_trait]
impl FilesystemEffects for FsHandler {
    async fn read_file(
        &self,
        req: ReadFileRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ReadFileResponse> {
        tracing::info!(path = %req.path, "[Fs] read_file starting");
        let max_bytes = if req.max_bytes <= 0 {
            1_048_576 // 1MB default
        } else {
            req.max_bytes as usize
        };

        let input = crate::services::filesystem::ReadFileInput {
            path: req.path,
            max_bytes,
        };

        let result = self.service.read_file(&input).await.effect_err("fs")?;

        tracing::info!(bytes_read = result.bytes_read, truncated = result.truncated, "[Fs] read_file complete");
        Ok(ReadFileResponse {
            content: result.content,
            bytes_read: result.bytes_read as i64,
            truncated: result.truncated,
            total_size: 0, // Service doesn't return total size yet
        })
    }

    async fn write_file(
        &self,
        req: WriteFileRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<WriteFileResponse> {
        tracing::info!(path = %req.path, content_bytes = req.content.len(), "[Fs] write_file starting");
        let input = crate::services::filesystem::WriteFileInput {
            path: req.path.clone(),
            content: req.content,
            create_parents: req.create_parents,
        };

        let result = self.service.write_file(&input).await.effect_err("fs")?;

        tracing::info!(bytes_written = result.bytes_written, "[Fs] write_file complete");
        Ok(WriteFileResponse {
            bytes_written: result.bytes_written as i64,
            path: result.path,
        })
    }

    async fn file_exists(
        &self,
        req: FileExistsRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<FileExistsResponse> {
        tracing::info!(path = %req.path, "[Fs] file_exists starting");
        let path = std::path::Path::new(&req.path);
        let exists = path.exists();
        let is_file = path.is_file();
        let is_directory = path.is_dir();

        tracing::info!(exists, is_file, is_directory, "[Fs] file_exists complete");
        Ok(FileExistsResponse {
            exists,
            is_file,
            is_directory,
        })
    }

    async fn list_directory(
        &self,
        req: ListDirectoryRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ListDirectoryResponse> {
        tracing::info!(path = %req.path, "[Fs] list_directory starting");
        let path = std::path::Path::new(&req.path);
        if !path.is_dir() {
            return Err(EffectError::not_found(format!(
                "Directory not found: {}",
                req.path
            )));
        }

        let mut entries = Vec::new();
        let mut read_dir = tokio::fs::read_dir(path).await.effect_err("fs")?;

        while let Some(entry) = read_dir.next_entry().await.effect_err("fs")? {
            let name = entry.file_name().to_string_lossy().to_string();
            if !req.include_hidden && name.starts_with('.') {
                continue;
            }

            let metadata = entry.metadata().await.effect_err("fs")?;

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
        tracing::info!(count, "[Fs] list_directory complete");
        Ok(ListDirectoryResponse { entries, count })
    }

    async fn delete_file(
        &self,
        req: DeleteFileRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<DeleteFileResponse> {
        tracing::info!(path = %req.path, recursive = req.recursive, "[Fs] delete_file starting");
        let path = std::path::Path::new(&req.path);
        if !path.exists() {
            return Ok(DeleteFileResponse { deleted: false });
        }

        if path.is_dir() {
            if req.recursive {
                tokio::fs::remove_dir_all(path).await.effect_err("fs")?;
            } else {
                tokio::fs::remove_dir(path).await.effect_err("fs")?;
            }
        } else {
            tokio::fs::remove_file(path).await.effect_err("fs")?;
        }

        tracing::info!(path = %req.path, "[Fs] delete_file complete");
        Ok(DeleteFileResponse { deleted: true })
    }
}
