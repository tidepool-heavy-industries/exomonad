//! Filesystem effect handler for the `fs.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::fs`.

use crate::effects::{dispatch_fs_effect, EffectError, EffectResult, FilesystemEffects, ResultExt};
use crate::services::filesystem::FileSystemService;
use crate::services::HasProjectDir;
use async_trait::async_trait;
use exomonad_proto::effects::fs::*;

/// Filesystem effect handler.
///
/// Handles all effects in the `fs.*` namespace by delegating to
/// the generated `dispatch_filesystem_effect` function.
pub struct FsHandler {
    service: FileSystemService,
    project_dir: std::path::PathBuf,
}

impl FsHandler {
    pub fn new(ctx: &impl HasProjectDir) -> Self {
        let project_dir = ctx.project_dir().to_path_buf();
        let service = FileSystemService::new(project_dir.clone());
        Self {
            service,
            project_dir,
        }
    }
}

crate::impl_pass_through_handler!(FsHandler, "fs", dispatch_fs_effect);

fn resolve_path(
    project_dir: &std::path::Path,
    working_dir: &std::path::Path,
    user_path: &str,
) -> EffectResult<std::path::PathBuf> {
    if user_path.contains('\0') {
        return Err(EffectError::custom("fs_path_invalid", "null byte in path"));
    }
    let p = std::path::Path::new(user_path);
    for component in p.components() {
        match component {
            std::path::Component::ParentDir => {
                return Err(EffectError::custom(
                    "fs_path_escape",
                    format!("parent-dir component rejected: {}", user_path),
                ));
            }
            std::path::Component::RootDir | std::path::Component::Prefix(_) => {
                return Err(EffectError::custom(
                    "fs_path_absolute",
                    format!("absolute path rejected: {}", user_path),
                ));
            }
            _ => {}
        }
    }
    Ok(project_dir.join(working_dir).join(user_path))
}

#[async_trait]
impl FilesystemEffects for FsHandler {
    async fn read_file(
        &self,
        req: ReadFileRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ReadFileResponse> {
        tracing::info!(path = %req.path, "[Fs] read_file starting");
        let path = resolve_path(&self.project_dir, &ctx.working_dir, &req.path)?;

        let max_bytes = if req.max_bytes <= 0 {
            1_048_576 // 1MB default
        } else {
            req.max_bytes as usize
        };

        let input = crate::services::filesystem::ReadFileInput {
            path: path.to_string_lossy().to_string(),
            max_bytes,
        };

        let result = self.service.read_file(&input).await.effect_err("fs")?;

        tracing::info!(
            bytes_read = result.bytes_read,
            truncated = result.truncated,
            "[Fs] read_file complete"
        );
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
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<WriteFileResponse> {
        tracing::info!(path = %req.path, content_bytes = req.content.len(), "[Fs] write_file starting");
        let path = resolve_path(&self.project_dir, &ctx.working_dir, &req.path)?;

        let input = crate::services::filesystem::WriteFileInput {
            path: path.to_string_lossy().to_string(),
            content: req.content,
            create_parents: req.create_parents,
        };

        let result = self.service.write_file(&input).await.effect_err("fs")?;

        tracing::info!(
            bytes_written = result.bytes_written,
            "[Fs] write_file complete"
        );
        Ok(WriteFileResponse {
            bytes_written: result.bytes_written as i64,
            path: result.path,
        })
    }

    async fn file_exists(
        &self,
        req: FileExistsRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<FileExistsResponse> {
        tracing::info!(path = %req.path, "[Fs] file_exists starting");
        let path = resolve_path(&self.project_dir, &ctx.working_dir, &req.path)?;
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
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<ListDirectoryResponse> {
        tracing::info!(path = %req.path, "[Fs] list_directory starting");
        let path = resolve_path(&self.project_dir, &ctx.working_dir, &req.path)?;
        if !path.is_dir() {
            return Err(EffectError::not_found(format!(
                "Directory not found: {}",
                req.path
            )));
        }

        let mut entries = Vec::new();
        let mut read_dir = tokio::fs::read_dir(&path).await.effect_err("fs")?;

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
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<DeleteFileResponse> {
        tracing::info!(path = %req.path, recursive = req.recursive, "[Fs] delete_file starting");
        let path = resolve_path(&self.project_dir, &ctx.working_dir, &req.path)?;
        if !path.exists() {
            return Ok(DeleteFileResponse { deleted: false });
        }

        if path.is_dir() {
            if req.recursive {
                tokio::fs::remove_dir_all(&path).await.effect_err("fs")?;
            } else {
                tokio::fs::remove_dir(&path).await.effect_err("fs")?;
            }
        } else {
            tokio::fs::remove_file(&path).await.effect_err("fs")?;
        }

        tracing::info!(path = %req.path, "[Fs] delete_file complete");
        Ok(DeleteFileResponse { deleted: true })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch};
    use crate::effects::{EffectContext, EffectHandler};
    use std::path::{Path, PathBuf};
    use tempfile::tempdir;

    struct TestCtx {
        project_dir: PathBuf,
    }

    impl HasProjectDir for TestCtx {
        fn project_dir(&self) -> &Path {
            &self.project_dir
        }
    }

    fn make_handler(dir: &Path) -> FsHandler {
        FsHandler::new(&TestCtx {
            project_dir: dir.to_path_buf(),
        })
    }

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test"),
            birth_branch: BirthBranch::from("main"),
            working_dir: PathBuf::from("."),
        }
    }

    #[test]
    fn test_fs_handler_new() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        assert_eq!(handler.namespace(), "fs");
    }

    #[tokio::test]
    async fn test_read_file() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let filename = "hello.txt";
        std::fs::write(dir.path().join(filename), "hello world").unwrap();

        let resp = handler
            .read_file(
                ReadFileRequest {
                    path: filename.to_string(),
                    max_bytes: 0,
                    offset: 0,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert_eq!(resp.content, "hello world");
        assert_eq!(resp.bytes_read, 11);
        assert!(!resp.truncated);
    }

    #[tokio::test]
    async fn test_read_file_truncated() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let filename = "big.txt";
        std::fs::write(dir.path().join(filename), "abcdefghij").unwrap();

        let resp = handler
            .read_file(
                ReadFileRequest {
                    path: filename.to_string(),
                    max_bytes: 5,
                    offset: 0,
                },
                &ctx,
            )
            .await
            .unwrap();
        // bytes_read reflects original file size, not truncated size
        assert_eq!(resp.bytes_read, 10);
        assert!(resp.truncated);
        assert_eq!(resp.content.len(), 5);
    }

    #[tokio::test]
    async fn test_read_file_nonexistent() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let result = handler
            .read_file(
                ReadFileRequest {
                    path: "nope.txt".to_string(),
                    max_bytes: 0,
                    offset: 0,
                },
                &ctx,
            )
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_write_file() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let filename = "out.txt";
        let resp = handler
            .write_file(
                WriteFileRequest {
                    path: filename.to_string(),
                    content: "written".into(),
                    create_parents: false,
                    append: false,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert_eq!(resp.bytes_written, 7);
        assert_eq!(
            std::fs::read_to_string(dir.path().join(filename)).unwrap(),
            "written"
        );
    }

    #[tokio::test]
    async fn test_write_file_create_parents() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let rel_path = "a/b/c.txt";
        let resp = handler
            .write_file(
                WriteFileRequest {
                    path: rel_path.to_string(),
                    content: "nested".into(),
                    create_parents: true,
                    append: false,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert_eq!(resp.bytes_written, 6);
        assert_eq!(
            std::fs::read_to_string(dir.path().join(rel_path)).unwrap(),
            "nested"
        );
    }

    #[tokio::test]
    async fn test_file_exists() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let filename = "exists.txt";
        std::fs::write(dir.path().join(filename), "hello").unwrap();

        let resp = handler
            .file_exists(
                FileExistsRequest {
                    path: filename.to_string(),
                },
                &ctx,
            )
            .await
            .unwrap();
        assert!(resp.exists);
        assert!(resp.is_file);
        assert!(!resp.is_directory);

        let resp_none = handler
            .file_exists(
                FileExistsRequest {
                    path: "none".to_string(),
                },
                &ctx,
            )
            .await
            .unwrap();
        assert!(!resp_none.exists);
    }

    #[tokio::test]
    async fn test_file_exists_directory() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let sub = "subdir";
        std::fs::create_dir(dir.path().join(sub)).unwrap();

        let resp = handler
            .file_exists(
                FileExistsRequest {
                    path: sub.to_string(),
                },
                &ctx,
            )
            .await
            .unwrap();
        assert!(resp.exists);
        assert!(!resp.is_file);
        assert!(resp.is_directory);
    }


    #[tokio::test]
    async fn test_list_directory() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        std::fs::write(dir.path().join("a.txt"), "a").unwrap();
        std::fs::create_dir(dir.path().join("subdir")).unwrap();

        let req = ListDirectoryRequest {
            path: ".".to_string(),
            include_hidden: false,
            include_metadata: false,
        };
        let resp = handler.list_directory(req, &ctx).await.unwrap();
        assert_eq!(resp.count, 2);
        let names: std::collections::HashSet<_> =
            resp.entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains("a.txt"));
        assert!(names.contains("subdir"));
    }

    #[tokio::test]
    async fn test_list_directory_hidden_files() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        std::fs::write(dir.path().join("visible.txt"), "").unwrap();
        std::fs::write(dir.path().join(".hidden"), "").unwrap();

        // Without include_hidden
        let resp = handler
            .list_directory(
                ListDirectoryRequest {
                    path: ".".to_string(),
                    include_hidden: false,
                    include_metadata: false,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert_eq!(resp.count, 1);
        assert_eq!(resp.entries[0].name, "visible.txt");

        // With include_hidden
        let resp = handler
            .list_directory(
                ListDirectoryRequest {
                    path: ".".to_string(),
                    include_hidden: true,
                    include_metadata: false,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert_eq!(resp.count, 2);
    }

    #[tokio::test]
    async fn test_list_directory_with_metadata() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        std::fs::write(dir.path().join("file.txt"), "12345").unwrap();

        let resp = handler
            .list_directory(
                ListDirectoryRequest {
                    path: ".".to_string(),
                    include_hidden: false,
                    include_metadata: true,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert_eq!(resp.count, 1);
        assert_eq!(resp.entries[0].size, 5);
        assert!(resp.entries[0].modified_at > 0);
    }

    #[tokio::test]
    async fn test_list_directory_nonexistent() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let result = handler
            .list_directory(
                ListDirectoryRequest {
                    path: "nope".to_string(),
                    include_hidden: false,
                    include_metadata: false,
                },
                &ctx,
            )
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_delete_file() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let filename = "delete_me.txt";
        let file_path = dir.path().join(filename);
        std::fs::write(&file_path, "bye").unwrap();

        let resp = handler
            .delete_file(
                DeleteFileRequest {
                    path: filename.to_string(),
                    recursive: false,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert!(resp.deleted);
        assert!(!file_path.exists());
    }

    #[tokio::test]
    async fn test_delete_nonexistent() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let resp = handler
            .delete_file(
                DeleteFileRequest {
                    path: "ghost.txt".to_string(),
                    recursive: false,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert!(!resp.deleted);
    }

    #[tokio::test]
    async fn test_delete_directory_recursive() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let sub_name = "to_delete";
        let sub = dir.path().join(sub_name);
        std::fs::create_dir(&sub).unwrap();
        std::fs::write(sub.join("inner.txt"), "").unwrap();

        let resp = handler
            .delete_file(
                DeleteFileRequest {
                    path: sub_name.to_string(),
                    recursive: true,
                },
                &ctx,
            )
            .await
            .unwrap();
        assert!(resp.deleted);
        assert!(!sub.exists());
    }

    #[test]
    fn test_resolve_path_rejects_null_byte() {
        let project_dir = PathBuf::from("/project");
        let working_dir = PathBuf::from("work");
        let result = resolve_path(&project_dir, &working_dir, "foo\0bar");
        assert!(result.is_err());
        let err = result.unwrap_err();
        if let EffectError::Custom { code, message, .. } = err {
            assert_eq!(code, "fs_path_invalid");
            assert!(message.contains("null"));
        } else {
            panic!("Expected custom error");
        }
    }

    #[test]
    fn test_resolve_path_rejects_parent_dir_component() {
        let project_dir = PathBuf::from("/project");
        let working_dir = PathBuf::from("work");
        assert!(resolve_path(&project_dir, &working_dir, "../secret").is_err());
        assert!(resolve_path(&project_dir, &working_dir, "a/../../etc/passwd").is_err());
        assert!(resolve_path(&project_dir, &working_dir, "a/../b").is_err());
    }

    #[test]
    fn test_resolve_path_rejects_absolute() {
        let project_dir = PathBuf::from("/project");
        let working_dir = PathBuf::from("work");
        // Unix absolute
        let result = resolve_path(&project_dir, &working_dir, "/etc/passwd");
        assert!(result.is_err());
        if let Err(EffectError::Custom { code, .. }) = result {
            assert_eq!(code, "fs_path_absolute");
        } else {
            panic!("Expected fs_path_absolute error");
        }
        // Windows absolute/prefix (if path parsing allows)
        // Note: On non-windows systems, C:\foo is a single Normal component.
        // But RootDir (/) is always a RootDir component.
    }

    #[test]
    fn test_resolve_path_allows_nested() {
        let project_dir = PathBuf::from("/project");
        let working_dir = PathBuf::from("work");
        let result = resolve_path(&project_dir, &working_dir, "subdir/file.txt").unwrap();
        assert_eq!(result, PathBuf::from("/project/work/subdir/file.txt"));
    }

    #[test]
    fn test_resolve_path_allows_dot_segments() {
        let project_dir = PathBuf::from("/project");
        let working_dir = PathBuf::from("work");
        let result = resolve_path(&project_dir, &working_dir, "./file.txt").unwrap();
        assert!(result.to_string_lossy().contains("file.txt"));
    }

    #[tokio::test]
    async fn test_read_file_rejects_traversal() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let result = handler
            .read_file(
                ReadFileRequest {
                    path: "../secret".to_string(),
                    max_bytes: 0,
                    offset: 0,
                },
                &ctx,
            )
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        if let EffectError::Custom { code, .. } = err {
            assert_eq!(code, "fs_path_escape");
        } else {
            panic!("Expected fs_path_escape error");
        }
    }

    #[tokio::test]
    async fn test_delete_file_rejects_traversal() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let result = handler
            .delete_file(
                DeleteFileRequest {
                    path: "../secret".to_string(),
                    recursive: false,
                },
                &ctx,
            )
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_list_directory_rejects_traversal() {
        let dir = tempdir().unwrap();
        let handler = make_handler(dir.path());
        let ctx = test_ctx();

        let result = handler
            .list_directory(
                ListDirectoryRequest {
                    path: "../etc".to_string(),
                    include_hidden: false,
                    include_metadata: false,
                },
                &ctx,
            )
            .await;
        assert!(result.is_err());
    }
}
