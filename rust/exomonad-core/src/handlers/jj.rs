//! Jujutsu (jj) effect handler for the `jj.*` namespace.
//!
//! Hybrid approach: `bookmark_create`, `git_push`, and `git_fetch` use jj-lib
//! directly via `JjWorkspaceService`. Remaining effects (`log`, `new`, `status`)
//! shell out to the `jj` CLI.

use crate::domain::{BranchName, Revision};
use crate::effects::{dispatch_jj_effect, EffectError, EffectHandler, EffectResult, JjEffects};
use crate::services::jj_workspace::JjWorkspaceService;
use async_trait::async_trait;
use exomonad_proto::effects::jj::*;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::process::Command;
use tracing::info;

pub struct JjHandler {
    jj: Arc<JjWorkspaceService>,
}

impl JjHandler {
    pub fn new(jj: Arc<JjWorkspaceService>) -> Self {
        Self { jj }
    }
}

#[async_trait]
impl EffectHandler for JjHandler {
    fn namespace(&self) -> &str {
        "jj"
    }

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_jj_effect(self, effect_type, payload, ctx).await
    }
}

#[async_trait]
impl JjEffects for JjHandler {
    async fn bookmark_create(
        &self,
        req: BookmarkCreateRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<BookmarkCreateResponse> {
        let working_dir = if req.working_dir.is_empty() {
            PathBuf::from(".")
        } else {
            PathBuf::from(&req.working_dir)
        };
        let name = BranchName::from(req.name.as_str());
        let revision = if req.revision.is_empty() {
            None
        } else {
            Some(Revision::from(req.revision.as_str()))
        };
        info!(name = %name, revision = ?revision, "jj bookmark create via jj-lib");
        let jj = self.jj.clone();
        tokio::task::spawn_blocking(move || {
            jj.create_bookmark(&working_dir, &name, revision.as_ref())
        })
        .await
        .map_err(|e| EffectError::custom("jj_error", format!("spawn_blocking: {}", e)))?
        .map_err(|e| EffectError::custom("jj_error", e.to_string()))?;
        Ok(BookmarkCreateResponse {
            change_id: String::new(),
        })
    }

    async fn git_push(
        &self,
        req: GitPushRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GitPushResponse> {
        let working_dir = if req.working_dir.is_empty() {
            PathBuf::from(".")
        } else {
            PathBuf::from(&req.working_dir)
        };
        let bookmark = BranchName::from(req.bookmark.as_str());
        info!(bookmark = %bookmark, "jj git push via jj-lib");
        let jj = self.jj.clone();
        let result = tokio::task::spawn_blocking(move || jj.push_bookmark(&working_dir, &bookmark))
            .await
            .map_err(|e| EffectError::custom("jj_error", format!("spawn_blocking: {}", e)))?;
        match result {
            Ok(()) => Ok(GitPushResponse {
                success: true,
                message: String::new(),
            }),
            Err(e) => Err(EffectError::custom("jj_error", e.to_string())),
        }
    }

    async fn git_fetch(
        &self,
        req: GitFetchRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<GitFetchResponse> {
        let working_dir = if req.working_dir.is_empty() {
            PathBuf::from(".")
        } else {
            PathBuf::from(&req.working_dir)
        };
        info!("jj git fetch via jj-lib");
        let jj = self.jj.clone();
        let result = tokio::task::spawn_blocking(move || jj.fetch(&working_dir))
            .await
            .map_err(|e| EffectError::custom("jj_error", format!("spawn_blocking: {}", e)))?;
        match result {
            Ok(()) => Ok(GitFetchResponse {
                success: true,
                message: String::new(),
            }),
            Err(e) => Err(EffectError::custom("jj_error", e.to_string())),
        }
    }

    async fn log(
        &self,
        req: LogRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<LogResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };
        let revset = if req.revset.is_empty() {
            " @".to_string()
        } else {
            req.revset
        };
        info!(revset = %revset, "jj log");
        // Use jj log with machine-readable template
        let template = r###"change_id ++ "	" ++ commit_id ++ "	" ++ description.first_line() ++ "	" ++ author.name() ++ "	" ++ parents.map(|p| p.commit_id()).join(",") ++ "	" ++ conflict ++ "	" ++ empty ++ "
"###;
        let output = Command::new("jj")
            .args(["log", "--no-graph", "-r", &revset, "-T", template])
            .current_dir(&working_dir)
            .output()
            .await
            .map_err(|e| EffectError::custom("jj_error", e.to_string()))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(EffectError::custom("jj_error", stderr.to_string()));
        }
        let stdout = String::from_utf8_lossy(&output.stdout);
        let entries: Vec<LogEntry> = stdout
            .lines()
            .filter(|l| !l.is_empty())
            .map(|line| {
                let parts: Vec<&str> = line.splitn(7, '\t').collect();
                LogEntry {
                    change_id: parts.first().unwrap_or(&"").to_string(),
                    commit_id: parts.get(1).unwrap_or(&"").to_string(),
                    description: parts.get(2).unwrap_or(&"").to_string(),
                    author: parts.get(3).unwrap_or(&"").to_string(),
                    parents: parts
                        .get(4)
                        .map(|p| p.split(',').map(|s| s.to_string()).collect())
                        .unwrap_or_default(),
                    is_conflicted: parts.get(5).unwrap_or(&"") == &"true",
                    is_empty: parts.get(6).unwrap_or(&"") == &"true",
                }
            })
            .collect();
        Ok(LogResponse { entries })
    }

    async fn new(
        &self,
        req: NewRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<NewResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };
        let mut args = vec!["new"];
        if !req.revision.is_empty() {
            args.push(&req.revision);
        }
        info!("jj new");
        let output = Command::new("jj")
            .args(&args)
            .current_dir(&working_dir)
            .output()
            .await
            .map_err(|e| EffectError::custom("jj_error", e.to_string()))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(EffectError::custom("jj_error", stderr.to_string()));
        }
        Ok(NewResponse {
            change_id: String::new(),
        })
    }

    async fn status(
        &self,
        req: StatusRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<StatusResponse> {
        let working_dir = if req.working_dir.is_empty() {
            ".".to_string()
        } else {
            req.working_dir
        };
        info!("jj status");
        // Use jj status to get working copy info
        let output = Command::new("jj")
            .args(["status"])
            .current_dir(&working_dir)
            .output()
            .await
            .map_err(|e| EffectError::custom("jj_error", e.to_string()))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(EffectError::custom("jj_error", stderr.to_string()));
        }
        let stdout = String::from_utf8_lossy(&output.stdout);
        let is_conflicted = stdout.contains("conflict");
        let modified_files: Vec<String> = stdout
            .lines()
            .filter(|l| l.starts_with('M') || l.starts_with('A') || l.starts_with('D'))
            .map(|l| l[2..].trim().to_string())
            .collect();
        // Get change_id from jj log @
        let id_output = Command::new("jj")
            .args(["log", "--no-graph", "-r", " @", "-T", "change_id"])
            .current_dir(&working_dir)
            .output()
            .await
            .map_err(|e| EffectError::custom("jj_error", e.to_string()))?;
        let change_id = String::from_utf8_lossy(&id_output.stdout)
            .trim()
            .to_string();
        Ok(StatusResponse {
            change_id,
            is_conflicted,
            modified_files,
        })
    }
}
