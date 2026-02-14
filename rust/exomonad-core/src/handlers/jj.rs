//! Jujutsu (jj) effect handler for the `jj.*` namespace.
//!
//! Calls jj CLI as subprocess. Requires jj to be installed.

use crate::effects::{dispatch_jj_effect, EffectError, EffectHandler, EffectResult, JjEffects};
use async_trait::async_trait;
use exomonad_proto::effects::jj::*;
use tokio::process::Command;
use tracing::{info, error};

pub struct JjHandler;

impl JjHandler {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl EffectHandler for JjHandler {
    fn namespace(&self) -> &str {
        "jj"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_jj_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl JjEffects for JjHandler {
    async fn bookmark_create(&self, req: BookmarkCreateRequest) -> EffectResult<BookmarkCreateResponse> {
        let working_dir = if req.working_dir.is_empty() { ".".to_string() } else { req.working_dir };
        let mut args = vec!["bookmark", "create", &req.name];
        if !req.revision.is_empty() {
            args.extend(["--revision", &req.revision]);
        }
        info!(name = %req.name, "jj bookmark create");
        let output = Command::new("jj")
            .args(&args)
            .current_dir(&working_dir)
            .output()
            .await
            .map_err(|e| EffectError::custom("jj_error", e.to_string()))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            error!(stderr = %stderr, "jj bookmark create failed");
            return Err(EffectError::custom("jj_error", stderr.to_string()));
        }
        Ok(BookmarkCreateResponse { change_id: String::new() })
    }

    async fn git_push(&self, req: GitPushRequest) -> EffectResult<GitPushResponse> {
        let working_dir = if req.working_dir.is_empty() { ".".to_string() } else { req.working_dir };
        info!(bookmark = %req.bookmark, "jj git push");
        let output = Command::new("jj")
            .args(["git", "push", "--bookmark", &req.bookmark])
            .current_dir(&working_dir)
            .output()
            .await
            .map_err(|e| EffectError::custom("jj_error", e.to_string()))?;
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        Ok(GitPushResponse {
            success: output.status.success(),
            message: if output.status.success() { String::new() } else { stderr },
        })
    }

    async fn git_fetch(&self, req: GitFetchRequest) -> EffectResult<GitFetchResponse> {
        let working_dir = if req.working_dir.is_empty() { ".".to_string() } else { req.working_dir };
        info!("jj git fetch");
        let output = Command::new("jj")
            .args(["git", "fetch"])
            .current_dir(&working_dir)
            .output()
            .await
            .map_err(|e| EffectError::custom("jj_error", e.to_string()))?;
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        Ok(GitFetchResponse {
            success: output.status.success(),
            message: if output.status.success() { String::new() } else { stderr },
        })
    }

    async fn log(&self, req: LogRequest) -> EffectResult<LogResponse> {
        let working_dir = if req.working_dir.is_empty() { ".".to_string() } else { req.working_dir };
        let revset = if req.revset.is_empty() { " @".to_string() } else { req.revset };
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
                    parents: parts.get(4).map(|p| p.split(',').map(|s| s.to_string()).collect()).unwrap_or_default(),
                    is_conflicted: parts.get(5).unwrap_or(&"") == &"true",
                    is_empty: parts.get(6).unwrap_or(&"") == &"true",
                }
            })
            .collect();
        Ok(LogResponse { entries })
    }

    async fn new(&self, req: NewRequest) -> EffectResult<NewResponse> {
        let working_dir = if req.working_dir.is_empty() { ".".to_string() } else { req.working_dir };
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
        Ok(NewResponse { change_id: String::new() })
    }

    async fn status(&self, req: StatusRequest) -> EffectResult<StatusResponse> {
        let working_dir = if req.working_dir.is_empty() { ".".to_string() } else { req.working_dir };
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
        let change_id = String::from_utf8_lossy(&id_output.stdout).trim().to_string();
        Ok(StatusResponse {
            change_id,
            is_conflicted,
            modified_files,
        })
    }
}
