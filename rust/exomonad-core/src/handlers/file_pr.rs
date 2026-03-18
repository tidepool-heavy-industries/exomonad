//! File PR effect handler for the `file_pr.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::file_pr`.

use super::non_empty;
use crate::domain::BranchName;
use crate::effects::{
    dispatch_file_pr_effect, EffectHandler, EffectResult, FilePrEffects, ResultExt,
};
use crate::services::event_log::EventLog;
use crate::services::file_pr::{self, FilePRInput};
use crate::services::git_worktree::GitWorktreeService;
use async_trait::async_trait;
use tracing::instrument;
use exomonad_proto::effects::file_pr::*;
use std::sync::Arc;

/// File PR effect handler.
///
/// Handles all effects in the `file_pr.*` namespace by delegating to
/// the generated `dispatch_file_pr_effect` function.
pub struct FilePRHandler {
    git_wt: Arc<GitWorktreeService>,
    event_log: Option<Arc<EventLog>>,
}

impl FilePRHandler {
    pub fn new(git_wt: Arc<GitWorktreeService>) -> Self {
        Self {
            git_wt,
            event_log: None,
        }
    }

    pub fn with_event_log(mut self, log: Arc<EventLog>) -> Self {
        self.event_log = Some(log);
        self
    }
}

#[async_trait]
impl EffectHandler for FilePRHandler {
    fn namespace(&self) -> &str {
        "file_pr"
    }

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_file_pr_effect(self, effect_type, payload, ctx).await
    }
}

#[async_trait]
impl FilePrEffects for FilePRHandler {
    #[instrument(skip_all, fields(agent_name = %ctx.agent_name, pr_title = %req.title))]
    async fn file_pr(
        &self,
        req: FilePrRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<FilePrResponse> {
        tracing::info!(title = %req.title, "[FilePR] file_pr starting");
        let base_branch = non_empty(req.base_branch).map(|s| BranchName::from(s.as_str()));

        let working_dir = ctx.working_dir.clone();

        let input = FilePRInput {
            title: req.title,
            body: req.body,
            base_branch,
            working_dir: Some(working_dir.to_string_lossy().to_string()),
        };

        let output = file_pr::file_pr_async(&input, self.git_wt.clone())
            .await
            .effect_err("file_pr")?;

        tracing::info!(
            pr_number = output.pr_number.as_u64(),
            created = output.created,
            "[FilePR] file_pr complete"
        );

        let event_type = if output.created {
            "pr.filed"
        } else {
            "pr.updated"
        };

        tracing::info!(
            otel.name = event_type,
            pr_number = output.pr_number.as_u64(),
            pr_url = %output.pr_url,
            head_branch = %output.head_branch,
            base_branch = %output.base_branch,
            created = output.created,
            title = %input.title,
            "[event] {}",
            event_type
        );

        if let Some(ref log) = self.event_log {
            if let Err(e) = log.append(
                event_type,
                &ctx.agent_name.to_string(),
                &serde_json::json!({
                    "pr_number": output.pr_number.as_u64(),
                    "pr_url": output.pr_url,
                    "head_branch": output.head_branch.to_string(),
                    "base_branch": output.base_branch.to_string(),
                    "created": output.created,
                    "title": input.title,
                }),
            ) {
                tracing::warn!(error = %e, "Failed to write event log");
            }
        }

        Ok(FilePrResponse {
            pr_url: output.pr_url,
            pr_number: output.pr_number.as_u64() as i64,
            head_branch: output.head_branch.to_string(),
            base_branch: output.base_branch.to_string(),
            created: output.created,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch};
    use crate::effects::EffectContext;
    use std::path::PathBuf;

    fn test_ctx(branch: &str) -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test"),
            birth_branch: BirthBranch::from(branch),
            working_dir: crate::services::agent_control::resolve_working_dir(branch),
        }
    }

    #[test]
    fn test_namespace() {
        let git_wt = Arc::new(GitWorktreeService::new(PathBuf::from(".")));
        let handler = FilePRHandler::new(git_wt);
        assert_eq!(handler.namespace(), "file_pr");
    }

    #[test]
    fn test_resolve_working_dir_root() {
        let ctx = test_ctx("main");
        let working_dir = ctx.working_dir.clone();
        assert_eq!(working_dir, PathBuf::from("."));
    }

    #[test]
    fn test_resolve_working_dir_spawned() {
        let ctx = test_ctx("main.feature");
        let working_dir = ctx.working_dir.clone();
        assert_eq!(working_dir, PathBuf::from(".exo/worktrees/feature/"));
    }
}
