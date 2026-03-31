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
use crate::services::GitHubClient;
use async_trait::async_trait;
use exomonad_proto::effects::file_pr::*;
use std::sync::Arc;
use tracing::instrument;

/// File PR effect handler.
///
/// Handles all effects in the `file_pr.*` namespace by delegating to
/// the generated `dispatch_file_pr_effect` function.
pub struct FilePRHandler {
    git_wt: Arc<GitWorktreeService>,
    github_client: Option<Arc<GitHubClient>>,
    event_log: Option<Arc<EventLog>>,
}

impl FilePRHandler {
    pub fn new(git_wt: Arc<GitWorktreeService>, services: &crate::services::Services) -> Self {
        Self {
            git_wt,
            github_client: services.github_client.clone(),
            event_log: services.event_log.clone(),
        }
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

        let output =
            file_pr::file_pr_async(&input, self.git_wt.clone(), self.github_client.as_deref())
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
                ctx.agent_name.as_ref(),
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
    use crate::services::Services;
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
        let services = Services::test();
        let handler = FilePRHandler::new(git_wt, &services);
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

    #[test]
    fn test_base_branch_conversion_empty_is_none() {
        let base_branch = non_empty("".to_string()).map(|s| BranchName::from(s.as_str()));
        assert!(
            base_branch.is_none(),
            "Empty string should become None (auto-detect)"
        );
    }

    #[test]
    fn test_base_branch_conversion_explicit() {
        let base_branch = non_empty("develop".to_string()).map(|s| BranchName::from(s.as_str()));
        assert_eq!(base_branch.unwrap().to_string(), "develop");
    }

    #[test]
    fn test_response_field_conversion() {
        let pr_number = crate::domain::PRNumber::new(42);
        let head = BranchName::from("main.fix-auth-gemini");
        let base = BranchName::from("main");

        let response = FilePrResponse {
            pr_url: "https://github.com/owner/repo/pull/42".to_string(),
            pr_number: pr_number.as_u64() as i64,
            head_branch: head.to_string(),
            base_branch: base.to_string(),
            created: true,
        };

        assert_eq!(response.pr_number, 42);
        assert_eq!(response.head_branch, "main.fix-auth-gemini");
        assert_eq!(response.base_branch, "main");
        assert!(response.created);
    }
}
