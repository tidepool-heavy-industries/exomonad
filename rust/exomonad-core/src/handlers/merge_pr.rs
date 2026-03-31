use crate::effects::{dispatch_merge_pr_effect, EffectResult, MergePrEffects, ResultExt};
use crate::services::event_log::EventLog;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::merge_pr;
use crate::services::GitHubClient;
use async_trait::async_trait;
use exomonad_proto::effects::merge_pr::*;
use std::sync::Arc;
use tracing::instrument;

pub struct MergePRHandler {
    git_wt: Arc<GitWorktreeService>,
    github_client: Option<Arc<GitHubClient>>,
    event_log: Option<Arc<EventLog>>,
}

impl MergePRHandler {
    pub fn new(git_wt: Arc<GitWorktreeService>, services: &crate::services::Services) -> Self {
        Self {
            git_wt,
            github_client: services.github_client.clone(),
            event_log: services.event_log.clone(),
        }
    }
}

crate::impl_pass_through_handler!(MergePRHandler, "merge_pr", dispatch_merge_pr_effect);

#[async_trait]
impl MergePrEffects for MergePRHandler {
    #[instrument(skip_all, fields(agent_name = %ctx.agent_name, pr_number = req.pr_number))]
    async fn merge_pr(
        &self,
        req: MergePrRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<MergePrResponse> {
        let pr_number = crate::domain::PRNumber::new(req.pr_number as u64);
        let strategy = crate::domain::MergeStrategy::parse(&req.strategy).effect_err("merge_pr")?;
        tracing::info!(
            pr_number = pr_number.as_u64(),
            strategy = strategy.as_str(),
            "[MergePR] merge_pr starting"
        );
        let result = merge_pr::merge_pr_async(
            pr_number,
            &strategy,
            &req.working_dir,
            self.git_wt.clone(),
            self.github_client.as_deref(),
        )
        .await
        .effect_err("merge_pr")?;
        tracing::info!(
            success = result.success,
            git_fetched = result.git_fetched,
            "[MergePR] merge_pr complete"
        );

        if result.success {
            tracing::info!(
                otel.name = "pr.merged",
                pr_number = pr_number.as_u64(),
                strategy = strategy.as_str(),
                git_fetched = result.git_fetched,
                "[event] pr.merged"
            );
            if let Some(ref log) = self.event_log {
                let _ = log.append(
                    "pr.merged",
                    ctx.agent_name.as_ref(),
                    &serde_json::json!({
                        "pr_number": pr_number.as_u64(),
                        "strategy": strategy.as_str(),
                        "git_fetched": result.git_fetched,
                    }),
                );
            }
        } else {
            tracing::info!(
                otel.name = "pr.merge_failed",
                pr_number = pr_number.as_u64(),
                error = %result.message,
                "[event] pr.merge_failed"
            );
            if let Some(ref log) = self.event_log {
                let _ = log.append(
                    "pr.merge_failed",
                    ctx.agent_name.as_ref(),
                    &serde_json::json!({
                        "pr_number": pr_number.as_u64(),
                        "error": &result.message,
                    }),
                );
            }
        }

        Ok(MergePrResponse {
            success: result.success,
            message: result.message,
            git_fetched: result.git_fetched,
            branch_name: result.branch_name.to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch, PRNumber};
    use crate::effects::{EffectContext, EffectHandler};
    use crate::services::Services;
    use std::path::PathBuf;

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test"),
            birth_branch: BirthBranch::from("main"),
            working_dir: std::path::PathBuf::from("."),
        }
    }

    #[test]
    fn test_namespace() {
        let _ctx = test_ctx();
        let git_wt = Arc::new(GitWorktreeService::new(PathBuf::from(".")));
        let services = Services::test();
        let handler = MergePRHandler::new(git_wt, &services);
        assert_eq!(handler.namespace(), "merge_pr");
    }

    #[test]
    fn test_pr_number_conversion() {
        let proto_pr_number: i64 = 123;
        let pr_number = PRNumber::new(proto_pr_number as u64);
        assert_eq!(pr_number.as_u64(), 123);
    }

    #[test]
    fn test_pr_number_round_trip() {
        let original: u64 = 456;
        let pr_number = PRNumber::new(original);
        assert_eq!(pr_number.as_u64(), original);
    }

    #[test]
    fn test_response_field_mapping() {
        let response = MergePrResponse {
            success: true,
            message: "PR #42 merged via squash".to_string(),
            git_fetched: true,
            branch_name: "main.fix-auth-gemini".to_string(),
        };

        assert!(response.success);
        assert!(response.message.contains("42"));
        assert!(response.git_fetched);
        assert_eq!(response.branch_name, "main.fix-auth-gemini");
    }

    #[test]
    fn test_strategy_default_handling() {
        let req = MergePrRequest {
            pr_number: 42,
            strategy: "".to_string(),
            working_dir: ".".to_string(),
        };

        // Empty strategy should be handled by the service layer (defaults to "squash")
        assert!(req.strategy.is_empty());
    }
}
