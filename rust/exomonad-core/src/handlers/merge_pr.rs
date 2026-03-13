use crate::effects::{dispatch_merge_pr_effect, EffectResult, MergePrEffects, ResultExt};
use crate::services::event_log::EventLog;
use crate::services::git_worktree::GitWorktreeService;
use crate::services::merge_pr;
use async_trait::async_trait;
use exomonad_proto::effects::merge_pr::*;
use std::sync::Arc;

pub struct MergePRHandler {
    git_wt: Arc<GitWorktreeService>,
    event_log: Option<Arc<EventLog>>,
}

impl MergePRHandler {
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

crate::impl_pass_through_handler!(MergePRHandler, "merge_pr", dispatch_merge_pr_effect);

#[async_trait]
impl MergePrEffects for MergePRHandler {
    async fn merge_pr(
        &self,
        req: MergePrRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<MergePrResponse> {
        let pr_number = crate::domain::PRNumber::new(req.pr_number as u64);
        tracing::info!(pr_number = pr_number.as_u64(), strategy = %req.strategy, "[MergePR] merge_pr starting");
        let result = merge_pr::merge_pr_async(
            pr_number,
            &req.strategy,
            &req.working_dir,
            self.git_wt.clone(),
        )
        .await
        .effect_err("merge_pr")?;
        tracing::info!(
            success = result.success,
            git_fetched = result.git_fetched,
            "[MergePR] merge_pr complete"
        );

        if let Some(ref log) = self.event_log {
            if result.success {
                if let Err(e) = log.append(
                    "pr.merged",
                    &ctx.agent_name.to_string(),
                    &serde_json::json!({
                        "pr_number": pr_number.as_u64(),
                        "strategy": req.strategy,
                        "git_fetched": result.git_fetched,
                    }),
                ) {
                    tracing::warn!(error = %e, "Failed to write event log");
                }
            }
        }

        Ok(MergePrResponse {
            success: result.success,
            message: result.message,
            git_fetched: result.git_fetched,
            branch_name: result.branch_name,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch, PRNumber};
    use crate::effects::{EffectContext, EffectHandler};
    use std::path::PathBuf;

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test"),
            birth_branch: BirthBranch::from("main"),
        }
    }

    #[test]
    fn test_namespace() {
        let _ctx = test_ctx();
        let git_wt = Arc::new(GitWorktreeService::new(PathBuf::from(".")));
        let handler = MergePRHandler::new(git_wt);
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
}
