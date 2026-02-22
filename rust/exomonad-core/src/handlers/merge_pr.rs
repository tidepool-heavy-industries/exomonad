use crate::effects::{
    dispatch_merge_pr_effect, EffectResult, MergePrEffects, ResultExt,
};
use crate::services::git_worktree::GitWorktreeService;
use crate::services::merge_pr;
use async_trait::async_trait;
use exomonad_proto::effects::merge_pr::*;
use std::sync::Arc;

pub struct MergePRHandler {
    git_wt: Arc<GitWorktreeService>,
}

impl MergePRHandler {
    pub fn new(git_wt: Arc<GitWorktreeService>) -> Self {
        Self { git_wt }
    }
}

crate::impl_pass_through_handler!(MergePRHandler, "merge_pr", dispatch_merge_pr_effect);

#[async_trait]
impl MergePrEffects for MergePRHandler {
    async fn merge_pr(
        &self,
        req: MergePrRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<MergePrResponse> {
        let pr_number = crate::domain::PRNumber::new(req.pr_number as u64);
        tracing::info!(pr_number = pr_number.as_u64(), strategy = %req.strategy, "[MergePR] merge_pr starting");
        let result =
            merge_pr::merge_pr_async(pr_number, &req.strategy, &req.working_dir, self.git_wt.clone())
                .await
                .effect_err("merge_pr")?;
        tracing::info!(success = result.success, git_fetched = result.git_fetched, "[MergePR] merge_pr complete");
        Ok(MergePrResponse {
            success: result.success,
            message: result.message,
            jj_fetched: result.git_fetched,
        })
    }
}
