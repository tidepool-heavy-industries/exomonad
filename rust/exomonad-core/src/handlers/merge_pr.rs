use crate::effects::{
    dispatch_merge_pr_effect, EffectError, EffectHandler, EffectResult, MergePrEffects,
};
use crate::services::jj_workspace::JjWorkspaceService;
use crate::services::merge_pr;
use async_trait::async_trait;
use exomonad_proto::effects::merge_pr::*;
use std::sync::Arc;

pub struct MergePRHandler {
    jj: Arc<JjWorkspaceService>,
}

impl MergePRHandler {
    pub fn new(jj: Arc<JjWorkspaceService>) -> Self {
        Self { jj }
    }
}

#[async_trait]
impl EffectHandler for MergePRHandler {
    fn namespace(&self) -> &str {
        "merge_pr"
    }
    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_merge_pr_effect(self, effect_type, payload, ctx).await
    }
}

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
            merge_pr::merge_pr_async(pr_number, &req.strategy, &req.working_dir, self.jj.clone())
                .await
                .map_err(|e| EffectError::custom("merge_pr_error", e.to_string()))?;
        tracing::info!(success = result.success, jj_fetched = result.jj_fetched, "[MergePR] merge_pr complete");
        Ok(MergePrResponse {
            success: result.success,
            message: result.message,
            jj_fetched: result.jj_fetched,
        })
    }
}
