use crate::effects::{
    dispatch_merge_pr_effect, EffectError, EffectHandler, EffectResult, MergePrEffects,
};
use crate::services::merge_pr;
use async_trait::async_trait;
use exomonad_proto::effects::merge_pr::*;

#[derive(Default)]
pub struct MergePRHandler;

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
        let result = merge_pr::merge_pr_async(req.pr_number, &req.strategy, &req.working_dir)
            .await
            .map_err(|e| EffectError::custom("merge_pr_error", e.to_string()))?;
        Ok(MergePrResponse {
            success: result.success,
            message: result.message,
            jj_fetched: result.jj_fetched,
        })
    }
}
