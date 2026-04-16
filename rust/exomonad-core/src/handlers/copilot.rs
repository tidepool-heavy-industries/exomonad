//! Copilot effect handler for the `copilot.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::copilot`.

use crate::effects::{
    dispatch_copilot_effect, CopilotEffects, EffectHandler, EffectResult, ResultExt,
};
use crate::services::copilot_review;
use crate::services::HasTmuxIpc;
use async_trait::async_trait;
use exomonad_proto::effects::copilot::*;
use std::sync::Arc;

/// Copilot effect handler.
///
/// Handles all effects in the `copilot.*` namespace by delegating to
/// the generated `dispatch_copilot_effect` function.
pub struct CopilotHandler<C> {
    ctx: Arc<C>,
}

impl<C: HasTmuxIpc + 'static> CopilotHandler<C> {
    pub fn new(ctx: Arc<C>) -> Self {
        Self { ctx }
    }
}

#[async_trait]
impl<C: HasTmuxIpc + 'static> EffectHandler for CopilotHandler<C> {
    fn namespace(&self) -> &str {
        "copilot"
    }

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_copilot_effect(self, effect_type, payload, ctx).await
    }
}

#[async_trait]
impl<C: HasTmuxIpc + 'static> CopilotEffects for CopilotHandler<C> {
    async fn wait_for_copilot_review(
        &self,
        req: WaitForCopilotReviewRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<WaitForCopilotReviewResponse> {
        let pr_number = crate::domain::PRNumber::new(req.pr_number as u64);
        tracing::info!(
            pr_number = pr_number.as_u64(),
            timeout_secs = req.timeout_secs,
            "[Copilot] wait_for_copilot_review starting"
        );
        let input = copilot_review::WaitForCopilotReviewInput {
            pr_number,
            timeout_secs: if req.timeout_secs <= 0 {
                300
            } else {
                req.timeout_secs as u64
            },
            poll_interval_secs: if req.poll_interval_secs <= 0 {
                30
            } else {
                req.poll_interval_secs as u64
            },
        };

        let output = copilot_review::wait_for_copilot_review(&input, self.ctx.tmux_ipc())
            .await
            .effect_err("copilot")?;

        let comments: Vec<CopilotComment> = output
            .comments
            .into_iter()
            .map(|c| CopilotComment {
                path: c.path,
                line: c.line.unwrap_or(0) as i64,
                body: c.body,
                diff_hunk: c.diff_hunk.unwrap_or_default(),
            })
            .collect();

        tracing::info!(status = %output.status, comment_count = comments.len(), "[Copilot] wait_for_copilot_review complete");
        Ok(WaitForCopilotReviewResponse {
            status: output.status,
            comments,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copilot_handler_new() {
        let services = std::sync::Arc::new(crate::services::Services::test());
        let handler = CopilotHandler::new(services);
        assert_eq!(handler.namespace(), "copilot");
    }

    #[test]
    fn test_copilot_handler_namespace() {
        let services = std::sync::Arc::new(crate::services::Services::test());
        let handler = CopilotHandler::new(services);
        assert_eq!(handler.namespace(), "copilot");
    }
}
