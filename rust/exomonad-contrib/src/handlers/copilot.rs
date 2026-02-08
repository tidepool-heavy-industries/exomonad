//! Copilot effect handler for the `copilot.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::copilot`.

use crate::services::copilot_review;
use async_trait::async_trait;
use exomonad_core::effects::{
    dispatch_copilot_effect, CopilotEffects, EffectError, EffectHandler, EffectResult,
};
use exomonad_proto::effects::copilot::*;

/// Copilot effect handler.
///
/// Handles all effects in the `copilot.*` namespace by delegating to
/// the generated `dispatch_copilot_effect` function.
pub struct CopilotHandler;

impl CopilotHandler {
    pub fn new() -> Self {
        Self
    }
}

impl Default for CopilotHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl EffectHandler for CopilotHandler {
    fn namespace(&self) -> &str {
        "copilot"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_copilot_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl CopilotEffects for CopilotHandler {
    async fn wait_for_copilot_review(
        &self,
        req: WaitForCopilotReviewRequest,
    ) -> EffectResult<WaitForCopilotReviewResponse> {
        let input = copilot_review::WaitForCopilotReviewInput {
            pr_number: req.pr_number as u64,
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

        let output = copilot_review::wait_for_copilot_review(&input)
            .map_err(|e| EffectError::custom("copilot_error", e.to_string()))?;

        let comments = output
            .comments
            .into_iter()
            .map(|c| CopilotComment {
                path: c.path,
                line: c.line.unwrap_or(0) as i64,
                body: c.body,
                diff_hunk: c.diff_hunk.unwrap_or_default(),
            })
            .collect();

        Ok(WaitForCopilotReviewResponse {
            status: output.status,
            comments,
        })
    }
}
