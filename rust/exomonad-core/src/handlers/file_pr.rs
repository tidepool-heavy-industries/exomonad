//! File PR effect handler for the `file_pr.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::file_pr`.

use crate::effects::{
    dispatch_file_pr_effect, EffectError, EffectHandler, EffectResult, FilePrEffects,
};
use crate::services::file_pr::{self, FilePRInput};
use crate::services::jj_workspace::JjWorkspaceService;
use async_trait::async_trait;
use exomonad_proto::effects::file_pr::*;
use std::sync::Arc;

/// File PR effect handler.
///
/// Handles all effects in the `file_pr.*` namespace by delegating to
/// the generated `dispatch_file_pr_effect` function.
pub struct FilePRHandler {
    jj: Arc<JjWorkspaceService>,
}

impl FilePRHandler {
    pub fn new(jj: Arc<JjWorkspaceService>) -> Self {
        Self { jj }
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
    async fn file_pr(
        &self,
        req: FilePrRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<FilePrResponse> {
        tracing::info!(title = %req.title, "[FilePR] file_pr starting");
        let base_branch = if req.base_branch.is_empty() {
            None
        } else {
            Some(req.base_branch.clone())
        };

        let working_dir = crate::services::agent_control::resolve_agent_working_dir(ctx);

        let input = FilePRInput {
            title: req.title,
            body: req.body,
            base_branch,
            working_dir: Some(working_dir.to_string_lossy().to_string()),
        };

        let output = file_pr::file_pr_async(&input, self.jj.clone())
            .await
            .map_err(|e| EffectError::custom("file_pr_error", e.to_string()))?;

        tracing::info!(pr_number = output.pr_number.as_u64(), created = output.created, "[FilePR] file_pr complete");
        Ok(FilePrResponse {
            pr_url: output.pr_url,
            pr_number: output.pr_number.as_u64() as i64,
            head_branch: output.head_branch,
            base_branch: output.base_branch,
            created: output.created,
        })
    }
}
