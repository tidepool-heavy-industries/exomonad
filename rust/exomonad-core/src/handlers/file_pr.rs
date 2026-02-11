//! File PR effect handler for the `file_pr.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::file_pr`.

use crate::effects::{
    dispatch_file_pr_effect, EffectError, EffectHandler, EffectResult, FilePrEffects,
};
use crate::services::file_pr::{self, FilePRInput};
use async_trait::async_trait;
use exomonad_proto::effects::file_pr::*;

/// File PR effect handler.
///
/// Handles all effects in the `file_pr.*` namespace by delegating to
/// the generated `dispatch_file_pr_effect` function.
pub struct FilePRHandler;

impl FilePRHandler {
    pub fn new() -> Self {
        Self
    }
}

impl Default for FilePRHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl EffectHandler for FilePRHandler {
    fn namespace(&self) -> &str {
        "file_pr"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_file_pr_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl FilePrEffects for FilePRHandler {
    async fn file_pr(&self, req: FilePrRequest) -> EffectResult<FilePrResponse> {
        let base_branch = if req.base_branch.is_empty() {
            None
        } else {
            Some(req.base_branch.clone())
        };

        let input = FilePRInput {
            title: req.title,
            body: req.body,
            base_branch,
        };

        let output = file_pr::file_pr_async(&input)
            .await
            .map_err(|e| EffectError::custom("file_pr_error", e.to_string()))?;

        Ok(FilePrResponse {
            pr_url: output.pr_url,
            pr_number: output.pr_number as i64,
            head_branch: output.head_branch,
            base_branch: output.base_branch,
            created: output.created,
        })
    }
}
