//! Messaging effect handler for the `messaging.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::messaging`.

use crate::effects::{
    dispatch_messaging_effect, EffectError, EffectHandler, EffectResult, MessagingEffects,
};
use crate::services::messaging::MessagingService;
use async_trait::async_trait;
use exomonad_proto::effects::messaging::*;

/// Messaging effect handler.
///
/// Handles `messaging.send_note` and `messaging.send_question` effects
/// by reading/writing JSONL mailbox files in the agent's worktree.
pub struct MessagingHandler {
    service: MessagingService,
}

impl MessagingHandler {
    pub fn new() -> Self {
        Self {
            service: MessagingService::from_cwd(),
        }
    }
}

impl Default for MessagingHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl EffectHandler for MessagingHandler {
    fn namespace(&self) -> &str {
        "messaging"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_messaging_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl MessagingEffects for MessagingHandler {
    async fn send_note(&self, req: SendNoteRequest) -> EffectResult<SendNoteResponse> {
        self.service
            .send_note(&req.content)
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        Ok(SendNoteResponse { ack: true })
    }

    async fn send_question(
        &self,
        req: SendQuestionRequest,
    ) -> EffectResult<SendQuestionResponse> {
        let answer = self
            .service
            .send_question(&req.content, &req.context, 300, 5)
            .await
            .map_err(|e| EffectError::custom("messaging_error", e.to_string()))?;

        Ok(SendQuestionResponse { answer })
    }
}
