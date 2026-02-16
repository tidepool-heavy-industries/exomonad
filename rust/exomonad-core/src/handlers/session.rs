//! Session effect handler for the `session.*` namespace.
//!
//! Stores Claude Code session UUIDs so spawn_subtree can use --resume --fork-session.

use crate::domain::ClaudeSessionUuid;
use crate::effects::{dispatch_session_effect, EffectHandler, EffectResult, SessionEffects};
use crate::services::claude_session_registry::ClaudeSessionRegistry;
use async_trait::async_trait;
use exomonad_proto::effects::session::*;
use std::sync::Arc;
use tracing::info;

/// Session effect handler.
pub struct SessionHandler {
    registry: Arc<ClaudeSessionRegistry>,
}

impl SessionHandler {
    pub fn new(registry: Arc<ClaudeSessionRegistry>) -> Self {
        Self { registry }
    }
}

#[async_trait]
impl EffectHandler for SessionHandler {
    fn namespace(&self) -> &str {
        "session"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_session_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl SessionEffects for SessionHandler {
    async fn register_claude_id(
        &self,
        req: RegisterClaudeSessionRequest,
    ) -> EffectResult<RegisterClaudeSessionResponse> {
        let agent_id = crate::mcp::agent_identity::get_agent_id();
        let key = if agent_id.as_str().is_empty() {
            "root".to_string()
        } else {
            agent_id.to_string()
        };

        let claude_uuid = ClaudeSessionUuid::try_from(req.claude_session_id.clone())
            .map_err(|e| crate::effects::EffectError::invalid_input(e.to_string()))?;

        info!(
            key = %key,
            claude_session_id = %claude_uuid,
            "Registering Claude session via effect"
        );

        self.registry.register(&key, claude_uuid.clone()).await;

        // Also store under slug variant (strip -claude suffix) for broader lookup
        if let Some(slug) = key.strip_suffix("-claude") {
            self.registry.register(slug, claude_uuid).await;
        }

        Ok(RegisterClaudeSessionResponse { success: true })
    }
}
