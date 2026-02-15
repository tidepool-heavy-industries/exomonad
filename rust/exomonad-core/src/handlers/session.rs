//! Session effect handler for the `session.*` namespace.
//!
//! Stores Claude Code session UUIDs so spawn_subtree can use --resume --fork-session.

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
        let key = if agent_id.is_empty() {
            "root".to_string()
        } else {
            agent_id.clone()
        };

        info!(
            key = %key,
            claude_session_id = %req.claude_session_id,
            "Registering Claude session via effect"
        );

        self.registry.register(&key, &req.claude_session_id).await;

        // Also store under slug variant (strip -claude suffix) for broader lookup
        if let Some(slug) = key.strip_suffix("-claude") {
            self.registry.register(slug, &req.claude_session_id).await;
        }

        Ok(RegisterClaudeSessionResponse { success: true })
    }
}
