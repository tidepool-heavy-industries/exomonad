//! Session effect handler for the `session.*` namespace.
//!
//! Stores Claude Code session UUIDs so spawn_subtree can use --resume --fork-session.
//! Stores Claude Teams info so notify_parent can route via Teams inbox.

use crate::domain::ClaudeSessionUuid;
use crate::effects::{dispatch_session_effect, EffectResult, SessionEffects};
use crate::services::claude_session_registry::ClaudeSessionRegistry;
use crate::services::team_registry::{TeamInfo, TeamRegistry};
use async_trait::async_trait;
use exomonad_proto::effects::session::*;
use std::sync::Arc;
use tracing::info;

/// Session effect handler.
pub struct SessionHandler {
    registry: Arc<ClaudeSessionRegistry>,
    team_registry: Option<Arc<TeamRegistry>>,
}

impl SessionHandler {
    pub fn new(registry: Arc<ClaudeSessionRegistry>) -> Self {
        Self {
            registry,
            team_registry: None,
        }
    }

    pub fn with_team_registry(mut self, team_registry: Arc<TeamRegistry>) -> Self {
        self.team_registry = Some(team_registry);
        self
    }
}

crate::impl_pass_through_handler!(SessionHandler, "session", dispatch_session_effect);

#[async_trait]
impl SessionEffects for SessionHandler {
    async fn register_claude_id(
        &self,
        req: RegisterClaudeSessionRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<RegisterClaudeSessionResponse> {
        let agent_name = &ctx.agent_name;
        let key = if agent_name.as_str().is_empty() {
            "root".to_string()
        } else {
            agent_name.to_string()
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

    async fn register_team(
        &self,
        req: RegisterTeamRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<RegisterTeamResponse> {
        let agent_name = &ctx.agent_name;
        let key = if agent_name.as_str().is_empty() {
            "root".to_string()
        } else {
            agent_name.to_string()
        };

        info!(
            key = %key,
            team_name = %req.team_name,
            inbox_name = %req.inbox_name,
            "Registering Claude Teams info via effect"
        );

        if let Some(ref team_registry) = self.team_registry {
            team_registry
                .register(
                    &key,
                    TeamInfo {
                        team_name: req.team_name.clone(),
                        inbox_name: req.inbox_name.clone(),
                    },
                )
                .await;

            // Also store under slug variant for broader lookup
            if let Some(slug) = key.strip_suffix("-claude") {
                team_registry
                    .register(
                        slug,
                        TeamInfo {
                            team_name: req.team_name,
                            inbox_name: req.inbox_name,
                        },
                    )
                    .await;
            }
        } else {
            tracing::warn!(
                key = %key,
                "TeamRegistry not available; register_team is a no-op"
            );
        }

        Ok(RegisterTeamResponse { success: true })
    }
}
