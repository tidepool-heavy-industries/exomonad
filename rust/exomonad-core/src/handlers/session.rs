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

        // Register in-memory only — Claude Code owns team directory lifecycle via TeamCreate.
        // SessionStart hook instructs Claude to call TeamCreate, which creates ~/.claude/teams/{name}/.
        if let Some(ref team_registry) = self.team_registry {
            let info = TeamInfo {
                team_name: req.team_name.clone(),
                inbox_name: req.inbox_name.clone(),
            };

            team_registry.register(&key, info.clone()).await;

            // Also store under birth_branch — notify_parent looks up by parent's birth_branch.
            let bb = ctx.birth_branch.to_string();
            if bb != key {
                info!(key = %bb, team_name = %req.team_name, "Also registering team under birth_branch");
                team_registry.register(&bb, info.clone()).await;
            }

            // Also store under slug variant for broader lookup
            if let Some(slug) = key.strip_suffix("-claude") {
                team_registry.register(slug, info).await;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch};
    use crate::effects::{EffectContext, EffectHandler};

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test"),
            birth_branch: BirthBranch::from("main"),
        }
    }

    #[test]
    fn test_namespace() {
        let registry = Arc::new(ClaudeSessionRegistry::new());
        let handler = SessionHandler::new(registry);
        assert_eq!(handler.namespace(), "session");
    }

    #[tokio::test]
    async fn test_register_claude_id() {
        let registry = Arc::new(ClaudeSessionRegistry::new());
        let handler = SessionHandler::new(registry.clone());
        let ctx = test_ctx();

        let req = RegisterClaudeSessionRequest {
            claude_session_id: "7343ced0-1d95-450a-8ae5-976fe94421f0".into(),
        };

        let resp = handler.register_claude_id(req, &ctx).await.unwrap();
        assert!(resp.success);

        let registered = registry.get("test").await.unwrap();
        assert_eq!(
            registered.to_string(),
            "7343ced0-1d95-450a-8ae5-976fe94421f0"
        );
    }

    #[tokio::test]
    async fn test_register_team() {
        let registry = Arc::new(ClaudeSessionRegistry::new());
        let team_registry = Arc::new(TeamRegistry::new());
        let handler = SessionHandler::new(registry).with_team_registry(team_registry.clone());
        let ctx = test_ctx();

        let req = RegisterTeamRequest {
            team_name: "test-team".into(),
            inbox_name: "test-inbox".into(),
        };

        let resp = handler.register_team(req, &ctx).await.unwrap();
        assert!(resp.success);

        // Verify it was registered in team_registry
        let info = team_registry.get("test").await.unwrap();
        assert_eq!(info.team_name, "test-team");
        assert_eq!(info.inbox_name, "test-inbox");

        // Verify it was registered under birth_branch (main)
        let info_bb = team_registry.get("main").await.unwrap();
        assert_eq!(info_bb.team_name, "test-team");
    }

    #[tokio::test]
    async fn test_register_team_slug_variant() {
        let registry = Arc::new(ClaudeSessionRegistry::new());
        let team_registry = Arc::new(TeamRegistry::new());
        let handler = SessionHandler::new(registry).with_team_registry(team_registry.clone());

        let ctx = EffectContext {
            agent_name: AgentName::from("foo-claude"),
            birth_branch: BirthBranch::from("main"),
        };

        let req = RegisterTeamRequest {
            team_name: "test-team".into(),
            inbox_name: "test-inbox".into(),
        };

        handler.register_team(req, &ctx).await.unwrap();

        // Should be found under "foo-claude"
        assert!(team_registry.get("foo-claude").await.is_some());
        // Should be found under slug "foo"
        assert!(team_registry.get("foo").await.is_some());
    }

    #[tokio::test]
    async fn test_register_claude_id_slug_variant() {
        let registry = Arc::new(ClaudeSessionRegistry::new());
        let handler = SessionHandler::new(registry.clone());

        let ctx = EffectContext {
            agent_name: AgentName::from("foo-claude"),
            birth_branch: BirthBranch::from("main"),
        };

        let req = RegisterClaudeSessionRequest {
            claude_session_id: "7343ced0-1d95-450a-8ae5-976fe94421f0".into(),
        };

        handler.register_claude_id(req, &ctx).await.unwrap();

        // Should be found under "foo-claude"
        assert!(registry.get("foo-claude").await.is_some());
        // Should be found under slug "foo"
        assert!(registry.get("foo").await.is_some());
    }
}
