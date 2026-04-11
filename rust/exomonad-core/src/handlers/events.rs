//! Events effect handler for the `events.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::events`.

use crate::domain::Address;
use crate::effects::{dispatch_events_effect, EffectHandler, EffectResult, EventEffects};
use async_trait::async_trait;
use exomonad_proto::effects::events::*;
use std::sync::Arc;
use std::time::Duration;

use crate::services::{
    HasAcpRegistry, HasAgentResolver, HasEventLog, HasEventQueue, HasProjectDir,
    HasSupervisorRegistry, HasTeamRegistry, HasTmuxIpc,
};

/// Events effect handler.
///
/// Handles all effects in the `events.*` namespace.
/// Delegates to the local `EventQueue` service.
pub struct EventHandler<C> {
    ctx: Arc<C>,
    /// Event queue scope ID (server-internal UUID, NOT the birth-branch).
    event_queue_scope: String,
}

impl<C: HasEventQueue> EventHandler<C> {
    pub fn new(ctx: Arc<C>, event_queue_scope: Option<String>) -> Self {
        Self {
            ctx,
            event_queue_scope: event_queue_scope.unwrap_or_else(|| "default".to_string()),
        }
    }
}

#[async_trait]
impl<
        C: HasTeamRegistry
            + HasAcpRegistry
            + HasAgentResolver
            + HasEventLog
            + HasEventQueue
            + HasProjectDir
            + HasSupervisorRegistry
            + HasTmuxIpc
            + 'static,
    > EffectHandler for EventHandler<C>
{
    fn namespace(&self) -> &str {
        "events"
    }

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_events_effect(self, effect_type, payload, ctx).await
    }
}

#[async_trait]
impl<
        C: HasTeamRegistry
            + HasAcpRegistry
            + HasAgentResolver
            + HasEventLog
            + HasEventQueue
            + HasProjectDir
            + HasSupervisorRegistry
            + HasTmuxIpc
            + 'static,
    > EventEffects for EventHandler<C>
{
    async fn wait_for_event(
        &self,
        req: WaitForEventRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<WaitForEventResponse> {
        tracing::info!(
            event_queue_scope = %self.event_queue_scope,
            types = ?req.types,
            timeout_secs = req.timeout_secs,
            after_event_id = req.after_event_id,
            "wait_for_event called"
        );

        // Use a default timeout of 300s if not specified or 0
        let timeout_secs = if req.timeout_secs <= 0 {
            300
        } else {
            req.timeout_secs as u64
        };

        let event = self
            .ctx
            .event_queue()
            .wait_for_event(
                &self.event_queue_scope,
                &req.types,
                Duration::from_secs(timeout_secs),
                req.after_event_id,
            )
            .await
            .map_err(|e| {
                crate::effects::EffectError::custom("events.wait_failed", e.to_string())
            })?;

        Ok(WaitForEventResponse { event: Some(event) })
    }

    async fn notify_event(
        &self,
        req: NotifyEventRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<NotifyEventResponse> {
        tracing::info!(
            session_id = %req.session_id,
            has_event = req.event.is_some(),
            "notify_event called"
        );
        // Local handling
        if let Some(event) = req.event {
            self.ctx
                .event_queue()
                .notify_event(&req.session_id, event)
                .await;
            Ok(NotifyEventResponse { success: true })
        } else {
            Ok(NotifyEventResponse { success: false })
        }
    }

    async fn notify_parent(
        &self,
        req: NotifyParentRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<NotifyParentResponse> {
        let birth_branch = &ctx.birth_branch;
        let agent_name = &ctx.agent_name;

        // Prefer agent_id from the request (set by WASM caller) over structural identity
        let (agent_id, agent_id_source) = if req.agent_id.is_empty() {
            (agent_name.clone(), "ctx")
        } else {
            (
                crate::domain::AgentName::from(req.agent_id.as_str()),
                "request",
            )
        };

        tracing::debug!(
            agent_id = %agent_id,
            source = agent_id_source,
            "notify_parent: resolved agent_id"
        );

        // Check for override_recipient first (explicit routing)
        let override_addr = Address::from_proto(req.override_recipient.clone());

        // Resolve parent session ID:
        // 1. If override_recipient is set and not Supervisor, use that address via route_message
        // 2. Check SupervisorRegistry for explicit supervisor mapping
        // 3. Fall back to structural identity (birth-branch parent)
        if !matches!(override_addr, Address::Supervisor) {
            tracing::info!(
                address = %override_addr,
                "notify_parent: using override_recipient"
            );
            // Resolve the override address to a concrete agent key for notify_parent_delivery
            let resolver_ref = Some(self.ctx.agent_resolver());
            let (parent_session_id, tab_name) = match &override_addr {
                Address::Agent(name) => {
                    let tab =
                        crate::services::delivery::resolve_tab_name_for_agent(name, resolver_ref);
                    (name.as_str().to_string(), tab)
                }
                Address::Team {
                    member: Some(m), ..
                } => {
                    let tab =
                        crate::services::delivery::resolve_tab_name_for_agent(m, resolver_ref);
                    (m.as_str().to_string(), tab)
                }
                Address::Team { team, member: None } => {
                    let lead = self.ctx.team_registry().resolve_lead(team.as_str()).await;
                    let id = lead.unwrap_or_else(|| "root".to_string());
                    if id.is_empty() {
                        return Err(crate::effects::EffectError::custom(
                            "events.empty_id",
                            "empty agent id from registry",
                        ));
                    }
                    let lead_name = crate::domain::AgentName::from(id.as_str());
                    let tab = crate::services::delivery::resolve_tab_name_for_agent(
                        &lead_name,
                        resolver_ref,
                    );
                    (id, tab)
                }
                Address::Supervisor => unreachable!(),
            };

            let status = crate::services::delivery::NotifyStatus::parse(&req.status);
            crate::services::delivery::notify_parent_delivery(
                &*self.ctx,
                &agent_id,
                &parent_session_id,
                &tab_name,
                status,
                &req.message,
                None,
                "agent",
            )
            .await;
            return Ok(NotifyParentResponse { ack: true });
        }

        // Check SupervisorRegistry for this agent's birth-branch
        if let Some(info) = self
            .ctx
            .supervisor_registry()
            .lookup(birth_branch.as_str())
            .await
        {
            tracing::info!(
                supervisor = %info.supervisor,
                team = %info.team,
                "notify_parent: resolved supervisor from registry"
            );
            let parent_session_id = info.supervisor.as_str();
            let supervisor_name = crate::domain::AgentName::from(parent_session_id);
            let tab_name = crate::services::delivery::resolve_tab_name_for_agent(
                &supervisor_name,
                Some(self.ctx.agent_resolver()),
            );

            let status = crate::services::delivery::NotifyStatus::parse(&req.status);
            crate::services::delivery::notify_parent_delivery(
                &*self.ctx,
                &agent_id,
                parent_session_id,
                &tab_name,
                status,
                &req.message,
                None,
                "agent",
            )
            .await;
            return Ok(NotifyParentResponse { ack: true });
        }

        // Structural fallback: birth-branch parent
        let parent_session_id = if agent_name.is_gemini_worker() {
            birth_branch.to_string()
        } else {
            birth_branch
                .parent()
                .map(|p| p.to_string())
                .unwrap_or_else(|| "root".to_string())
        };

        tracing::info!(
            birth_branch = %birth_branch,
            parent_session_id = %parent_session_id,
            status = %req.status,
            "notify_parent: routing via structural identity"
        );

        let parent_agent = crate::domain::AgentName::from(parent_session_id.as_str());
        let tab_name = crate::services::delivery::resolve_tab_name_for_agent(
            &parent_agent,
            Some(self.ctx.agent_resolver()),
        );

        let status = crate::services::delivery::NotifyStatus::parse(&req.status);
        crate::services::delivery::notify_parent_delivery(
            &*self.ctx,
            &agent_id,
            &parent_session_id,
            &tab_name,
            status,
            &req.message,
            None,
            "agent",
        )
        .await;

        Ok(NotifyParentResponse { ack: true })
    }

    async fn send_message(
        &self,
        req: SendMessageRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<SendMessageResponse> {
        let summary = if req.summary.is_empty() {
            req.content.chars().take(50).collect::<String>()
        } else {
            req.summary.clone()
        };

        let address = Address::from_proto(req.recipient.clone());

        // Validate: send_message requires an explicit recipient, not Supervisor
        if matches!(address, Address::Supervisor) {
            return Err(crate::effects::EffectError::custom(
                "events.invalid_input",
                "send_message requires an explicit recipient (agent name or team); got empty/missing recipient".to_string(),
            ));
        }

        tracing::info!(
            address = %address,
            sender = %ctx.agent_name,
            "send_message: routing via Address"
        );

        let outcome = crate::services::delivery::route_message(
            &*self.ctx,
            &address,
            &ctx.agent_name,
            &req.content,
            &summary,
        )
        .await;

        let method_string = outcome.method_string();
        let success = outcome.is_success();

        tracing::info!(
            otel.name = "agent.message_sent",
            address = %address,
            method = method_string,
            success = success,
            "[event] agent.message_sent"
        );

        Ok(SendMessageResponse {
            success,
            delivery_method: method_string.to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch};
    use crate::effects::EffectContext;

    #[test]
    fn test_event_handler_namespace() {
        let services = Arc::new(crate::services::Services::test());
        let handler = EventHandler::new(services, None);
        assert_eq!(handler.namespace(), "events");
    }

    #[tokio::test]
    async fn test_notify_parent_empty_lead() {
        let services = Arc::new(crate::services::Services::test());
        // Register an empty lead for "test-team"
        services
            .team_registry()
            .register(
                "",
                claude_teams_bridge::TeamInfo {
                    team_name: "test-team".to_string(),
                    inbox_name: "lead".to_string(),
                },
            )
            .await;

        let handler = EventHandler::new(services, None);
        let ctx = EffectContext {
            agent_name: AgentName::from("agent"),
            birth_branch: BirthBranch::from("main"),
            working_dir: std::path::PathBuf::from("."),
        };

        let req = NotifyParentRequest {
            message: "hello".to_string(),
            status: "success".to_string(),
            agent_id: "".to_string(),
            override_recipient: Some(exomonad_proto::effects::events::Address {
                kind: Some(exomonad_proto::effects::events::address::Kind::Team(
                    exomonad_proto::effects::events::TeamAddress {
                        team: "test-team".to_string(),
                        member: "".to_string(),
                    },
                )),
            }),
        };

        let result = handler.notify_parent(req, &ctx).await;
        assert!(result.is_err());
        if let Err(crate::effects::EffectError::Custom { code, .. }) = result {
            assert_eq!(code, "events.empty_id");
        } else {
            panic!("Expected events.empty_id error, got {:?}", result);
        }
    }
}
