//! Events effect handler for the `events.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::events`.

use crate::domain::Address;
use crate::effects::{dispatch_events_effect, EffectHandler, EffectResult, EventEffects};
use crate::services::acp_registry::AcpRegistry;
use crate::services::agent_resolver::AgentResolver;
use crate::services::supervisor_registry::SupervisorRegistry;
use crate::services::EventQueue;
use async_trait::async_trait;
use claude_teams_bridge::TeamRegistry;
use exomonad_proto::effects::events::*;
use std::sync::Arc;
use std::time::Duration;

/// Events effect handler.
///
/// Handles all effects in the `events.*` namespace.
/// Delegates to the local `EventQueue` service.
pub struct EventHandler {
    queue: Arc<EventQueue>,
    /// Event queue scope ID (server-internal UUID, NOT the birth-branch).
    event_queue_scope: String,
    /// Claude Teams registry for inbox-based delivery.
    team_registry: Arc<TeamRegistry>,
    /// ACP connection registry for prompt-based delivery.
    acp_registry: Arc<AcpRegistry>,
    /// Supervisor registry for child → parent routing.
    supervisor_registry: Arc<SupervisorRegistry>,
    /// Canonical agent identity resolver.
    agent_resolver: Arc<AgentResolver>,
    /// Project root directory for resolving UDS socket paths.
    project_dir: std::path::PathBuf,
    /// JSONL event log for offline analysis.
    event_log: Option<Arc<crate::services::event_log::EventLog>>,
}

impl EventHandler {
    pub fn new(
        services: &crate::services::Services,
        event_queue_scope: Option<String>,
        project_dir: std::path::PathBuf,
    ) -> Self {
        Self {
            queue: services.event_queue.clone(),
            event_queue_scope: event_queue_scope.unwrap_or_else(|| "default".to_string()),
            team_registry: services.team_registry.clone(),
            acp_registry: services.acp_registry.clone(),
            supervisor_registry: services.supervisor_registry.clone(),
            agent_resolver: services.agent_resolver.clone(),
            project_dir,
            event_log: services.event_log.clone(),
        }
    }
}

#[async_trait]
impl EffectHandler for EventHandler {
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
impl EventEffects for EventHandler {
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
            .queue
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
            self.queue.notify_event(&req.session_id, event).await;
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
            let resolver_ref = Some(&*self.agent_resolver);
            let (parent_session_id, tab_name) = match &override_addr {
                Address::Agent(name) => {
                    let tab = crate::services::delivery::resolve_tab_name_for_agent(name, resolver_ref);
                    (name.as_str().to_string(), tab)
                }
                Address::Team {
                    member: Some(m), ..
                } => {
                    let tab = crate::services::delivery::resolve_tab_name_for_agent(m, resolver_ref);
                    (m.as_str().to_string(), tab)
                }
                Address::Team { team, member: None } => {
                    let lead = self.team_registry.resolve_lead(team.as_str()).await;
                    let id = lead.unwrap_or_else(|| "root".to_string());
                    let lead_name = crate::domain::AgentName::from(id.as_str());
                    let tab = crate::services::delivery::resolve_tab_name_for_agent(&lead_name, resolver_ref);
                    (id, tab)
                }
                Address::Supervisor => unreachable!(),
            };

            let status = crate::services::delivery::NotifyStatus::from_str(&req.status);
            crate::services::delivery::notify_parent_delivery(
                Some(&*self.team_registry),
                Some(&*self.acp_registry),
                self.event_log.as_deref(),
                &self.queue,
                &self.project_dir,
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
        if let Some(info) = self.supervisor_registry.lookup(birth_branch.as_str()).await {
            tracing::info!(
                supervisor = %info.supervisor,
                team = %info.team,
                "notify_parent: resolved supervisor from registry"
            );
            let parent_session_id = info.supervisor.as_str();
            let supervisor_name = crate::domain::AgentName::from(parent_session_id);
            let tab_name =
                crate::services::delivery::resolve_tab_name_for_agent(&supervisor_name, Some(&*self.agent_resolver));

            let status = crate::services::delivery::NotifyStatus::from_str(&req.status);
            crate::services::delivery::notify_parent_delivery(
                Some(&*self.team_registry),
                Some(&*self.acp_registry),
                self.event_log.as_deref(),
                &self.queue,
                &self.project_dir,
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
            Some(&*self.agent_resolver),
        );

        let status = crate::services::delivery::NotifyStatus::from_str(&req.status);
        crate::services::delivery::notify_parent_delivery(
            Some(&*self.team_registry),
            Some(&*self.acp_registry),
            self.event_log.as_deref(),
            &self.queue,
            &self.project_dir,
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
        let sender = ctx.agent_name.as_str();
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
            sender = %sender,
            "send_message: routing via Address"
        );

        let outcome = crate::services::delivery::route_message(
            &address,
            Some(&*self.team_registry),
            Some(&*self.acp_registry),
            Some(&*self.agent_resolver),
            &self.project_dir,
            sender,
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

    #[test]
    fn test_event_handler_namespace() {
        let services = crate::services::Services::test();
        let handler = EventHandler::new(&services, None, std::path::PathBuf::from("."));
        assert_eq!(handler.namespace(), "events");
    }
}
