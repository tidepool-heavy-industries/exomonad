//! Events effect handler for the `events.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::events`.

use crate::effects::{dispatch_events_effect, EffectHandler, EffectResult, EventEffects};
use crate::domain::NotifyStatus;
use crate::services::acp_registry::AcpRegistry;
use crate::services::delivery::DeliveryResult;
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
    team_registry: Option<Arc<TeamRegistry>>,
    /// ACP connection registry for prompt-based delivery.
    acp_registry: Option<Arc<AcpRegistry>>,
    /// Project root directory for resolving UDS socket paths.
    project_dir: std::path::PathBuf,
    /// JSONL event log for offline analysis.
    event_log: Option<Arc<crate::services::event_log::EventLog>>,
}

impl EventHandler {
    pub fn new(
        queue: Arc<EventQueue>,
        event_queue_scope: Option<String>,
        project_dir: std::path::PathBuf,
    ) -> Self {
        Self {
            queue,
            event_queue_scope: event_queue_scope.unwrap_or_else(|| "default".to_string()),
            team_registry: None,
            acp_registry: None,
            project_dir,
            event_log: None,
        }
    }

    pub fn with_team_registry(mut self, registry: Arc<TeamRegistry>) -> Self {
        self.team_registry = Some(registry);
        self
    }

    pub fn with_acp_registry(mut self, registry: Arc<AcpRegistry>) -> Self {
        self.acp_registry = Some(registry);
        self
    }

    pub fn with_event_log(mut self, log: Arc<crate::services::event_log::EventLog>) -> Self {
        self.event_log = Some(log);
        self
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
        let (agent_id_str, agent_id_source) = if req.agent_id.is_empty() {
            (agent_name.to_string(), "ctx")
        } else {
            (req.agent_id.clone(), "request")
        };

        tracing::debug!(
            agent_id = %agent_id_str,
            source = agent_id_source,
            "notify_parent: resolved agent_id"
        );

        // Identity model:
        // - Subtree agents: birth-branch is their own branch name (e.g. "main.feature-a").
        //   Their parent is one level up (e.g. "main" -> root).
        // - Workers: birth-branch is inherited from parent (parent's birth-branch).
        let parent_session_id = if agent_name.is_gemini_worker() {
            // Worker: birth-branch is inherited from parent
            birth_branch.to_string()
        } else {
            // Subtree agent: parent is one level up
            birth_branch
                .parent()
                .map(|p| p.to_string())
                .unwrap_or_else(|| "root".to_string())
        };

        // Convert proto enum to NotifyStatus for the delivery pipeline
        let status = match exomonad_proto::effects::events::NotifyStatus::try_from(req.status) {
            Ok(exomonad_proto::effects::events::NotifyStatus::Failure) => NotifyStatus::Failure,
            _ => NotifyStatus::Success,
        };

        tracing::info!(
            birth_branch = %birth_branch,
            parent_session_id = %parent_session_id,
            status = %status.as_str(),
            "notify_parent: routing message to parent"
        );

        let tab_name = crate::services::agent_control::resolve_parent_tab_name(ctx);

        crate::services::delivery::notify_parent_delivery(
            self.team_registry.as_deref(),
            self.acp_registry.as_deref(),
            self.event_log.as_deref(),
            &self.queue,
            &self.project_dir,
            &agent_id_str,
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
        let tab_name = resolve_recipient_tab_name(&req.recipient);

        let delivery_result = crate::services::delivery::deliver_to_agent(
            self.team_registry.as_deref(),
            self.acp_registry.as_deref(),
            &self.project_dir,
            &req.recipient,
            &tab_name,
            sender,
            &req.content,
            &summary,
        )
        .await;

        let method_string = match delivery_result {
            DeliveryResult::Teams => "teams_inbox",
            DeliveryResult::Acp => "acp",
            DeliveryResult::Uds => "unix_socket",
            DeliveryResult::Tmux => "tmux_stdin",
            DeliveryResult::Failed => "failed",
        };

        tracing::info!(
            otel.name = "agent.message_sent",
            recipient = %req.recipient,
            method = method_string,
            "[event] agent.message_sent"
        );

        Ok(SendMessageResponse {
            success: !matches!(delivery_result, DeliveryResult::Failed),
            delivery_method: method_string.to_string(),
        })
    }
}

fn resolve_recipient_tab_name(recipient: &str) -> String {
    if recipient == "root" {
        "TL".to_string()
    } else {
        crate::services::agent_control::AgentType::from_dir_name(recipient)
            .tab_display_name(recipient)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_handler_namespace() {
        let queue = Arc::new(EventQueue::new());
        let handler = EventHandler::new(queue, None, std::path::PathBuf::from("."));
        assert_eq!(handler.namespace(), "events");
    }

    #[test]
    fn test_resolve_recipient_tab_name() {
        assert_eq!(super::resolve_recipient_tab_name("root"), "TL");
        assert_eq!(
            super::resolve_recipient_tab_name("feature-a-gemini"),
            "💎 feature-a-gemini"
        );
        assert_eq!(
            super::resolve_recipient_tab_name("feature-b-claude"),
            "🤖 feature-b-claude"
        );
    }
}
