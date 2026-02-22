//! Events effect handler for the `events.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::events`.

use crate::effects::{dispatch_events_effect, EffectHandler, EffectResult, EventEffects};
use crate::services::team_registry::TeamRegistry;
use crate::services::teams_mailbox;
use crate::services::{zellij_events, EventQueue};
use async_trait::async_trait;
use exomonad_proto::effects::events::event::EventType;
use exomonad_proto::effects::events::*;
use prost::Message;
use std::sync::Arc;
use std::time::Duration;

/// Events effect handler.
///
/// Handles all effects in the `events.*` namespace.
/// If `remote_url` is set, forwards notify events to the server via HTTP.
/// Otherwise, delegates to the local `EventQueue` service.
pub struct EventHandler {
    queue: Arc<EventQueue>,
    remote_url: Option<String>,
    /// Event queue scope ID (server-internal UUID, NOT the birth-branch).
    event_queue_scope: String,
    client: reqwest::Client,
    /// Tracks agents that have already called notify_parent to prevent duplicate notifications.
    notified_agents: std::sync::Mutex<std::collections::HashSet<String>>,
    /// Claude Teams registry for inbox-based delivery.
    team_registry: Option<Arc<TeamRegistry>>,
}

impl EventHandler {
    pub fn new(
        queue: Arc<EventQueue>,
        remote_port: Option<u16>,
        event_queue_scope: Option<String>,
    ) -> Self {
        let remote_url = remote_port.map(|port| format!("http://127.0.0.1:{}/events", port));
        Self {
            queue,
            remote_url,
            event_queue_scope: event_queue_scope.unwrap_or_else(|| "default".to_string()),
            client: reqwest::Client::new(),
            notified_agents: std::sync::Mutex::new(std::collections::HashSet::new()),
            team_registry: None,
        }
    }

    pub fn with_team_registry(mut self, registry: Arc<TeamRegistry>) -> Self {
        self.team_registry = Some(registry);
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

        let event_result = self
            .queue
            .wait_for_event(
                &self.event_queue_scope,
                &req.types,
                Duration::from_secs(req.timeout_secs as u64),
                req.after_event_id,
            )
            .await;

        match event_result {
            Ok(event) => Ok(WaitForEventResponse { event: Some(event) }),
            Err(_) => Ok(WaitForEventResponse { event: None }),
        }
    }

    async fn notify_event(
        &self,
        req: NotifyEventRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<NotifyEventResponse> {
        tracing::info!(
            session_id = %req.session_id,
            has_event = req.event.is_some(),
            remote = self.remote_url.is_some(),
            "notify_event called"
        );
        if let Some(ref url) = self.remote_url {
            // Forward to server
            let body = req.encode_to_vec();

            let resp = self
                .client
                .post(url)
                .body(body)
                .send()
                .await
                .map_err(|e| crate::effects::EffectError::network_error(e.to_string()))?;

            if !resp.status().is_success() {
                return Err(crate::effects::EffectError::network_error(format!(
                    "Server returned {}",
                    resp.status()
                )));
            }

            Ok(NotifyEventResponse { success: true })
        } else {
            // Local handling
            if let Some(event) = req.event {
                self.queue.notify_event(&req.session_id, event).await;
                Ok(NotifyEventResponse { success: true })
            } else {
                Ok(NotifyEventResponse { success: false })
            }
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
            (agent_name.to_string(), "structural")
        } else {
            (req.agent_id.clone(), "request")
        };

        // Prevent duplicate notifications from the same agent (common in re-runs)
        {
            let mut notified = self.notified_agents.lock().unwrap();
            if notified.contains(&agent_id_str) {
                tracing::debug!(agent_id = %agent_id_str, source = %agent_id_source, "notify_parent: already notified, skipping duplicate");
                return Ok(NotifyParentResponse { ack: true });
            }
            notified.insert(agent_id_str.clone());
        }

        let parent_session_id = match birth_branch.parent() {
            Some(parent) => parent.to_string(),
            None => "root".to_string(),
        };

        tracing::info!(
            agent_id = %agent_id_str,
            source = %agent_id_source,
            parent_session_id = %parent_session_id,
            status = %req.status,
            "notify_parent called"
        );

        // Map to internal Event for the queue
        let event = Event {
            event_id: 0, // Assigned by queue
            event_type: Some(EventType::WorkerComplete(WorkerComplete {
                worker_id: agent_id_str.clone(),
                status: req.status.clone(),
                changes: Vec::new(),
                message: req.message.clone(),
            })),
        };

        if let Some(ref url) = self.remote_url {
            tracing::debug!(url = %url, parent_session_id = %parent_session_id, "notify_parent: forwarding to remote server");
            let forward_req = NotifyEventRequest {
                session_id: parent_session_id.clone(),
                event: Some(event),
            };
            let body = forward_req.encode_to_vec();

            let resp = self.client.post(url).body(body).send().await.map_err(|e| {
                tracing::error!(url = %url, error = %e, "notify_parent: remote forwarding failed");
                crate::effects::EffectError::network_error(e.to_string())
            })?;

            if !resp.status().is_success() {
                tracing::error!(status = %resp.status(), "notify_parent: remote server returned error");
                return Err(crate::effects::EffectError::network_error(format!(
                    "Server returned {} during parent notification",
                    resp.status()
                )));
            }
            tracing::debug!(status = %resp.status(), "notify_parent: remote forwarding succeeded");
        } else {
            self.queue.notify_event(&parent_session_id, event).await;
        }

        // Deliver notification to parent: prefer Teams inbox, fall back to Zellij injection.
        let notification = format_parent_notification(&agent_id_str, &req.status, &req.message);
        let mut delivered_via_teams = false;

        if let Some(ref registry) = self.team_registry {
            // Resolve parent agent key for TeamRegistry lookup
            let parent_key = if parent_session_id == "root" {
                "root".to_string()
            } else {
                parent_session_id.clone()
            };

            if let Some(team_info) = registry.get(&parent_key).await {
                match teams_mailbox::write_to_inbox(
                    &team_info.team_name,
                    &team_info.inbox_name,
                    "exomonad",
                    &notification,
                    &format!("Agent completion: {}", agent_id_str),
                    "blue",
                ) {
                    Ok(()) => {
                        tracing::info!(
                            parent = %parent_key,
                            team = %team_info.team_name,
                            "notify_parent: delivered via Teams inbox"
                        );
                        delivered_via_teams = true;
                    }
                    Err(e) => {
                        tracing::warn!(
                            parent = %parent_key,
                            error = %e,
                            "notify_parent: Teams inbox write failed, falling back to Zellij"
                        );
                    }
                }
            }
        }

        if !delivered_via_teams {
            let tab_name = crate::services::agent_control::resolve_parent_tab_name(ctx);
            tracing::debug!(tab = %tab_name, chars = notification.len(), "notify_parent: injecting notification into parent pane");
            zellij_events::inject_input(&tab_name, &notification);
        }

        Ok(NotifyParentResponse { ack: true })
    }
}

fn format_parent_notification(agent_id: &str, status: &str, message: &str) -> String {
    match status {
        "success" => format!(
            "[CHILD COMPLETE: {}] {}",
            agent_id,
            if message.is_empty() {
                "Task completed successfully."
            } else {
                message
            }
        ),
        "failure" => format!(
            "[CHILD FAILED: {}] {}",
            agent_id,
            if message.is_empty() {
                "Task failed."
            } else {
                message
            }
        ),
        _ => format!("[CHILD {}: {}] {}", status.to_uppercase(), agent_id, message),
    }
}
