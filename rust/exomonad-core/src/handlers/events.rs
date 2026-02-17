//! Events effect handler for the `events.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::events`.

use crate::effects::{dispatch_events_effect, EffectHandler, EffectResult, EventEffects};
use crate::services::{zellij_events, EventQueue};
use async_trait::async_trait;
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
        }
    }

    fn resolve_parent_tab_name(&self, ctx: &crate::effects::EffectContext) -> String {
        let agent_name = &ctx.agent_name;

        let birth_branch = &ctx.birth_branch;
        let birth_branch_str = birth_branch.as_str();

        if agent_name.is_gemini_worker() {
            // Worker: birth-branch is parent's birth-branch (inherited)
            if birth_branch_str.contains('.') {
                let slug = birth_branch_str
                    .rsplit_once('.')
                    .map(|(_, s)| s)
                    .unwrap_or(birth_branch_str);
                format!("\u{1F916} {}", slug)
            } else {
                "TL".to_string()
            }
        } else {
            // Subtree agent: parent is one level up
            if let Some(parent) = birth_branch.parent() {
                if parent.as_str().contains('.') {
                    let slug = parent
                        .as_str()
                        .rsplit_once('.')
                        .map(|(_, s)| s)
                        .unwrap_or(parent.as_str());
                    format!("\u{1F916} {}", slug)
                } else {
                    "TL".to_string()
                }
            } else {
                "TL".to_string()
            }
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
            (agent_name.to_string(), "ctx")
        } else {
            (req.agent_id.clone(), "request")
        };

        // Idempotency: ignore duplicate notify_parent calls from spinning agents
        {
            let mut set = self.notified_agents.lock().unwrap();
            if !set.insert(agent_id_str.clone()) {
                tracing::warn!(agent_id = %agent_id_str, "notify_parent called again — ignoring duplicate");
                return Ok(NotifyParentResponse { ack: true });
            }
        }

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

        tracing::info!(
            agent_id = %agent_id_str,
            birth_branch = %birth_branch,
            parent_session_id = %parent_session_id,
            status = %req.status,
            "notify_parent: routing completion to parent"
        );

        let event = Event {
            event_id: 0, // Assigned by EventQueue
            event_type: Some(event::EventType::WorkerComplete(WorkerComplete {
                worker_id: agent_id_str.clone(),
                status: req.status.clone(),
                message: req.message.clone(),
                changes: Vec::new(),
            })),
        };

        if let Some(ref url) = self.remote_url {
            tracing::debug!(url = %url, parent_session_id = %parent_session_id, "notify_parent: forwarding to remote server");
            let forward_req = NotifyEventRequest {
                session_id: parent_session_id,
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

        // Inject natural-language notification into parent's Zellij pane
        let tab_name = self.resolve_parent_tab_name(ctx);
        let notification = format_parent_notification(&agent_id_str, &req.status, &req.message);
        tracing::debug!(tab = %tab_name, chars = notification.len(), "notify_parent: injecting notification into parent pane");
        zellij_events::inject_input(&tab_name, &notification);

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
        _ => format!("[CHILD STATUS: {} - {}] {}", agent_id, status, message),
    }
}
