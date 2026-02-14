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
    session_id: Option<String>,
}

impl EventHandler {
    pub fn new(
        queue: Arc<EventQueue>,
        remote_port: Option<u16>,
        session_id: Option<String>,
    ) -> Self {
        let remote_url = remote_port.map(|port| format!("http://127.0.0.1:{}/events", port));
        Self {
            queue,
            remote_url,
            session_id,
        }
    }

    fn resolve_parent_tab_name(&self) -> String {
        let session_id = self.session_id.as_deref().unwrap_or("");
        let agent_id = crate::mcp::agent_identity::get_agent_id();

        if agent_id.ends_with("-gemini") {
            // Worker: session_id is parent's session ID
            if session_id.contains('.') {
                let slug = session_id
                    .rsplit_once('.')
                    .map(|(_, s)| s)
                    .unwrap_or(session_id);
                format!("\u{1F916} {}", slug)
            } else {
                "TL".to_string()
            }
        } else {
            // Subtree agent: parent is one level up
            if let Some((parent, _)) = session_id.rsplit_once('.') {
                if parent.contains('.') {
                    let slug = parent.rsplit_once('.').map(|(_, s)| s).unwrap_or(parent);
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

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_events_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl EventEffects for EventHandler {
    async fn wait_for_event(&self, req: WaitForEventRequest) -> EffectResult<WaitForEventResponse> {
        let session_id = self.session_id.as_deref().unwrap_or("default");
        tracing::info!(
            session_id = %session_id,
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
                session_id,
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

    async fn notify_event(&self, req: NotifyEventRequest) -> EffectResult<NotifyEventResponse> {
        tracing::info!(
            session_id = %req.session_id,
            has_event = req.event.is_some(),
            remote = self.remote_url.is_some(),
            "notify_event called"
        );
        if let Some(ref url) = self.remote_url {
            // Forward to server
            let client = reqwest::Client::new();
            let body = req.encode_to_vec();

            let resp = client
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

    async fn notify_parent(&self, req: NotifyParentRequest) -> EffectResult<NotifyParentResponse> {
        let session_id = self.session_id.as_deref().unwrap_or("root");
        let agent_id = crate::mcp::agent_identity::get_agent_id();

        // Identity model:
        // - Subtree agents: session_id is their own branch name (e.g. "main.feature-a").
        //   Their parent is one level up (e.g. "main" -> "root").
        // - Workers: session_id is already their parent's session ID.
        let parent_session_id = if agent_id.ends_with("-gemini") {
            // Worker: session_id is inherited from parent
            session_id.to_string()
        } else {
            // Subtree agent: session_id is own branch, parent is parent branch
            if let Some((parent, _)) = session_id.rsplit_once('.') {
                parent.to_string()
            } else {
                "root".to_string()
            }
        };

        tracing::info!(
            agent_id = %agent_id,
            session_id = %session_id,
            parent_session_id = %parent_session_id,
            status = %req.status,
            "notify_parent: routing completion to parent"
        );

        let event = Event {
            event_id: 0, // Assigned by EventQueue
            event_type: Some(event::EventType::WorkerComplete(WorkerComplete {
                worker_id: agent_id.clone(),
                status: req.status.clone(),
                message: req.message.clone(),
                changes: Vec::new(),
            })),
        };

        if let Some(ref url) = self.remote_url {
            let forward_req = NotifyEventRequest {
                session_id: parent_session_id,
                event: Some(event),
            };
            let client = reqwest::Client::new();
            let body = forward_req.encode_to_vec();

            let resp = client
                .post(url)
                .body(body)
                .send()
                .await
                .map_err(|e| crate::effects::EffectError::network_error(e.to_string()))?;

            if !resp.status().is_success() {
                return Err(crate::effects::EffectError::network_error(format!(
                    "Server returned {} during parent notification",
                    resp.status()
                )));
            }
        } else {
            self.queue.notify_event(&parent_session_id, event).await;
        }

        // Inject natural-language notification into parent's Zellij pane
        let tab_name = self.resolve_parent_tab_name();
        let notification = format_parent_notification(&agent_id, &req.status, &req.message);
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
