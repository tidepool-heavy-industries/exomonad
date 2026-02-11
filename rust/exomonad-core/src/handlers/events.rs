//! Events effect handler for the `events.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::events`.

use crate::effects::{dispatch_events_effect, EffectHandler, EffectResult, EventEffects};
use crate::services::EventQueue;
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
    pub fn new(queue: Arc<EventQueue>, remote_port: Option<u16>, session_id: Option<String>) -> Self {
        let remote_url = remote_port.map(|port| format!("http://127.0.0.1:{}/events", port));
        Self {
            queue,
            remote_url,
            session_id,
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
        
        // Use a default timeout of 300s if not specified or 0
        let timeout_secs = if req.timeout_secs <= 0 { 300 } else { req.timeout_secs as u64 };
        
        let event = self.queue.wait_for_event(
            session_id,
            &req.types,
            Duration::from_secs(timeout_secs)
        ).await
        .map_err(|e| crate::effects::EffectError::custom("events.wait_failed", e.to_string()))?;

        Ok(WaitForEventResponse {
            event: Some(event),
        })
    }

    async fn notify_event(&self, req: NotifyEventRequest) -> EffectResult<NotifyEventResponse> {
        if let Some(ref url) = self.remote_url {
            // Forward to server
            let client = reqwest::Client::new();
            let body = req.encode_to_vec();
            
            let resp = client.post(url)
                .body(body)
                .send()
                .await
                .map_err(|e| crate::effects::EffectError::network_error(e.to_string()))?;
                
            if !resp.status().is_success() {
                return Err(crate::effects::EffectError::network_error(format!("Server returned {}", resp.status())));
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
}