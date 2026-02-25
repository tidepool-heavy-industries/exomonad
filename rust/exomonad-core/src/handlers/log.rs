//! Log effect handler for the `log.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::log`.

use crate::effects::{dispatch_log_effect, EffectHandler, EffectResult, LogEffects};
use crate::services::EventLog;
use async_trait::async_trait;
use exomonad_proto::effects::log::*;
use std::sync::Arc;
use uuid::Uuid;

/// Log effect handler.
///
/// Handles all effects in the `log.*` namespace by delegating to
/// the generated `dispatch_log_effect` function.
/// Optionally writes structured events to a JSONL event log.
pub struct LogHandler {
    event_log: Option<Arc<EventLog>>,
}

impl LogHandler {
    pub fn new() -> Self {
        Self { event_log: None }
    }

    pub fn with_event_log(mut self, event_log: Arc<EventLog>) -> Self {
        self.event_log = Some(event_log);
        self
    }

    async fn do_log(
        &self,
        level: tracing::Level,
        message: String,
        fields: Vec<u8>,
    ) -> EffectResult<LogResponse> {
        let fields_str = if fields.is_empty() {
            None
        } else {
            String::from_utf8(fields).ok()
        };

        if let Some(f) = fields_str {
            match level {
                tracing::Level::ERROR => tracing::error!(message = %message, fields = %f, "[wasm]"),
                tracing::Level::WARN => tracing::warn!(message = %message, fields = %f, "[wasm]"),
                tracing::Level::INFO => tracing::info!(message = %message, fields = %f, "[wasm]"),
                tracing::Level::DEBUG => tracing::debug!(message = %message, fields = %f, "[wasm]"),
                tracing::Level::TRACE => tracing::trace!(message = %message, fields = %f, "[wasm]"),
            }
        } else {
            match level {
                tracing::Level::ERROR => tracing::error!(message = %message, "[wasm]"),
                tracing::Level::WARN => tracing::warn!(message = %message, "[wasm]"),
                tracing::Level::INFO => tracing::info!(message = %message, "[wasm]"),
                tracing::Level::DEBUG => tracing::debug!(message = %message, "[wasm]"),
                tracing::Level::TRACE => tracing::trace!(message = %message, "[wasm]"),
            }
        }

        Ok(LogResponse { success: true })
    }
}

impl Default for LogHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl EffectHandler for LogHandler {
    fn namespace(&self) -> &str {
        "log"
    }

    async fn handle(
        &self,
        effect_type: &str,
        payload: &[u8],
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<Vec<u8>> {
        dispatch_log_effect(self, effect_type, payload, ctx).await
    }
}

#[async_trait]
impl LogEffects for LogHandler {
    async fn log(
        &self,
        req: LogRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<LogResponse> {
        let level = match req.level() {
            LogLevel::Error => tracing::Level::ERROR,
            LogLevel::Warn => tracing::Level::WARN,
            LogLevel::Debug => tracing::Level::DEBUG,
            _ => tracing::Level::INFO,
        };
        self.do_log(level, req.message, req.fields).await
    }

    async fn debug(
        &self,
        req: DebugRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<LogResponse> {
        self.do_log(tracing::Level::DEBUG, req.message, req.fields)
            .await
    }

    async fn info(
        &self,
        req: InfoRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<LogResponse> {
        self.do_log(tracing::Level::INFO, req.message, req.fields)
            .await
    }

    async fn warn(
        &self,
        req: WarnRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<LogResponse> {
        self.do_log(tracing::Level::WARN, req.message, req.fields)
            .await
    }

    async fn error(
        &self,
        req: ErrorRequest,
        _ctx: &crate::effects::EffectContext,
    ) -> EffectResult<LogResponse> {
        self.do_log(tracing::Level::ERROR, req.message, req.fields)
            .await
    }

    async fn emit_event(
        &self,
        req: EmitEventRequest,
        ctx: &crate::effects::EffectContext,
    ) -> EffectResult<EmitEventResponse> {
        let event_id = Uuid::new_v4().to_string();

        let payload_str = if req.payload.is_empty() {
            "{}".to_string()
        } else {
            String::from_utf8(req.payload.clone()).unwrap_or_else(|_| "<binary>".to_string())
        };

        tracing::info!(
            event_type = %req.event_type,
            event_id = %event_id,
            payload = %payload_str,
            "[event]"
        );

        // Write to JSONL event log
        if let Some(ref log) = self.event_log {
            let data: serde_json::Value = if req.payload.is_empty() {
                serde_json::json!({})
            } else {
                serde_json::from_slice(&req.payload)
                    .unwrap_or(serde_json::json!({"raw": payload_str}))
            };
            let agent_id = ctx.agent_name.to_string();
            if let Err(e) = log.append(&req.event_type, &agent_id, &data) {
                tracing::warn!(error = %e, "Failed to write event to JSONL log");
            }
        }

        Ok(EmitEventResponse { event_id })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn test_log_handler_new() {
        let handler = LogHandler::new();
        assert!(handler.event_log.is_none());
    }

    #[test]
    fn test_log_handler_default() {
        let handler = LogHandler::default();
        assert!(handler.event_log.is_none());
    }

    #[test]
    fn test_log_handler_namespace() {
        let handler = LogHandler::new();
        assert_eq!(handler.namespace(), "log");
    }

    #[test]
    fn test_log_handler_with_event_log() {
        let tmp = tempfile::tempdir().unwrap();
        let log_path = tmp.path().join("events.jsonl");
        let event_log = Arc::new(EventLog::open(log_path).unwrap());
        let handler = LogHandler::new().with_event_log(event_log.clone());

        assert!(handler.event_log.is_some());
        assert!(Arc::ptr_eq(handler.event_log.as_ref().unwrap(), &event_log));
    }
}