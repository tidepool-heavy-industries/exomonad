//! Log effect handler for the `log.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::log`.

use crate::effects::{dispatch_log_effect, EffectHandler, EffectResult, LogEffects};
use async_trait::async_trait;
use exomonad_proto::effects::log::*;
use uuid::Uuid;

/// Log effect handler.
///
/// Handles all effects in the `log.*` namespace by delegating to
/// the generated `dispatch_log_effect` function.
pub struct LogHandler {}

impl LogHandler {
    pub fn new() -> Self {
        Self {}
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
            otel.name = %req.event_type,
            event_type = %req.event_type,
            event_id = %event_id,
            agent_id = %ctx.agent_name,
            payload = %payload_str,
            "[event]"
        );

        Ok(EmitEventResponse { event_id })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_handler_new() {
        let _handler = LogHandler::new();
    }

    #[test]
    fn test_log_handler_default() {
        let _handler = LogHandler::default();
    }

    #[test]
    fn test_log_handler_namespace() {
        let handler = LogHandler::new();
        assert_eq!(handler.namespace(), "log");
    }
}
