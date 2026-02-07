//! Log effect handler for the `log.*` namespace.
//!
//! Uses proto-generated types from `exomonad_proto::effects::log`.

use async_trait::async_trait;
use exomonad_core::effects::{dispatch_log_effect, EffectHandler, EffectResult, LogEffects};
use exomonad_proto::effects::log::*;
use uuid::Uuid;

/// Log effect handler.
///
/// Handles all effects in the `log.*` namespace by delegating to
/// the generated `dispatch_log_effect` function.
pub struct LogHandler;

impl LogHandler {
    pub fn new() -> Self {
        Self
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

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_log_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl LogEffects for LogHandler {
    async fn log(&self, req: LogRequest) -> EffectResult<LogResponse> {
        let level = req.level();
        let fields_str = if req.fields.is_empty() {
            None
        } else {
            String::from_utf8(req.fields).ok()
        };

        match level {
            LogLevel::Debug => {
                if let Some(ref f) = fields_str {
                    tracing::debug!(message = %req.message, fields = %f, "[wasm]");
                } else {
                    tracing::debug!(message = %req.message, "[wasm]");
                }
            }
            LogLevel::Warn => {
                if let Some(ref f) = fields_str {
                    tracing::warn!(message = %req.message, fields = %f, "[wasm]");
                } else {
                    tracing::warn!(message = %req.message, "[wasm]");
                }
            }
            LogLevel::Error => {
                if let Some(ref f) = fields_str {
                    tracing::error!(message = %req.message, fields = %f, "[wasm]");
                } else {
                    tracing::error!(message = %req.message, "[wasm]");
                }
            }
            _ => {
                // Info and Unspecified
                if let Some(ref f) = fields_str {
                    tracing::info!(message = %req.message, fields = %f, "[wasm]");
                } else {
                    tracing::info!(message = %req.message, "[wasm]");
                }
            }
        }

        Ok(LogResponse { success: true })
    }

    async fn debug(&self, req: DebugRequest) -> EffectResult<LogResponse> {
        let fields_str = if req.fields.is_empty() {
            None
        } else {
            String::from_utf8(req.fields).ok()
        };

        if let Some(f) = fields_str {
            tracing::debug!(message = %req.message, fields = %f, "[wasm]");
        } else {
            tracing::debug!(message = %req.message, "[wasm]");
        }

        Ok(LogResponse { success: true })
    }

    async fn info(&self, req: InfoRequest) -> EffectResult<LogResponse> {
        let fields_str = if req.fields.is_empty() {
            None
        } else {
            String::from_utf8(req.fields).ok()
        };

        if let Some(f) = fields_str {
            tracing::info!(message = %req.message, fields = %f, "[wasm]");
        } else {
            tracing::info!(message = %req.message, "[wasm]");
        }

        Ok(LogResponse { success: true })
    }

    async fn warn(&self, req: WarnRequest) -> EffectResult<LogResponse> {
        let fields_str = if req.fields.is_empty() {
            None
        } else {
            String::from_utf8(req.fields).ok()
        };

        if let Some(f) = fields_str {
            tracing::warn!(message = %req.message, fields = %f, "[wasm]");
        } else {
            tracing::warn!(message = %req.message, "[wasm]");
        }

        Ok(LogResponse { success: true })
    }

    async fn error(&self, req: ErrorRequest) -> EffectResult<LogResponse> {
        let fields_str = if req.fields.is_empty() {
            None
        } else {
            String::from_utf8(req.fields).ok()
        };

        if let Some(f) = fields_str {
            tracing::error!(message = %req.message, fields = %f, "[wasm]");
        } else {
            tracing::error!(message = %req.message, "[wasm]");
        }

        Ok(LogResponse { success: true })
    }

    async fn emit_event(&self, req: EmitEventRequest) -> EffectResult<EmitEventResponse> {
        let event_id = Uuid::new_v4().to_string();

        let payload_str = if req.payload.is_empty() {
            "{}".to_string()
        } else {
            String::from_utf8(req.payload).unwrap_or_else(|_| "<binary>".to_string())
        };

        tracing::info!(
            event_type = %req.event_type,
            event_id = %event_id,
            payload = %payload_str,
            "[event]"
        );

        Ok(EmitEventResponse { event_id })
    }
}
