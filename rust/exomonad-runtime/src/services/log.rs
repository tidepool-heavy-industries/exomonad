use anyhow::Result;
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use extism_convert::Json;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tracing;

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
}

#[derive(Debug, Clone, Default)]
pub struct LogService;

impl LogService {
    pub fn log(&self, level: LogLevel, message: &str, fields: Option<&HashMap<String, String>>) {
        match level {
            LogLevel::Debug => tracing::debug!(fields = ?fields, "{}", message),
            LogLevel::Info => tracing::info!(fields = ?fields, "{}", message),
            LogLevel::Warn => tracing::warn!(fields = ?fields, "{}", message),
            LogLevel::Error => tracing::error!(fields = ?fields, "{}", message),
        }
    }

    pub fn emit_event(&self, event_type: &str, data: &Value) {
        tracing::info!(target: "telemetry", event_type = %event_type, data = %data, "Telemetry Event");
    }
}

#[derive(Deserialize)]
struct LogPayload {
    #[allow(dead_code)]
    level: LogLevel,
    message: String,
    fields: Option<HashMap<String, String>>,
}

// We need a way to access LogService from UserData.
// Assuming we pass Arc<Services> as UserData.
// We'll define a trait or struct for Services later, but for this file,
// we can make the host function generic or assume a concrete type.
// The task uses `Arc<Services>`. Let's assume `Services` has a `log` field.

pub trait HasLogService {
    fn log_service(&self) -> &LogService;
}

/// Registers the `log_info` host function.
///
/// Note: This function executes synchronously. "Fire-and-forget" here means
/// it returns `void` to the WASM caller, not that it runs on a background thread.
/// It enforces `LogLevel::Info` regardless of the payload's level.
pub fn log_info_host_fn<S: HasLogService + Send + Sync + 'static>(services: Arc<S>) -> Function {
    Function::new(
        "log_info",
        [ValType::I64],
        [],
        UserData::new(services),
        |plugin, inputs, _outputs, user_data| {
            let services = user_data.get()?;
            let services = services.lock().map_err(|_| Error::msg("Poisoned lock"))?;
            
            if inputs.is_empty() {
                return Err(Error::msg("No inputs provided"));
            }
            let Json(payload): Json<LogPayload> = plugin.memory_get_val(&inputs[0])?;
            
            // Enforce Info level
            services
                .log_service()
                .log(LogLevel::Info, &payload.message, payload.fields.as_ref());
            Ok(())
        },
    )
    .with_namespace("env")
}

/// Registers the `log_error` host function.
///
/// Note: This function executes synchronously.
/// It enforces `LogLevel::Error` regardless of the payload's level.
pub fn log_error_host_fn<S: HasLogService + Send + Sync + 'static>(services: Arc<S>) -> Function {
    Function::new(
        "log_error",
        [ValType::I64],
        [],
        UserData::new(services),
        |plugin, inputs, _outputs, user_data| {
            let services = user_data.get()?;
            let services = services.lock().map_err(|_| Error::msg("Poisoned lock"))?;
            
            if inputs.is_empty() {
                return Err(Error::msg("No inputs provided"));
            }
            let Json(payload): Json<LogPayload> = plugin.memory_get_val(&inputs[0])?;
            
            // Enforce Error level
            services
                .log_service()
                .log(LogLevel::Error, &payload.message, payload.fields.as_ref());
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn emit_event_host_fn<S: HasLogService + Send + Sync + 'static>(services: Arc<S>) -> Function {
    Function::new(
        "emit_event",
        [ValType::I64],
        [],
        UserData::new(services),
        |plugin, inputs, _outputs, user_data| {
            #[derive(Deserialize)]
            struct EventPayload {
                event_type: String,
                data: Value,
            }

            let services = user_data.get()?;
            let services = services.lock().map_err(|_| Error::msg("Poisoned lock"))?;
            
            if inputs.is_empty() {
                return Err(Error::msg("No inputs provided"));
            }
            let Json(payload): Json<EventPayload> = plugin.memory_get_val(&inputs[0])?;
            
            services
                .log_service()
                .emit_event(&payload.event_type, &payload.data);
            Ok(())
        },
    )
    .with_namespace("env")
}
