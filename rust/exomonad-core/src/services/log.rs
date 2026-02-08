use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LogPayload {
    pub message: String,
    pub fields: Option<HashMap<String, String>>,
}

pub trait HasLogService {
    fn log_service(&self) -> &LogService;
}
