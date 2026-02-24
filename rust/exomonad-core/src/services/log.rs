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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_log_level_info_serialization() {
        let val = LogLevel::Info;
        let json = serde_json::to_string(&val).unwrap();
        assert_eq!(json, "\"info\"");
    }

    #[test]
    fn test_log_level_error_serialization() {
        let val = LogLevel::Error;
        let json = serde_json::to_string(&val).unwrap();
        assert_eq!(json, "\"error\"");
    }

    #[test]
    fn test_log_level_roundtrip() {
        let val = LogLevel::Warn;
        let json = serde_json::to_string(&val).unwrap();
        let back: LogLevel = serde_json::from_str(&json).unwrap();
        assert!(matches!(back, LogLevel::Warn));
    }

    #[test]
    fn test_log_payload_roundtrip_with_fields() {
        let mut fields = HashMap::new();
        fields.insert("key".to_string(), "value".to_string());
        let val = LogPayload {
            message: "test message".to_string(),
            fields: Some(fields),
        };
        let json = serde_json::to_string(&val).unwrap();
        let back: LogPayload = serde_json::from_str(&json).unwrap();
        assert_eq!(back, val);
    }

    #[test]
    fn test_log_payload_roundtrip_without_fields() {
        let val = LogPayload {
            message: "test message".to_string(),
            fields: None,
        };
        let json = serde_json::to_string(&val).unwrap();
        let back: LogPayload = serde_json::from_str(&json).unwrap();
        assert_eq!(back, val);
    }
}
