//! Effect-specific error types for the extensible effects system.
//!
//! These error types are designed to be:
//! - Serializable across the WASM boundary
//! - Semantically meaningful for effect handlers
//! - Extensible via the `Custom` variant for domain-specific errors

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Effect-specific error with common variants and Custom fallback.
///
/// This error type is returned by effect handlers when operations fail.
/// It provides structured information that can be serialized across the
/// WASM boundary and handled programmatically by the guest.
///
/// # Common Patterns
///
/// - `NotFound`: Resource doesn't exist (file, issue, entity)
/// - `InvalidInput`: Request parameters failed validation
/// - `NetworkError`: External service unreachable or returned error
/// - `Custom`: Domain-specific errors with extensible structure
///
/// # Example
///
/// ```rust,ignore
/// use crate::effects::EffectError;
///
/// fn find_entity(id: &str) -> Result<(), EffectError> {
///     // Simulate a lookup that fails
///     if id == "not_found" {
///         Err(EffectError::not_found(format!("entity/{}", id)))
///     } else {
///         Ok(())
///     }
/// }
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "kind")]
pub enum EffectError {
    /// Resource not found.
    #[serde(rename = "not_found")]
    NotFound {
        /// Description of the missing resource (e.g., "issue/123", "file/README.md").
        resource: String,
    },

    /// Invalid input parameters.
    #[serde(rename = "invalid_input")]
    InvalidInput {
        /// Human-readable description of the validation failure.
        message: String,
    },

    /// Network or external service error.
    #[serde(rename = "network_error")]
    NetworkError {
        /// Description of the network failure.
        message: String,
    },

    /// Permission denied.
    #[serde(rename = "permission_denied")]
    PermissionDenied {
        /// Description of the permission failure.
        message: String,
    },

    /// Operation timed out.
    #[serde(rename = "timeout")]
    Timeout {
        /// Description of what timed out.
        message: String,
    },

    /// Custom domain-specific error.
    ///
    /// Use this for errors that don't fit the common patterns above.
    /// External consumers can define their own error codes and structures.
    #[serde(rename = "custom")]
    Custom {
        /// Application-specific error code (e.g., "egregore.signal_failed").
        code: String,
        /// Human-readable error message.
        message: String,
        /// Optional structured data for debugging/handling.
        #[serde(skip_serializing_if = "Option::is_none")]
        data: Option<Value>,
    },
}

impl std::fmt::Display for EffectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EffectError::NotFound { resource } => write!(f, "Not found: {}", resource),
            EffectError::InvalidInput { message } => write!(f, "Invalid input: {}", message),
            EffectError::NetworkError { message } => write!(f, "Network error: {}", message),
            EffectError::PermissionDenied { message } => {
                write!(f, "Permission denied: {}", message)
            }
            EffectError::Timeout { message } => write!(f, "Timeout: {}", message),
            EffectError::Custom { code, message, .. } => write!(f, "[{}] {}", code, message),
        }
    }
}

impl std::error::Error for EffectError {}

/// Extension trait for converting `Result<T, E>` into `Result<T, EffectError>`.
///
/// Eliminates the ubiquitous `.map_err(|e| EffectError::custom("ns_error", e.to_string()))?`
/// pattern. Instead: `.effect_err("ns")?`
pub trait ResultExt<T> {
    /// Convert error to `EffectError::Custom` with `{namespace}_error` code.
    fn effect_err(self, namespace: &str) -> Result<T, EffectError>;
}

impl<T, E: std::fmt::Display> ResultExt<T> for Result<T, E> {
    fn effect_err(self, namespace: &str) -> Result<T, EffectError> {
        self.map_err(|e| EffectError::custom(format!("{}_error", namespace), e.to_string()))
    }
}

/// Run a blocking closure via `spawn_blocking` and convert both the JoinError
/// and the inner error to `EffectError`.
///
/// Replaces the double `.map_err` pattern:
/// ```ignore
/// spawn_blocking(move || op())
///     .await
///     .map_err(|e| EffectError::custom("ns_error", format!("spawn_blocking: {}", e)))?
///     .map_err(|e| EffectError::custom("ns_error", e.to_string()))?
/// ```
/// With: `spawn_blocking_effect("ns", move || op()).await?`
pub async fn spawn_blocking_effect<T, E>(
    namespace: &str,
    f: impl FnOnce() -> Result<T, E> + Send + 'static,
) -> Result<T, EffectError>
where
    T: Send + 'static,
    E: std::fmt::Display + Send + 'static,
{
    let ns = namespace.to_string();
    tokio::task::spawn_blocking(f)
        .await
        .map_err(|e| {
            EffectError::custom(format!("{}_error", ns), format!("spawn_blocking: {}", e))
        })?
        .map_err(|e| EffectError::custom(format!("{}_error", ns), e.to_string()))
}

impl EffectError {
    /// Create a not found error.
    pub fn not_found(resource: impl Into<String>) -> Self {
        EffectError::NotFound {
            resource: resource.into(),
        }
    }

    /// Create an invalid input error.
    pub fn invalid_input(message: impl Into<String>) -> Self {
        EffectError::InvalidInput {
            message: message.into(),
        }
    }

    /// Create a network error.
    pub fn network_error(message: impl Into<String>) -> Self {
        EffectError::NetworkError {
            message: message.into(),
        }
    }

    /// Create a permission denied error.
    pub fn permission_denied(message: impl Into<String>) -> Self {
        EffectError::PermissionDenied {
            message: message.into(),
        }
    }

    /// Create a timeout error.
    pub fn timeout(message: impl Into<String>) -> Self {
        EffectError::Timeout {
            message: message.into(),
        }
    }

    /// Create a custom error with code and message.
    pub fn custom(code: impl Into<String>, message: impl Into<String>) -> Self {
        EffectError::Custom {
            code: code.into(),
            message: message.into(),
            data: None,
        }
    }

    /// Create a custom error with additional data.
    pub fn custom_with_data(
        code: impl Into<String>,
        message: impl Into<String>,
        data: Value,
    ) -> Self {
        EffectError::Custom {
            code: code.into(),
            message: message.into(),
            data: Some(data),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_serialization() {
        let err = EffectError::NotFound {
            resource: "issue/123".to_string(),
        };
        let json = serde_json::to_string(&err).unwrap();
        assert!(json.contains("not_found"));
        assert!(json.contains("issue/123"));
    }

    #[test]
    fn test_custom_error_with_data() {
        let err = EffectError::custom_with_data(
            "egregore.signal_failed",
            "Signal propagation failed",
            serde_json::json!({"retry_count": 3}),
        );
        let json = serde_json::to_string(&err).unwrap();
        assert!(json.contains("custom"));
        assert!(json.contains("egregore.signal_failed"));
        assert!(json.contains("retry_count"));
    }

    #[test]
    fn test_error_display() {
        let err = EffectError::not_found("file/README.md");
        assert_eq!(err.to_string(), "Not found: file/README.md");
    }
}
