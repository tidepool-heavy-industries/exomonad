//! Extensible effects system for external consumers.
//!
//! This module provides the infrastructure for external repos to define custom effects
//! that their Rust host interprets, without forking exomonad.
//!
//! # Architecture
//!
//! ```text
//! WASM Guest (Haskell)
//!     │
//!     │ yieldEffect "egregore.emit_signal" {...}
//!     ▼
//! yield_effect host function
//!     │
//!     │ routes by namespace prefix
//!     ▼
//! EffectRegistry
//!     ├── "git.*"       → GitHandler (builtin)
//!     ├── "github.*"    → GitHubHandler (builtin)
//!     └── "egregore.*"  → EgregoreHandler (user-provided)
//! ```
//!
//! # Usage
//!
//! External consumers implement [`EffectHandler`] for their domain:
//!
//! ```rust,ignore
//! use exomonad_runtime::effects::{EffectHandler, EffectError, EffectResult};
//! use async_trait::async_trait;
//!
//! struct EgregoreHandler { /* ... */ }
//!
//! #[async_trait]
//! impl EffectHandler for EgregoreHandler {
//!     fn namespace(&self) -> &str { "egregore" }
//!
//!     async fn handle(&self, effect_type: &str, payload: Value) -> EffectResult<Value> {
//!         match effect_type {
//!             "egregore.emit_signal" => { /* ... */ }
//!             _ => Err(EffectError::not_found(effect_type))
//!         }
//!     }
//! }
//! ```
//!
//! Then register it with the runtime:
//!
//! ```rust,ignore
//! RuntimeBuilder::new()
//!     .with_effect_handler(EgregoreHandler::new())
//!     .with_wasm_path("plugin.wasm")
//!     .build()
//! ```

pub mod error;
pub mod host_fn;

pub use error::EffectError;

use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

/// Result type for effect handlers.
pub type EffectResult<T> = Result<T, EffectError>;

/// Trait for effect handlers - implemented per namespace.
///
/// Effect handlers are registered with the [`EffectRegistry`] and dispatched
/// based on the effect type prefix (namespace).
///
/// # Design Notes
///
/// - Handlers are async to support I/O operations
/// - Each handler owns a namespace (e.g., "egregore", "git")
/// - Effect types are fully qualified (e.g., "egregore.emit_signal")
/// - Payloads are JSON for flexibility across the WASM boundary
///
/// # Example
///
/// ```rust,ignore
/// use exomonad_runtime::effects::{EffectHandler, EffectResult};
/// use async_trait::async_trait;
/// use serde_json::Value;
///
/// struct MyHandler;
///
/// #[async_trait]
/// impl EffectHandler for MyHandler {
///     fn namespace(&self) -> &str { "my_domain" }
///
///     async fn handle(&self, effect_type: &str, payload: Value) -> EffectResult<Value> {
///         // Dispatch based on effect_type suffix
///         let suffix = effect_type.strip_prefix("my_domain.").unwrap_or(effect_type);
///         match suffix {
///             "do_thing" => Ok(serde_json::json!({"done": true})),
///             _ => Err(EffectError::not_found(effect_type)),
///         }
///     }
/// }
/// ```
#[async_trait]
pub trait EffectHandler: Send + Sync {
    /// Namespace prefix this handler owns (e.g., "egregore", "git").
    ///
    /// Effect types starting with "{namespace}." will be routed to this handler.
    fn namespace(&self) -> &str;

    /// Handle an effect request.
    ///
    /// # Arguments
    ///
    /// * `effect_type` - Full effect type including namespace (e.g., "egregore.emit_signal")
    /// * `payload` - Effect-specific JSON payload
    ///
    /// # Returns
    ///
    /// JSON response value, or an error.
    async fn handle(&self, effect_type: &str, payload: Value) -> EffectResult<Value>;
}

/// Registry for effect handlers.
///
/// The registry maps namespace prefixes to handlers and dispatches
/// effect requests accordingly.
///
/// # Thread Safety
///
/// The registry is immutable after construction. Handlers are stored
/// as `Arc<dyn EffectHandler>` for cheap cloning and concurrent access.
///
/// # Example
///
/// ```rust,ignore
/// let mut registry = EffectRegistry::new();
/// registry.register(Arc::new(MyHandler));
///
/// // Dispatch an effect
/// let result = registry.dispatch("my_domain.do_thing", payload).await?;
/// ```
pub struct EffectRegistry {
    /// Map from namespace prefix to handler.
    handlers: HashMap<String, Arc<dyn EffectHandler>>,
}

impl EffectRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    /// Register an effect handler for its namespace.
    ///
    /// # Panics
    ///
    /// Panics if a handler is already registered for this namespace.
    /// This is intentional to catch configuration errors at startup.
    pub fn register(&mut self, handler: Arc<dyn EffectHandler>) {
        let namespace = handler.namespace().to_string();
        if self.handlers.contains_key(&namespace) {
            panic!(
                "Effect handler already registered for namespace: {}",
                namespace
            );
        }
        tracing::info!(namespace = %namespace, "Registered effect handler");
        self.handlers.insert(namespace, handler);
    }

    /// Register a handler, taking ownership.
    pub fn register_owned(&mut self, handler: impl EffectHandler + 'static) {
        self.register(Arc::new(handler));
    }

    /// Dispatch an effect to the appropriate handler.
    ///
    /// Routes based on namespace prefix extracted from the effect type.
    ///
    /// # Arguments
    ///
    /// * `effect_type` - Full effect type (e.g., "egregore.emit_signal")
    /// * `payload` - Effect-specific JSON payload
    ///
    /// # Returns
    ///
    /// Handler result, or `NotFound` if no handler matches.
    pub async fn dispatch(&self, effect_type: &str, payload: Value) -> EffectResult<Value> {
        // Extract namespace from effect_type (everything before first '.')
        let namespace = effect_type
            .split('.')
            .next()
            .ok_or_else(|| EffectError::invalid_input("Effect type must contain namespace prefix"))?;

        let handler = self
            .handlers
            .get(namespace)
            .ok_or_else(|| EffectError::not_found(format!("handler/{}", namespace)))?;

        tracing::debug!(
            effect_type = %effect_type,
            namespace = %namespace,
            "Dispatching effect"
        );

        handler.handle(effect_type, payload).await
    }

    /// Check if a handler is registered for a namespace.
    pub fn has_handler(&self, namespace: &str) -> bool {
        self.handlers.contains_key(namespace)
    }

    /// Get the list of registered namespaces.
    pub fn namespaces(&self) -> Vec<&str> {
        self.handlers.keys().map(|s| s.as_str()).collect()
    }
}

impl Default for EffectRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestHandler {
        ns: String,
    }

    impl TestHandler {
        fn new(ns: &str) -> Self {
            Self { ns: ns.to_string() }
        }
    }

    #[async_trait]
    impl EffectHandler for TestHandler {
        fn namespace(&self) -> &str {
            &self.ns
        }

        async fn handle(&self, effect_type: &str, payload: Value) -> EffectResult<Value> {
            Ok(serde_json::json!({
                "handled_by": self.ns,
                "effect_type": effect_type,
                "payload": payload
            }))
        }
    }

    #[tokio::test]
    async fn test_registry_dispatch() {
        let mut registry = EffectRegistry::new();
        registry.register_owned(TestHandler::new("test"));

        let result = registry
            .dispatch("test.do_thing", serde_json::json!({"arg": 1}))
            .await
            .unwrap();

        assert_eq!(result["handled_by"], "test");
        assert_eq!(result["effect_type"], "test.do_thing");
    }

    #[tokio::test]
    async fn test_registry_not_found() {
        let registry = EffectRegistry::new();

        let result = registry
            .dispatch("unknown.effect", serde_json::json!({}))
            .await;

        assert!(result.is_err());
        if let Err(EffectError::NotFound { resource }) = result {
            assert!(resource.contains("unknown"));
        } else {
            panic!("Expected NotFound error");
        }
    }

    #[test]
    fn test_registry_namespaces() {
        let mut registry = EffectRegistry::new();
        registry.register_owned(TestHandler::new("alpha"));
        registry.register_owned(TestHandler::new("beta"));

        let mut namespaces = registry.namespaces();
        namespaces.sort();
        assert_eq!(namespaces, vec!["alpha", "beta"]);
    }

    #[test]
    #[should_panic(expected = "already registered")]
    fn test_duplicate_registration_panics() {
        let mut registry = EffectRegistry::new();
        registry.register_owned(TestHandler::new("test"));
        registry.register_owned(TestHandler::new("test")); // Should panic
    }
}
