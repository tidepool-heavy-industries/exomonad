//! `exomonad-services` provides standalone clients for external services used by the ExoMonad system.
//!
//! This crate implements the `ExternalService` trait for various providers:
//! - **Anthropic**: LLM chat completions (Claude).
//! - **GitHub**: Issue and PR management.
//! - **Ollama**: Local LLM generation.
//! - **OTLP**: OpenTelemetry traces and metrics export.
//!
//! # Example
//!
//! ```rust,no_run
//! use exomonad_services::{ExternalService, ServiceError};
//! use exomonad_services::github::GitHubService;
//! use exomonad_services::AnthropicService;
//! use exomonad_shared::protocol::ServiceRequest;
//!
//! let api_key = "sk-ant-api03-...";
//!
//! # async fn run() -> Result<(), ServiceError> {
//! let github = GitHubService::new("token".to_string());
//! let req = ServiceRequest::GitHubGetIssue {
//!     owner: "owner".into(),
//!     repo: "repo".into(),
//!     number: 1,
//!     include_comments: false,
//! };
//! let res = github.call(req).await?;
//! # Ok(())
//! # }
//! ```

use async_trait::async_trait;

pub mod anthropic;
pub mod github;
pub mod ollama;
pub mod otel;
pub mod server;

pub use anthropic::AnthropicService;
pub use github::GitHubService;
pub use ollama::OllamaService;
pub use otel::OtelService;
pub use server::ServiceServer;

/// A common trait for all external service clients.
///
/// This trait abstracts over the underlying service implementation, allowing
/// uniform handling of requests and responses via the `ServiceRequest` and
/// `ServiceResponse` enums defined in `exomonad-shared`.
#[async_trait]
pub trait ExternalService {
    type Request;
    type Response;

    /// Execute the service request.
    async fn call(&self, req: Self::Request) -> Result<Self::Response, ServiceError>;
}

/// Errors that can occur during service execution.
#[derive(Debug, thiserror::Error)]
pub enum ServiceError {
    /// Low-level HTTP client error (reqwest).
    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),

    /// API-level error returned by the service (non-2xx response).
    #[error("API error: {code} - {message}")]
    Api { code: i32, message: String },

    /// Rate limit exceeded.
    #[error("Rate limited, retry after {retry_after_ms}ms")]
    RateLimited { retry_after_ms: u64 },

    /// Operation timed out.
    #[error("Timeout after {0}ms")]
    Timeout(u64),
}
