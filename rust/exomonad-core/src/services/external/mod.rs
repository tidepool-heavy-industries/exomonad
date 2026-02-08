//! External service clients for LLM providers, GitHub, and observability.
//!
//! Provides standalone clients for external APIs:
//! - **Anthropic**: Claude chat completions
//! - **GitHub**: Issue and PR management via octocrab
//! - **Ollama**: Local LLM generation
//! - **OTLP**: OpenTelemetry traces and metrics export

use async_trait::async_trait;

pub mod anthropic;
pub mod github;
pub mod ollama;
pub mod otel;

pub use anthropic::AnthropicService;
pub use github::GitHubService as ExternalGitHubService;
pub use ollama::OllamaService;
pub use otel::OtelService;

/// A common trait for all external service clients.
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
