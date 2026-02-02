//! Error types for exomonad.
//!
//! Provides typed errors for all failure modes, replacing anyhow with
//! structured error handling.

use std::io;
use std::path::PathBuf;
use thiserror::Error;

/// All error types that can occur in exomonad operations.
#[derive(Debug, Error)]
pub enum ExoMonadError {
    /// JSON parse error while processing stream.
    #[error("JSON parse error: {source}")]
    JsonParse {
        #[source]
        source: serde_json::Error,
    },

    /// Failed to serialize result to JSON.
    #[error("JSON serialize error: {0}")]
    JsonSerialize(#[source] serde_json::Error),

    /// Generic I/O error (for cases not covered above).
    #[error("I/O error: {0}")]
    Io(#[source] io::Error),

    // ---- Socket errors (for control envelope) ----
    /// Failed to connect to control server via Unix socket.
    #[error("failed to connect to control server at {path}: {source}")]
    UnixConnect {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    /// Failed to configure socket options.
    #[error("failed to configure socket: {source}")]
    SocketConfig {
        #[source]
        source: io::Error,
    },

    /// Failed to write to socket.
    #[error("failed to write to socket: {source}")]
    SocketWrite {
        #[source]
        source: io::Error,
    },

    /// Failed to read from socket.
    #[error("failed to read from socket: {source}")]
    SocketRead {
        #[source]
        source: io::Error,
    },

    // ---- MCP errors ----
    /// MCP server error.
    #[error("MCP server error: {0}")]
    McpServer(String),

    /// Health check error.
    #[error("health check error: {0}")]
    HealthCheck(String),
}

/// Result type alias using ExoMonadError.
pub type Result<T> = std::result::Result<T, ExoMonadError>;

impl From<serde_json::Error> for ExoMonadError {
    fn from(e: serde_json::Error) -> Self {
        Self::JsonParse { source: e }
    }
}

impl From<std::io::Error> for ExoMonadError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}
