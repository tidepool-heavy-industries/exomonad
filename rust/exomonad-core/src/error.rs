//! Error types for exomonad.
//!
//! Provides typed errors for all failure modes, replacing anyhow with
//! structured error handling.

use std::io;
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

    /// MCP server error.
    #[error("MCP server error: {0}")]
    McpServer(String),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_from_json() {
        let json_err = serde_json::from_str::<String>("not valid json").unwrap_err();
        let exo_err = ExoMonadError::from(json_err);
        assert!(matches!(exo_err, ExoMonadError::JsonParse { .. }));
    }

    #[test]
    fn test_error_from_io() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let exo_err = ExoMonadError::from(io_err);
        assert!(matches!(exo_err, ExoMonadError::Io(_)));
    }

    #[test]
    fn test_error_json_parse_display() {
        let json_err = serde_json::from_str::<String>("not valid json").unwrap_err();
        let exo_err = ExoMonadError::JsonParse { source: json_err };
        assert!(exo_err.to_string().contains("JSON parse error"));
    }

    #[test]
    fn test_error_io_display() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let exo_err = ExoMonadError::Io(io_err);
        assert!(exo_err.to_string().contains("I/O error"));
    }

    #[test]
    fn test_error_mcp_server_display() {
        let exo_err = ExoMonadError::McpServer("test error".to_string());
        assert!(exo_err.to_string().contains("MCP server error"));
    }
}
