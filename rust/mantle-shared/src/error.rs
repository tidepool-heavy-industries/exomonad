//! Error types for mantle.
//!
//! Provides typed errors for all failure modes, replacing anyhow with
//! structured error handling.

use std::io;
use std::path::PathBuf;
use std::time::Duration;
use thiserror::Error;

/// All error types that can occur in mantle operations.
#[derive(Debug, Error)]
pub enum MantleError {
    /// Failed to create a FIFO (named pipe).
    #[error("failed to create FIFO at {path}: {source}")]
    FifoCreate {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    /// Failed to open a FIFO for reading or writing.
    #[error("failed to open FIFO at {path}: {source}")]
    FifoOpen {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    /// Failed to read from a FIFO.
    #[error("failed to read from FIFO at {path}: {source}")]
    FifoRead {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    /// Failed to write to a FIFO.
    #[error("failed to write to FIFO at {path}: {source}")]
    FifoWrite {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    /// Timeout waiting for FIFO data.
    #[error("timeout after {elapsed:?} waiting for FIFO data")]
    FifoTimeout { elapsed: Duration },

    /// Failed to spawn the claude subprocess.
    #[error("failed to spawn claude: {0}")]
    Spawn(#[source] io::Error),

    /// Child process was killed due to timeout.
    #[error("child process killed after {elapsed:?} timeout")]
    ProcessTimeout { elapsed: Duration },

    /// JSON parse error while processing stream.
    #[error("JSON parse error: {source}")]
    JsonParse {
        #[source]
        source: serde_json::Error,
    },

    /// Failed to serialize result to JSON.
    #[error("JSON serialize error: {0}")]
    JsonSerialize(#[source] serde_json::Error),

    /// Signal FIFO communication error.
    #[error("signal error: {0}")]
    Signal(String),

    /// Poll system call error.
    #[error("poll error: {0}")]
    Poll(#[source] nix::Error),

    /// Generic I/O error (for cases not covered above).
    #[error("I/O error: {0}")]
    Io(#[source] io::Error),

    // ---- Socket errors (for control envelope) ----
    /// Failed to connect to control socket.
    #[error("failed to connect to control socket at {path}: {source}")]
    SocketConnect {
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

    // ---- Hub errors ----
    /// Hub communication error.
    #[error("hub error: {0}")]
    Hub(String),

    // ---- MCP errors ----
    /// MCP server error.
    #[error("MCP server error: {0}")]
    McpServer(String),
}

/// Result type alias using MantleError.
pub type Result<T> = std::result::Result<T, MantleError>;

impl From<serde_json::Error> for MantleError {
    fn from(e: serde_json::Error) -> Self {
        MantleError::JsonParse { source: e }
    }
}

impl From<nix::Error> for MantleError {
    fn from(e: nix::Error) -> Self {
        MantleError::Poll(e)
    }
}

impl From<std::io::Error> for MantleError {
    fn from(e: std::io::Error) -> Self {
        MantleError::Io(e)
    }
}
