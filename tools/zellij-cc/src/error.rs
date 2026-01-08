//! Error types for zellij-cc.
//!
//! Provides typed errors for all failure modes, replacing anyhow with
//! structured error handling.

use std::io;
use std::path::PathBuf;
use std::time::Duration;
use thiserror::Error;

/// All error types that can occur in zellij-cc operations.
#[derive(Debug, Error)]
pub enum ZellijCcError {
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

    /// Zellij client error.
    #[error("zellij client error: {0}")]
    Zellij(String),

    /// Poll system call error.
    #[error("poll error: {0}")]
    Poll(#[source] nix::Error),

    /// Generic I/O error (for cases not covered above).
    #[error("I/O error: {0}")]
    Io(#[source] io::Error),
}

/// Result type alias using ZellijCcError.
pub type Result<T> = std::result::Result<T, ZellijCcError>;

impl From<serde_json::Error> for ZellijCcError {
    fn from(e: serde_json::Error) -> Self {
        ZellijCcError::JsonParse { source: e }
    }
}

impl From<nix::Error> for ZellijCcError {
    fn from(e: nix::Error) -> Self {
        ZellijCcError::Poll(e)
    }
}

impl From<std::io::Error> for ZellijCcError {
    fn from(e: std::io::Error) -> Self {
        ZellijCcError::Io(e)
    }
}
