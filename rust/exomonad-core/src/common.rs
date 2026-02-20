//! Common error types used by services.

pub use crate::ffi::FFIBoundary;
use thiserror::Error;

/// Error type for command execution failures.
#[derive(Debug, Error)]
pub enum CommandError {
    /// The command was executed but returned a non-zero exit code.
    #[error("Command '{command}' failed with exit code {exit_code:?}: {stderr}")]
    ExecutionFailed {
        command: String,
        exit_code: Option<i32>,
        stderr: String,
        stdout: String,
    },
    /// The command failed to launch (e.g., binary not found).
    #[error("Failed to execute '{command}': {message}")]
    LaunchFailed { command: String, message: String },
}

/// Error type for operation timeouts.
#[derive(Debug, Error)]
#[error("{message}")]
pub struct TimeoutError {
    pub message: String,
}
