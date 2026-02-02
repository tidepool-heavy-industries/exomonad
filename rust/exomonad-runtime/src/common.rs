use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CommandError {
    #[error("Command '{command}' failed with exit code {exit_code:?}: {stderr}")]
    ExecutionFailed {
        command: String,
        exit_code: Option<i32>,
        stderr: String,
        stdout: String, // Useful for some cases even on failure
    },
    #[error("Failed to execute '{command}': {message}")]
    LaunchFailed {
        command: String,
        message: String,
    },
}

/// Standardized error code for programmatic handling across the FFI boundary.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ErrorCode {
    /// Resource (file, issue, branch, etc.) does not exist.
    NotFound,
    /// Missing or invalid credentials (e.g., GitHub token).
    NotAuthenticated,
    /// Git command failed (e.g., merge conflict, dirty working directory).
    GitError,
    /// File system operation failed (e.g., permission denied).
    IoError,
    /// Network request failed (e.g., API unreachable).
    NetworkError,
    /// Invalid input parameters provided to the host function.
    InvalidInput,
    /// Unexpected internal error (bug in the host function or runtime).
    InternalError,
    /// Operation timed out.
    Timeout,
    /// Resource already exists (e.g., worktree path).
    AlreadyExists,
}

impl Default for ErrorCode {
    fn default() -> Self {
        Self::InternalError
    }
}

/// Rich context for debugging errors.
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct ErrorContext {
    /// The command that failed (e.g., "git worktree add ...").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command: Option<String>,
    
    /// Process exit code, if applicable.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exit_code: Option<i32>,
    
    /// Standard error output from the command (truncated if necessary).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stderr: Option<String>,

    /// Standard output from the command (truncated if necessary).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stdout: Option<String>,
    
    /// Relevant file path.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file_path: Option<String>,
    
    /// Working directory where the operation was attempted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub working_dir: Option<String>,
}

/// Structured error returned to the WASM guest.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct HostError {
    /// Human-readable summary of the error.
    pub message: String,
    
    /// Programmatic error code.
    pub code: ErrorCode,
    
    /// Rich context for debugging.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<ErrorContext>,
    
    /// Actionable suggestion for recovery.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suggestion: Option<String>,
}

/// Standardized result envelope for all host functions.
#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "kind", content = "payload")]
pub enum HostResult<T> {
    Success(T),
    Error(HostError),
}

impl<T> From<anyhow::Error> for HostResult<T> {
    fn from(e: anyhow::Error) -> Self {
        // Try to downcast to CommandError
        if let Some(cmd_error) = e.downcast_ref::<CommandError>() {
            match cmd_error {
                CommandError::ExecutionFailed {
                    command,
                    exit_code,
                    stderr,
                    stdout,
                } => {
                    // Heuristically map command failures: git commands -> GitError, others -> IoError.
                    let inferred_code = {
                        let trimmed = command.trim_start();
                        if trimmed == "git" || trimmed.starts_with("git ") {
                            ErrorCode::GitError
                        } else {
                            ErrorCode::IoError
                        }
                    };

                    return HostResult::Error(HostError {
                        message: format!("Command failed: {}", command),
                        code: inferred_code,
                        context: Some(ErrorContext {
                            command: Some(command.clone()),
                            exit_code: *exit_code,
                            stderr: Some(stderr.clone()),
                            stdout: Some(stdout.clone()),
                            ..Default::default()
                        }),
                        suggestion: None, // Can be improved with heuristic analysis of stderr
                    });
                }
                CommandError::LaunchFailed { command, message } => {
                     return HostResult::Error(HostError {
                        message: format!("Failed to launch command '{}': {}", command, message),
                        code: ErrorCode::IoError,
                        context: Some(ErrorContext {
                            command: Some(command.clone()),
                            ..Default::default()
                        }),
                        suggestion: None,
                    });
                }
            }
        }

        // Default to InternalError
        HostResult::Error(HostError {
            message: e.to_string(),
            code: ErrorCode::InternalError,
            context: None,
            suggestion: None,
        })
    }
}

impl<T> From<anyhow::Result<T>> for HostResult<T> {
    fn from(res: anyhow::Result<T>) -> Self {
        match res {
            Ok(val) => HostResult::Success(val),
            Err(e) => e.into(),
        }
    }
}

impl<T> HostResult<T> {
    /// Create a success result.
    pub fn success(value: T) -> Self {
        Self::Success(value)
    }

    /// Create an error result with full details.
    pub fn error(
        message: impl Into<String>,
        code: ErrorCode,
        context: Option<ErrorContext>,
        suggestion: Option<String>,
    ) -> Self {
        Self::Error(HostError {
            message: message.into(),
            code,
            context,
            suggestion,
        })
    }

    /// Create a simple error result.
    pub fn simple_error(message: impl Into<String>, code: ErrorCode) -> Self {
        Self::Error(HostError {
            message: message.into(),
            code,
            context: None,
            suggestion: None,
        })
    }
}

