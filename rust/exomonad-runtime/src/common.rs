pub use exomonad_shared::ffi::{
    ErrorCode, ErrorContext, FFIError, FFIResult, FFIBoundary,
};
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

#[derive(Debug, Error)]
#[error("{message}")]
pub struct TimeoutError {
    pub message: String,
}

pub type HostResult<T> = FFIResult<T>;
pub type HostError = FFIError;

pub trait IntoFFIResult<T> {
    fn into_ffi_result(self) -> FFIResult<T>;
}

impl<T> IntoFFIResult<T> for anyhow::Result<T> {
    fn into_ffi_result(self) -> FFIResult<T> {
        match self {
            Ok(val) => FFIResult::Success(val),
            Err(e) => {
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
                                if trimmed.starts_with("git ") {
                                    ErrorCode::GitError
                                } else {
                                    ErrorCode::IoError
                                }
                            };

                            return FFIResult::Error(FFIError {
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
                            return FFIResult::Error(FFIError {
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

                // Check for timeout (typed)
                if let Some(timeout_err) = e.downcast_ref::<TimeoutError>() {
                    return FFIResult::Error(FFIError {
                        message: timeout_err.message.clone(),
                        code: ErrorCode::Timeout,
                        context: None,
                        suggestion: Some(
                            "Try increasing the timeout or checking network connection.".to_string(),
                        ),
                    });
                }

                // Check for timeout (legacy string matching for other sources)
                if e.to_string().to_lowercase().contains("timed out") {
                    return FFIResult::Error(FFIError {
                        message: format!("Operation timed out: {}", e),
                        code: ErrorCode::Timeout,
                        context: None,
                        suggestion: Some(
                            "Try increasing the timeout or checking network connection.".to_string(),
                        ),
                    });
                }

                // Default to InternalError
                FFIResult::Error(FFIError {
                    message: e.to_string(),
                    code: ErrorCode::InternalError,
                    context: None,
                    suggestion: None,
                })
            }
        }
    }
}

