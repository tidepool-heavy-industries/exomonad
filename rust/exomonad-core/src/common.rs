pub use crate::ffi::{ErrorCode, ErrorContext, FFIBoundary, FFIError, FFIResult};
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
        stdout: String, // Useful for some cases even on failure
    },
    /// The command failed to launch (e.g., binary not found).
    #[error("Failed to execute '{command}': {message}")]
    LaunchFailed { command: String, message: String },
}

/// Error type for operation timeouts.
#[derive(Debug, Error)]
#[error("{message}")]
pub struct TimeoutError {
    /// Human-readable description of the timeout.
    pub message: String,
}

/// Type alias for FFIResult used in the runtime host.
pub type HostResult<T> = FFIResult<T>;
/// Type alias for FFIError used in the runtime host.
pub type HostError = FFIError;

/// Trait to convert results (e.g. anyhow::Result) into FFIResult.
pub trait IntoFFIResult<T> {
    /// Convert this value into an `FFIResult`, mapping any error into an `FFIError`
    /// according to the implementing type (for example, turning `Ok` into `Success`
    /// and `Err` into an error with an appropriate `ErrorCode` and `ErrorContext`).
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
                                message: format!(
                                    "Failed to launch command '{}': {}",
                                    command, message
                                ),
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
                            "Try increasing the timeout or checking network connection."
                                .to_string(),
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
                            "Try increasing the timeout or checking network connection."
                                .to_string(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::anyhow;

    #[test]
    fn test_into_ffi_result_ok() {
        let res: anyhow::Result<String> = Ok("success".to_string());
        let ffi_res = res.into_ffi_result();
        assert_eq!(ffi_res, FFIResult::Success("success".to_string()));
    }

    #[test]
    fn test_into_ffi_result_git_error() {
        let err = CommandError::ExecutionFailed {
            command: "git status".to_string(),
            exit_code: Some(128),
            stderr: "fatal: not a git repository".to_string(),
            stdout: "".to_string(),
        };
        let res: anyhow::Result<()> = Err(anyhow!(err));
        let ffi_res = res.into_ffi_result();

        if let FFIResult::Error(ffi_err) = ffi_res {
            assert_eq!(ffi_err.code, ErrorCode::GitError);
            assert!(ffi_err.message.contains("git status"));
            let context = ffi_err.context.unwrap();
            assert_eq!(context.command, Some("git status".to_string()));
            assert_eq!(context.exit_code, Some(128));
        } else {
            panic!("Expected FFIResult::Error");
        }
    }

    #[test]
    fn test_into_ffi_result_io_error() {
        let err = CommandError::ExecutionFailed {
            command: "ls /nonexistent".to_string(),
            exit_code: Some(2),
            stderr: "ls: cannot access '/nonexistent': No such file or directory".to_string(),
            stdout: "".to_string(),
        };
        let res: anyhow::Result<()> = Err(anyhow!(err));
        let ffi_res = res.into_ffi_result();

        if let FFIResult::Error(ffi_err) = ffi_res {
            assert_eq!(ffi_err.code, ErrorCode::IoError);
            assert!(ffi_err.message.contains("ls /nonexistent"));
        } else {
            panic!("Expected FFIResult::Error");
        }
    }

    #[test]
    fn test_into_ffi_result_launch_failed() {
        let err = CommandError::LaunchFailed {
            command: "invalid-command".to_string(),
            message: "No such file or directory".to_string(),
        };
        let res: anyhow::Result<()> = Err(anyhow!(err));
        let ffi_res = res.into_ffi_result();

        if let FFIResult::Error(ffi_err) = ffi_res {
            assert_eq!(ffi_err.code, ErrorCode::IoError);
            assert!(ffi_err.message.contains("Failed to launch command"));
            assert_eq!(
                ffi_err.context.unwrap().command,
                Some("invalid-command".to_string())
            );
        } else {
            panic!("Expected FFIResult::Error");
        }
    }

    #[test]
    fn test_into_ffi_result_timeout_error() {
        let err = TimeoutError {
            message: "Operation timed out after 30s".to_string(),
        };
        let res: anyhow::Result<()> = Err(anyhow!(err));
        let ffi_res = res.into_ffi_result();

        if let FFIResult::Error(ffi_err) = ffi_res {
            assert_eq!(ffi_err.code, ErrorCode::Timeout);
            assert_eq!(ffi_err.message, "Operation timed out after 30s");
            assert!(ffi_err.suggestion.is_some());
        } else {
            panic!("Expected FFIResult::Error");
        }
    }

    #[test]
    fn test_into_ffi_result_anyhow_timeout_string() {
        let res: anyhow::Result<()> = Err(anyhow!("the connection timed out"));
        let ffi_res = res.into_ffi_result();

        if let FFIResult::Error(ffi_err) = ffi_res {
            assert_eq!(ffi_err.code, ErrorCode::Timeout);
            assert!(ffi_err.message.contains("timed out"));
        } else {
            panic!("Expected FFIResult::Error");
        }
    }

    #[test]
    fn test_into_ffi_result_generic_error() {
        let res: anyhow::Result<()> = Err(anyhow!("something went wrong"));
        let ffi_res = res.into_ffi_result();

        if let FFIResult::Error(ffi_err) = ffi_res {
            assert_eq!(ffi_err.code, ErrorCode::InternalError);
            assert_eq!(ffi_err.message, "something went wrong");
        } else {
            panic!("Expected FFIResult::Error");
        }
    }
}
