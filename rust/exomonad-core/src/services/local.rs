//! Local executor for running commands directly as subprocesses.

use crate::common::CommandError;
use crate::services::command::CommandExecutor;
use anyhow::Result;
use std::future::Future;
use std::pin::Pin;
use std::process::Stdio;
use tokio::process::Command;
use tracing::{debug, trace};

/// Local executor that runs commands directly via subprocess.
#[derive(Clone, Default)]
pub struct LocalExecutor;

impl LocalExecutor {
    pub fn new() -> Self {
        Self
    }
}

impl CommandExecutor for LocalExecutor {
    fn exec<'a>(
        &'a self,
        dir: &'a str,
        cmd: &'a [&'a str],
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>> {
        let dir = dir.to_string();
        let cmd: Vec<String> = cmd.iter().map(|s| s.to_string()).collect();

        Box::pin(async move {
            if cmd.is_empty() {
                return Err(anyhow::anyhow!("Empty command"));
            }

            let program = &cmd[0];
            let args = &cmd[1..];
            let command_str = cmd.join(" ");

            debug!(
                program = program,
                args = ?args,
                dir = dir,
                "LocalExecutor: executing command"
            );

            let output = Command::new(program)
                .args(args)
                .current_dir(&dir)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output()
                .await
                .map_err(|e| CommandError::LaunchFailed {
                    command: command_str.clone(),
                    message: e.to_string(),
                })?;

            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();

            trace!(
                exit_code = output.status.code(),
                stdout_len = stdout.len(),
                stderr_len = stderr.len(),
                "LocalExecutor: command completed"
            );

            if !output.status.success() {
                return Err(CommandError::ExecutionFailed {
                    command: command_str,
                    exit_code: output.status.code(),
                    stderr,
                    stdout,
                }
                .into());
            }

            Ok(stdout)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_local_executor_echo() {
        let executor = LocalExecutor::new();
        let result = executor.exec("/tmp", &["echo", "hello"]).await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap().trim(), "hello");
    }

    #[tokio::test]
    async fn test_local_executor_pwd() {
        let executor = LocalExecutor::new();
        let result = executor.exec("/tmp", &["pwd"]).await;
        assert!(result.is_ok());
        // macOS: /tmp is a symlink to /private/tmp, so pwd returns /private/tmp
        let output = result.unwrap();
        assert!(
            output.trim().ends_with("/tmp"),
            "Expected path ending in /tmp, got: {}",
            output.trim()
        );
    }

    #[tokio::test]
    async fn test_local_executor_git_version() {
        let executor = LocalExecutor::new();
        let result = executor.exec("/tmp", &["git", "--version"]).await;
        assert!(result.is_ok());
        assert!(result.unwrap().contains("git version"));
    }

    #[tokio::test]
    async fn test_local_executor_failure() {
        let executor = LocalExecutor::new();
        let result = executor
            .exec("/tmp", &["false"]) // `false` always exits 1
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_local_executor_empty_command() {
        let executor = LocalExecutor::new();
        let result = executor.exec("/tmp", &[]).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Empty command"));
    }
}
