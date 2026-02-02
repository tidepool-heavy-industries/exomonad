//! Command executor trait.
//!
//! This module defines the DockerExecutor trait (historical name - should be renamed to CommandExecutor).
//! Both DockerService (removed) and LocalExecutor implemented this trait.

use anyhow::Result;
use std::future::Future;
use std::pin::Pin;
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

/// Trait for executing commands in a specific directory.
///
/// Historical note: Originally designed for Docker containers (hence the name),
/// but now only used by LocalExecutor. The `container` parameter is ignored
/// in local execution mode.
///
/// TODO: Rename to CommandExecutor and remove container parameter.
pub trait DockerExecutor: Send + Sync {
    fn exec<'a>(
        &'a self,
        container: &'a str,
        dir: &'a str,
        cmd: &'a [&'a str],
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>>;
}
