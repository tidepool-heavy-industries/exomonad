//! Command executor trait.
//!
//! This module defines the DockerExecutor trait (historical name - should be renamed to CommandExecutor).
//! Both DockerService (removed) and LocalExecutor implemented this trait.

use anyhow::Result;
use std::future::Future;
use std::pin::Pin;

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
