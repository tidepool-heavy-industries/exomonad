//! Command executor trait for running subprocess commands.

use anyhow::Result;
use std::future::Future;
use std::pin::Pin;

/// Trait for executing commands in a specific directory.
pub trait CommandExecutor: Send + Sync {
    fn exec<'a>(
        &'a self,
        dir: &'a str,
        cmd: &'a [&'a str],
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>>;
}
