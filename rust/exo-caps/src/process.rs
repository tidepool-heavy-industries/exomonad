//! `Process` capability — ad-hoc command execution. Returns the std library's
//! [`std::process::Output`] (which carries [`std::process::ExitStatus`] — the proper
//! exit-status type, `.success()`/`.code()`/`.signal()` — and raw `Vec<u8>` stdout/stderr,
//! since process output isn't guaranteed UTF-8). No hand-rolled type: this is exactly
//! what `Command::output()` yields, so the runtime impl produces it for free.
//!
//! The argument surface (cwd / env / timeout) firms up in Wave 1.

use async_trait::async_trait;
use std::time::Duration;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ProcessError {
    #[error("failed to spawn {program}: {source}")]
    Spawn {
        program: String,
        #[source]
        source: std::io::Error,
    },
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

/// The result of a bounded-duration process run — see [`Process::run_with_timeout`].
#[derive(Debug)]
pub enum ProcessOutcome {
    /// The process exited (successfully or not) before the timeout elapsed.
    Completed(std::process::Output),
    /// The timeout elapsed first; the process's whole process group was killed. Partial output is
    /// `Some` only when cheaply available at the kill point — an impl that has none to offer
    /// returns `None` rather than fabricating an empty `Output`.
    TimedOut {
        partial_output: Option<std::process::Output>,
    },
}

#[async_trait]
pub trait Process {
    async fn run(
        &self,
        program: &str,
        args: &[String],
    ) -> Result<std::process::Output, ProcessError>;

    /// Bounded-duration variant of [`run`](Process::run). On expiry the runtime kills the
    /// process's entire **process group**, not just the direct child — a command that forks
    /// grandchildren (a build wrapper, a shell pipeline) must not leak them. No default impl: a
    /// naive `tokio::time::timeout` wrapped around `run` cannot kill anything it doesn't hold a
    /// handle to, so every impl provides its own kill-capable body.
    async fn run_with_timeout(
        &self,
        program: &str,
        args: &[String],
        timeout: Duration,
    ) -> Result<ProcessOutcome, ProcessError>;
}
