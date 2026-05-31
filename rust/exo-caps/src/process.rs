//! `Process` capability — ad-hoc command execution. Returns the std library's
//! [`std::process::Output`] (which carries [`std::process::ExitStatus`] — the proper
//! exit-status type, `.success()`/`.code()`/`.signal()` — and raw `Vec<u8>` stdout/stderr,
//! since process output isn't guaranteed UTF-8). No hand-rolled type: this is exactly
//! what `Command::output()` yields, so the runtime impl produces it for free.
//!
//! The argument surface (cwd / env / timeout) firms up in Wave 1.

use crate::error::CapResult;
use async_trait::async_trait;

#[async_trait]
pub trait Process {
    async fn run(&self, program: &str, args: &[String]) -> CapResult<std::process::Output>;
}
