//! `impl Process for Runtime` — ad-hoc command execution.
//!
//! **Leaf R3.** Trivial: `tokio::process::Command::new(program).args(args).output().await`,
//! mapping a spawn failure to `ProcessError::Spawn`. Returns `std::process::Output` directly
//! (what `Command::output()` yields — no hand-rolled type).

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Process, ProcessError};

#[async_trait]
impl Process for Runtime {
    async fn run(
        &self,
        _program: &str,
        _args: &[String],
    ) -> Result<std::process::Output, ProcessError> {
        todo!("R3: tokio::process::Command output().await; map spawn err to ProcessError::Spawn")
    }
}
