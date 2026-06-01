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
        program: &str,
        args: &[String],
    ) -> Result<std::process::Output, ProcessError> {
        tokio::process::Command::new(program)
            .args(args)
            .output()
            .await
            .map_err(|e| ProcessError::Spawn {
                program: program.to_string(),
                source: e,
            })
    }
}
