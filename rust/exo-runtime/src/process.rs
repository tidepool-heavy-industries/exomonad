//! `impl Process for Runtime` — ad-hoc command execution.
//!
//! **Leaf R3.** Trivial: `tokio::process::Command::new(program).args(args).output().await`,
//! mapping a spawn failure to `ProcessError::Spawn`. Returns `std::process::Output` directly
//! (what `Command::output()` yields — no hand-rolled type).

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Process, ProcessError};
use tracing::debug;

#[async_trait]
impl Process for Runtime {
    async fn run(
        &self,
        program: &str,
        args: &[String],
    ) -> Result<std::process::Output, ProcessError> {
        // DEBUG: generic subprocess boundary — a non-zero exit is returned as `Ok(output)` for the
        // caller to interpret (a failing check script is expected control flow), so only the trace
        // breadcrumb lives here. A spawn failure propagates as `ProcessError::Spawn` for the caller.
        debug!(program = %program, args = ?args, "process: exec");
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
