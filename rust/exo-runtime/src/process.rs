//! `impl Process for Runtime` — ad-hoc command execution.
//!
//! **Leaf R3.** Trivial: `tokio::process::Command::new(program).args(args).output().await`,
//! mapping a spawn failure to `ProcessError::Spawn`. Returns `std::process::Output` directly
//! (what `Command::output()` yields — no hand-rolled type).

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{Process, ProcessError, ProcessOutcome};
use std::process::Stdio;
use std::time::Duration;
use tracing::{debug, warn};

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

    async fn run_with_timeout(
        &self,
        program: &str,
        args: &[String],
        timeout: Duration,
    ) -> Result<ProcessOutcome, ProcessError> {
        debug!(program = %program, args = ?args, timeout_ms = timeout.as_millis(), "process: exec with timeout");
        // `process_group(0)` puts the child in a NEW process group (pgid == its own pid), so a
        // timeout's killpg only ever hits this command's own tree, never ours or a sibling's.
        let child = tokio::process::Command::new(program)
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .process_group(0)
            .spawn()
            .map_err(|e| ProcessError::Spawn {
                program: program.to_string(),
                source: e,
            })?;
        let pid = child.id();

        match tokio::time::timeout(timeout, child.wait_with_output()).await {
            Ok(result) => {
                let out = result?;
                debug!(program = %program, status = ?out.status, "process: exec with timeout completed");
                Ok(ProcessOutcome::Completed(out))
            }
            Err(_elapsed) => {
                warn!(
                    program = %program,
                    timeout_ms = timeout.as_millis(),
                    "process: timed out, killing process group"
                );
                // The timed-out future above owned `child` and was dropped when the timeout
                // fired — tokio reaps a dropped, unwaited `Child` in the background (its process
                // driver keeps polling SIGCHLD independent of whether anyone holds the handle),
                // so no explicit wait() is needed here to avoid a zombie. `pid` was captured
                // before that drop, which is all `killpg` needs.
                if let Some(pid) = pid {
                    let pgid = nix::unistd::Pid::from_raw(pid as i32);
                    if let Err(e) =
                        nix::sys::signal::killpg(pgid, nix::sys::signal::Signal::SIGKILL)
                    {
                        warn!(
                            program = %program,
                            pid,
                            error = %e,
                            "process: failed to kill process group after timeout"
                        );
                    }
                } else {
                    warn!(program = %program, "process: no pid to kill after timeout (already reaped?)");
                }
                Ok(ProcessOutcome::TimedOut {
                    partial_output: None,
                })
            }
        }
    }
}
