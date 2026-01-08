//! Process supervision with timeout and signal forwarding.
//!
//! The `Supervisor` manages a child process's lifecycle:
//! - Spawning with environment setup
//! - Timeout enforcement (actually kills the child when timeout fires)
//! - Signal forwarding (SIGINT/SIGTERM forwarded to child)
//! - Cleanup on drop (ensures no orphan processes)
//!
//! ## Signal Handling
//!
//! Uses a global atomic flag to track received signals. The main loop
//! checks this flag and forwards signals to the child process. This
//! approach avoids the complexity of async signal handlers.

use crate::error::{Result, ZellijCcError};
use nix::libc;
use nix::sys::signal::{kill, Signal};
use nix::unistd::Pid;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::time::{Duration, Instant};
use tracing::{debug, info, warn};

// ============================================================================
// Global Signal State
// ============================================================================

/// Global flag indicating a signal was received.
static SIGNAL_RECEIVED: AtomicBool = AtomicBool::new(false);

/// The signal number that was received.
static SIGNAL_NUMBER: AtomicI32 = AtomicI32::new(0);

/// Install signal handlers for SIGINT and SIGTERM.
///
/// Sets a global flag when either signal is received. The Supervisor
/// checks this flag and forwards signals to the child process.
pub fn install_signal_handlers() {
    use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet};

    extern "C" fn signal_handler(signum: libc::c_int) {
        SIGNAL_RECEIVED.store(true, Ordering::Release);
        SIGNAL_NUMBER.store(signum, Ordering::Release);
    }

    let handler = SigHandler::Handler(signal_handler);
    let action = SigAction::new(handler, SaFlags::SA_RESTART, SigSet::empty());

    unsafe {
        // Install handlers for SIGINT and SIGTERM
        if let Err(e) = sigaction(Signal::SIGINT, &action) {
            warn!(error = %e, "Failed to install SIGINT handler");
        }
        if let Err(e) = sigaction(Signal::SIGTERM, &action) {
            warn!(error = %e, "Failed to install SIGTERM handler");
        }
    }

    debug!("Signal handlers installed for SIGINT and SIGTERM");
}

/// Check if a signal was received and get the signal number.
fn check_signal() -> Option<Signal> {
    if SIGNAL_RECEIVED.load(Ordering::Acquire) {
        // Reset the flag
        SIGNAL_RECEIVED.store(false, Ordering::Release);
        let signum = SIGNAL_NUMBER.load(Ordering::Acquire);
        Signal::try_from(signum).ok()
    } else {
        None
    }
}

// ============================================================================
// Supervisor
// ============================================================================

/// Supervises a child process with timeout and signal forwarding.
pub struct Supervisor {
    child: Child,
    timeout: Option<Duration>,
    start_time: Instant,
    child_pid: Pid,
}

impl Supervisor {
    /// Spawn a new supervised child process.
    ///
    /// # Arguments
    /// * `cmd` - The command to spawn (must have stdout set to Stdio::piped())
    /// * `timeout` - Optional timeout; child will be killed if exceeded
    pub fn spawn(mut cmd: Command, timeout: Option<Duration>) -> Result<Self> {
        let child = cmd.spawn().map_err(ZellijCcError::Spawn)?;
        let child_pid = Pid::from_raw(child.id() as i32);

        info!(
            pid = child.id(),
            timeout_secs = timeout.map(|d| d.as_secs()),
            "Spawned child process"
        );

        Ok(Self {
            child,
            timeout,
            start_time: Instant::now(),
            child_pid,
        })
    }

    /// Get a reference to the child process.
    pub fn child(&mut self) -> &mut Child {
        &mut self.child
    }

    /// Get the child's stdout (takes ownership).
    ///
    /// # Panics
    /// Panics if stdout was not configured or already taken.
    pub fn take_stdout(&mut self) -> std::process::ChildStdout {
        self.child
            .stdout
            .take()
            .expect("stdout was not configured or already taken")
    }

    /// Get the child's stdin (takes ownership).
    pub fn take_stdin(&mut self) -> Option<std::process::ChildStdin> {
        self.child.stdin.take()
    }

    /// Wait for the child to exit, with timeout enforcement.
    ///
    /// This method:
    /// 1. Polls `try_wait()` in a loop
    /// 2. Checks for timeout and kills the child if exceeded
    /// 3. Checks for signals and forwards them to the child
    ///
    /// # Returns
    /// * `Ok(ExitStatus)` - Child exited normally
    /// * `Err(ProcessTimeout)` - Child was killed due to timeout
    pub fn wait_with_timeout(&mut self) -> Result<ExitStatus> {
        loop {
            // Check if child has exited
            match self.child.try_wait() {
                Ok(Some(status)) => {
                    info!(
                        pid = self.child_pid.as_raw(),
                        exit_code = status.code(),
                        elapsed_secs = self.start_time.elapsed().as_secs(),
                        "Child process exited"
                    );
                    return Ok(status);
                }
                Ok(None) => {
                    // Child still running
                }
                Err(e) => {
                    return Err(ZellijCcError::Io(e));
                }
            }

            // Check timeout
            if let Some(timeout) = self.timeout {
                let elapsed = self.start_time.elapsed();
                if elapsed >= timeout {
                    warn!(
                        pid = self.child_pid.as_raw(),
                        elapsed_secs = elapsed.as_secs(),
                        timeout_secs = timeout.as_secs(),
                        "Child process timeout, killing"
                    );
                    self.kill()?;
                    return Err(ZellijCcError::ProcessTimeout { elapsed });
                }
            }

            // Check for signals to forward
            if let Some(signal) = check_signal() {
                info!(
                    pid = self.child_pid.as_raw(),
                    signal = ?signal,
                    "Forwarding signal to child"
                );
                if let Err(e) = kill(self.child_pid, signal) {
                    warn!(error = %e, "Failed to forward signal to child");
                }
            }

            // Sleep briefly to avoid busy-waiting
            std::thread::sleep(Duration::from_millis(50));
        }
    }

    /// Kill the child process.
    pub fn kill(&mut self) -> Result<()> {
        debug!(pid = self.child_pid.as_raw(), "Killing child process");

        // First try SIGTERM
        if let Err(e) = kill(self.child_pid, Signal::SIGTERM) {
            // Process may already be dead
            debug!(error = %e, "SIGTERM failed (process may be dead)");
        }

        // Give it a moment to terminate gracefully
        std::thread::sleep(Duration::from_millis(100));

        // Check if it's dead
        match self.child.try_wait() {
            Ok(Some(_)) => {
                debug!(pid = self.child_pid.as_raw(), "Child terminated after SIGTERM");
                return Ok(());
            }
            Ok(None) => {
                // Still running, escalate to SIGKILL
                warn!(
                    pid = self.child_pid.as_raw(),
                    "Child didn't respond to SIGTERM, sending SIGKILL"
                );
                if let Err(e) = kill(self.child_pid, Signal::SIGKILL) {
                    debug!(error = %e, "SIGKILL failed");
                }
            }
            Err(e) => {
                return Err(ZellijCcError::Io(e));
            }
        }

        // Wait for it to die
        match self.child.wait() {
            Ok(_) => Ok(()),
            Err(e) => Err(ZellijCcError::Io(e)),
        }
    }

    /// Get elapsed time since spawn.
    pub fn elapsed(&self) -> Duration {
        self.start_time.elapsed()
    }
}

impl Drop for Supervisor {
    fn drop(&mut self) {
        // Ensure child is killed if still running
        match self.child.try_wait() {
            Ok(Some(_)) => {
                // Already exited, nothing to do
            }
            Ok(None) => {
                // Still running, kill it
                warn!(
                    pid = self.child_pid.as_raw(),
                    "Child still running on Supervisor drop, killing"
                );
                let _ = self.kill();
            }
            Err(e) => {
                warn!(error = %e, "Failed to check child status on drop");
            }
        }
    }
}

// ============================================================================
// Builder for Command with common settings
// ============================================================================

/// Build a Command for spawning claude with appropriate settings.
pub fn build_claude_command(
    claude_args: &[String],
    cwd: Option<&std::path::Path>,
    signal_fifo_path: &std::path::Path,
) -> Command {
    let mut cmd = Command::new("claude");
    cmd.args(claude_args)
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit()) // Pass stderr through to pane
        .env("TIDEPOOL_SIGNAL_FIFO", signal_fifo_path);

    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }

    cmd
}
