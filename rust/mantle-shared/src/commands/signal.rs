//! Signal command implementation.
//!
//! Provides the `send_signal` function for writing interrupt signals to a FIFO.
//! This is called by Claude via its Bash tool to request state transitions.

use crate::error::Result;
use crate::events::InterruptSignal;
use crate::fifo::write_signal;
use std::path::Path;

/// Send an interrupt signal to the wrapper process.
///
/// This writes a JSON-encoded [`InterruptSignal`] to the signal FIFO,
/// which is monitored by the wrapper process. The signal can request:
/// - State transitions (`transition`)
/// - Escalation to human review (`escalate`)
/// - Session forking (`fork`)
/// - Review requests (`request_review`)
///
/// # Arguments
///
/// * `fifo` - Path to the signal FIFO
/// * `signal_type` - Type of signal (e.g., "transition", "escalate", "fork")
/// * `state` - Optional target state for transitions
/// * `reason` - Optional reason/payload for the signal
///
/// # Example
///
/// ```ignore
/// // Called by Claude via bash:
/// // mantle-agent signal fork --state "feat-x" --reason "Need separate worktree"
/// send_signal(&fifo_path, "fork", Some("feat-x"), Some("Need separate worktree"))?;
/// ```
pub fn send_signal(
    fifo: &Path,
    signal_type: &str,
    state: Option<&str>,
    reason: Option<&str>,
) -> Result<()> {
    let signal = InterruptSignal {
        signal_type: signal_type.to_string(),
        state: state.map(|s| s.to_string()),
        reason: reason.map(|s| s.to_string()),
    };

    write_signal(fifo, &signal)?;

    println!("Signal sent: {} (state: {:?})", signal_type, state);
    Ok(())
}
