//! Simple (non-TUI) wrapper mode for Claude Code.
//!
//! Processes Claude's JSONL output line-by-line, printing humanized output
//! to stdout. Used for CI/headless environments where a full TUI isn't needed.

use crate::error::Result;
use crate::fifo::SignalFifo;
use crate::humanize::{print_event_humanized, print_interrupt};
use crate::supervisor::Supervisor;
use crate::wrapper::collector::ResultDestination;
use crate::wrapper::EventCollector;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::process::ChildStdout;
use tracing::warn;

/// Run Claude in simple println mode (for CI/headless).
///
/// This is the original non-TUI behavior:
/// - Reads JSONL from Claude's stdout line by line
/// - Prints humanized output to the terminal
/// - Collects interrupt signals from FIFO
/// - Writes final result when Claude exits
pub fn wrap_claude_simple(
    stdout: ChildStdout,
    signal_fifo: &SignalFifo,
    supervisor: &mut Supervisor,
    result_fifo: &Path,
    session_tag: Option<&str>,
) -> Result<()> {
    let reader = BufReader::new(stdout);
    let mut collector = EventCollector::new();

    // Process each JSONL line from claude stdout
    for line in reader.lines() {
        // Check for signals (non-blocking)
        while let Some(signal) = signal_fifo.try_recv() {
            print_interrupt(&signal);
            collector.add_interrupt(signal);
        }

        let line = match line {
            Ok(l) => l,
            Err(e) => {
                warn!(error = %e, "Error reading line from claude stdout");
                continue;
            }
        };

        if let Some(event) = collector.process_line(&line) {
            print_event_humanized(&event); // Human-readable to stdout (pane)
        }
    }

    // Drain any remaining signals
    collector.drain_signals(signal_fifo);

    // Wait for claude to exit
    let status = supervisor.wait_with_timeout()?;
    let exit_code = status.code().unwrap_or(-1);

    // Build final result and write to FIFO (unblocks the waiting orchestrator)
    collector.finalize(exit_code, session_tag, result_fifo)
}

/// Run Claude in simple println mode, writing to specified destination.
///
/// Like `wrap_claude_simple`, but allows writing to either a FIFO or hub socket.
pub fn wrap_claude_simple_to(
    stdout: ChildStdout,
    signal_fifo: &SignalFifo,
    supervisor: &mut Supervisor,
    destination: ResultDestination<'_>,
    session_tag: Option<&str>,
) -> Result<()> {
    let reader = BufReader::new(stdout);
    let mut collector = EventCollector::new();

    // Process each JSONL line from claude stdout
    for line in reader.lines() {
        // Check for signals (non-blocking)
        while let Some(signal) = signal_fifo.try_recv() {
            print_interrupt(&signal);
            collector.add_interrupt(signal);
        }

        let line = match line {
            Ok(l) => l,
            Err(e) => {
                warn!(error = %e, "Error reading line from claude stdout");
                continue;
            }
        };

        if let Some(event) = collector.process_line(&line) {
            print_event_humanized(&event); // Human-readable to stdout (pane)
        }
    }

    // Drain any remaining signals
    collector.drain_signals(signal_fifo);

    // Wait for claude to exit
    let status = supervisor.wait_with_timeout()?;
    let exit_code = status.code().unwrap_or(-1);

    // Build final result and write to destination
    collector.finalize_to(exit_code, session_tag, destination)
}
