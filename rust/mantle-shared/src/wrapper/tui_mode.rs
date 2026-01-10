//! TUI wrapper mode for Claude Code.
//!
//! Runs Claude with an interactive terminal UI that displays streaming output,
//! tool calls, and session state. Uses ratatui for rendering.

use crate::error::Result;
use crate::events::{InterruptSignal, StreamEvent};
use crate::fifo::SignalFifo;
use crate::supervisor::Supervisor;
use crate::tui::{run_tui, TuiEvent};
use crate::wrapper::EventCollector;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::process::ChildStdout;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use tracing::warn;

/// Run Claude with interactive TUI display.
///
/// Spawns background threads for:
/// - Reading Claude's JSONL stdout
/// - Polling the signal FIFO for interrupts
///
/// The TUI runs in the main thread and displays events as they arrive.
/// When Claude exits or the user quits, the final result is written to the FIFO.
pub fn wrap_claude_tui(
    stdout: ChildStdout,
    signal_fifo: &SignalFifo,
    supervisor: &mut Supervisor,
    result_fifo: &Path,
    session_tag: Option<&str>,
) -> Result<()> {
    // Create channel for TUI events
    let (tx, rx) = mpsc::channel::<TuiEvent>();
    let tx_signal = tx.clone();

    // Spawn stdout reader thread
    let stdout_handle = thread::spawn(move || {
        let reader = BufReader::new(stdout);
        for line in reader.lines() {
            let line = match line {
                Ok(l) => l,
                Err(e) => {
                    warn!(error = %e, "Error reading line from claude stdout");
                    continue;
                }
            };

            if line.trim().is_empty() {
                continue;
            }

            match serde_json::from_str::<StreamEvent>(&line) {
                Ok(event) => {
                    if tx.send(TuiEvent::Claude(event)).is_err() {
                        break; // TUI closed
                    }
                }
                Err(e) => {
                    let truncated: String = line.chars().take(50).collect();
                    warn!(
                        error = %e,
                        line = %truncated,
                        "JSON parse error"
                    );
                }
            }
        }
        // Signal that stdout is done
        let _ = tx.send(TuiEvent::ProcessExit);
    });

    // Clone signal_fifo path for the thread
    let signal_path = signal_fifo.path().to_path_buf();

    // Spawn signal reader thread
    let signal_handle = thread::spawn(move || {
        // Poll periodically to check for signals
        loop {
            // Try to read signal from the FIFO
            if let Ok(contents) = std::fs::read_to_string(&signal_path) {
                for line in contents.lines() {
                    if let Ok(signal) = serde_json::from_str::<InterruptSignal>(line) {
                        if tx_signal.send(TuiEvent::Interrupt(signal)).is_err() {
                            return; // TUI closed
                        }
                    }
                }
                // Clear the file after reading
                let _ = std::fs::write(&signal_path, "");
            }
            thread::sleep(Duration::from_millis(100));
        }
    });

    // Run TUI (blocks until user quits)
    let tui_result = run_tui(rx)?;

    // Wait for stdout thread to finish
    let _ = stdout_handle.join();

    // Signal thread runs forever, so we don't join it - it'll be terminated when we exit
    drop(signal_handle);

    // Wait for the child process (non-blocking check in loop)
    let exit_code = loop {
        match supervisor.try_wait()? {
            Some(status) => {
                break status.code().unwrap_or(-1);
            }
            None => {
                // Process still running, wait a bit
                thread::sleep(Duration::from_millis(100));
            }
        }
    };

    // Build final result from TUI state and write to FIFO
    EventCollector::from_tui_result(tui_result).finalize(exit_code, session_tag, result_fifo)
}
