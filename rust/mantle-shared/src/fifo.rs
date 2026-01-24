//! FIFO (named pipe) abstractions for inter-process communication.
//!
//! Provides typed wrappers for result and signal FIFOs with proper
//! cleanup, timeout handling, and thread synchronization.
//!
//! ## Threading Model
//!
//! `SignalFifo` spawns a background reader thread that polls the signal FIFO
//! and sends parsed signals over an mpsc channel. The main thread consumes
//! signals via `try_recv()` (non-blocking) or `drain()` (collect all pending).
//!
//! Thread synchronization uses `Acquire`/`Release` ordering on the stop flag
//! to ensure visibility of the termination signal across threads.

use crate::error::{MantleError, Result};
use crate::events::{InterruptSignal, RunResult};
use nix::poll::{poll, PollFd, PollFlags, PollTimeout};
use nix::sys::stat::Mode;
use nix::unistd::mkfifo;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::os::fd::AsFd;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};
use tracing::{debug, trace, warn};

// ============================================================================
// FIFO Guard (RAII cleanup)
// ============================================================================

/// Guard that removes a FIFO on drop, ensuring cleanup even on error paths.
pub struct FifoGuard {
    path: PathBuf,
}

impl FifoGuard {
    /// Create a new FIFO at the given path.
    ///
    /// Removes any stale FIFO from previous runs (e.g., after crash).
    pub fn new(path: PathBuf) -> Result<Self> {
        // Remove any stale FIFO from previous runs
        if path.exists() {
            std::fs::remove_file(&path).ok();
        }
        mkfifo(&path, Mode::S_IRUSR | Mode::S_IWUSR).map_err(|e| MantleError::FifoCreate {
            path: path.clone(),
            source: std::io::Error::from(e), // Errno implements Into<io::Error>
        })?;
        debug!(path = %path.display(), "Created FIFO");
        Ok(Self { path })
    }

    /// Get the path to the FIFO.
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for FifoGuard {
    fn drop(&mut self) {
        if let Err(err) = std::fs::remove_file(&self.path) {
            warn!(path = %self.path.display(), error = %err, "Failed to remove FIFO");
        } else {
            debug!(path = %self.path.display(), "Removed FIFO");
        }
    }
}

// ============================================================================
// Result FIFO
// ============================================================================

/// FIFO for receiving the final RunResult from the wrap subprocess.
pub struct ResultFifo {
    guard: FifoGuard,
}

impl ResultFifo {
    /// Create a new result FIFO with a unique path based on PID.
    pub fn new() -> Result<Self> {
        let path = PathBuf::from(format!("/tmp/mantle-{}.fifo", std::process::id()));
        Ok(Self {
            guard: FifoGuard::new(path)?,
        })
    }

    /// Get the path to the FIFO.
    pub fn path(&self) -> &Path {
        self.guard.path()
    }

    /// Read the RunResult with proper timeout handling.
    ///
    /// Fixes the poll timeout bug by looping with 60-second chunks and
    /// tracking elapsed time properly. Timeouts >65s now work correctly.
    ///
    /// # Arguments
    /// * `timeout` - Maximum time to wait. Use `Duration::ZERO` for no timeout.
    pub fn read_with_timeout(&self, timeout: Duration) -> Result<RunResult> {
        let start = Instant::now();
        let path = self.guard.path();

        // Open FIFO for reading (blocks until writer opens it)
        let file = File::open(path).map_err(|e| MantleError::FifoOpen {
            path: path.to_path_buf(),
            source: e,
        })?;

        let mut poll_fds = [PollFd::new(file.as_fd(), PollFlags::POLLIN)];

        loop {
            let elapsed = start.elapsed();

            // Check timeout (Duration::ZERO means no timeout)
            if timeout != Duration::ZERO && elapsed >= timeout {
                return Err(MantleError::FifoTimeout { elapsed });
            }

            // Calculate remaining time, capped at 60s to avoid u16 overflow.
            // PollTimeout::from takes u16 milliseconds, max 65535ms (~65s).
            // We loop with 60s chunks to support arbitrarily long timeouts.
            let remaining = if timeout == Duration::ZERO {
                Duration::from_secs(60) // Poll in 60s chunks when no timeout
            } else {
                timeout.saturating_sub(elapsed).min(Duration::from_secs(60))
            };

            let poll_timeout = if remaining.is_zero() {
                // Shouldn't happen due to timeout check above, but be safe
                return Err(MantleError::FifoTimeout { elapsed });
            } else {
                // Cap at 60000ms to be safely under u16::MAX
                let ms = remaining.as_millis().min(60000) as u16;
                PollTimeout::from(ms)
            };

            trace!(remaining_ms = remaining.as_millis(), "Polling FIFO");

            match poll(&mut poll_fds, poll_timeout)? {
                0 => {
                    // Poll timeout, loop to check overall timeout
                    trace!("Poll chunk timeout, continuing...");
                    continue;
                }
                _ => {
                    // Data available, read and parse
                    let content =
                        std::io::read_to_string(&file).map_err(|e| MantleError::FifoRead {
                            path: path.to_path_buf(),
                            source: e,
                        })?;

                    let result: RunResult = serde_json::from_str(&content)
                        .map_err(|e| MantleError::JsonParse { source: e })?;

                    debug!(
                        session_id = %result.session_id,
                        exit_code = result.exit_code,
                        "Received result from FIFO"
                    );
                    return Ok(result);
                }
            }
        }
    }
}

// ============================================================================
// Signal FIFO
// ============================================================================

/// FIFO for receiving interrupt signals from Claude Code.
///
/// Spawns a background reader thread that polls the FIFO and sends
/// parsed signals over an mpsc channel. The main thread can consume
/// signals non-blocking via `try_recv()`.
pub struct SignalFifo {
    guard: FifoGuard,
    rx: Receiver<InterruptSignal>,
    should_stop: Arc<AtomicBool>,
    thread: Option<JoinHandle<()>>,
}

impl SignalFifo {
    /// Create a new signal FIFO and start the reader thread.
    pub fn new() -> Result<Self> {
        let path = PathBuf::from(format!("/tmp/mantle-{}.signal", std::process::id()));
        let guard = FifoGuard::new(path)?;

        let (tx, rx) = mpsc::channel::<InterruptSignal>();
        let should_stop = Arc::new(AtomicBool::new(false));

        let fifo_path = guard.path().to_path_buf();
        let should_stop_clone = Arc::clone(&should_stop);

        let thread = thread::spawn(move || {
            Self::reader_thread(fifo_path, tx, should_stop_clone);
        });

        Ok(Self {
            guard,
            rx,
            should_stop,
            thread: Some(thread),
        })
    }

    /// Get the path to the FIFO (for setting TIDEPOOL_SIGNAL_FIFO env var).
    pub fn path(&self) -> &Path {
        self.guard.path()
    }

    /// Try to receive a signal without blocking.
    pub fn try_recv(&self) -> Option<InterruptSignal> {
        self.rx.try_recv().ok()
    }

    /// Drain all pending signals.
    pub fn drain(&self) -> Vec<InterruptSignal> {
        let mut signals = Vec::new();
        while let Ok(signal) = self.rx.try_recv() {
            signals.push(signal);
        }
        signals
    }

    /// Background reader thread.
    ///
    /// Uses `Acquire` ordering when loading the stop flag to ensure
    /// visibility of the `Release` store from the main thread.
    fn reader_thread(
        fifo_path: PathBuf,
        tx: Sender<InterruptSignal>,
        should_stop: Arc<AtomicBool>,
    ) {
        while !should_stop.load(Ordering::Acquire) {
            // Try to open the FIFO
            match File::open(&fifo_path) {
                Ok(file) => {
                    let reader = BufReader::new(file);
                    for line in reader.lines() {
                        // Check stop flag between lines
                        if should_stop.load(Ordering::Acquire) {
                            break;
                        }

                        if let Ok(line) = line {
                            if line.trim().is_empty() {
                                continue;
                            }
                            match serde_json::from_str::<InterruptSignal>(&line) {
                                Ok(signal) => {
                                    debug!(
                                        signal_type = %signal.signal_type,
                                        state = ?signal.state,
                                        "Received interrupt signal"
                                    );
                                    if tx.send(signal).is_err() {
                                        // Receiver dropped, exit
                                        return;
                                    }
                                }
                                Err(e) => {
                                    warn!(error = %e, line = %line, "Failed to parse signal JSON");
                                }
                            }
                        }
                    }
                    // EOF on FIFO, will reopen on next iteration
                }
                Err(e) => {
                    // FIFO not ready yet or permission error
                    // Only log once per retry cycle to avoid spam
                    trace!(error = %e, "FIFO not ready, retrying...");
                    thread::sleep(Duration::from_millis(100));
                }
            }
        }
        debug!("Signal reader thread exiting");
    }
}

impl Drop for SignalFifo {
    fn drop(&mut self) {
        // Signal thread to stop using Release ordering to ensure visibility
        self.should_stop.store(true, Ordering::Release);

        // Unblock the reader thread by opening the FIFO for writing
        // This satisfies the reader's blocking open() if it's waiting
        let path = self.guard.path().to_path_buf();
        // Use a non-blocking open or just a quick open/close
        // We ignore errors because the file might be gone or already open
        let _ = std::fs::OpenOptions::new().write(true).open(&path);

        // Join the thread (may block briefly if it's waiting on FIFO read)
        if let Some(thread) = self.thread.take() {
            // Give thread a moment to notice the stop flag
            thread::sleep(Duration::from_millis(10));

            match thread.join() {
                Ok(()) => {
                    debug!("Signal reader thread joined successfully");
                }
                Err(e) => {
                    warn!("Signal reader thread panicked: {:?}", e);
                }
            }
        }
        // FifoGuard drop will clean up the FIFO file
    }
}

// ============================================================================
// Signal Writer (for the `signal` subcommand)
// ============================================================================

/// Write an interrupt signal to a signal FIFO.
///
/// Used by the `mantle signal` subcommand to send signals from
/// Claude Code to the wrap process.
pub fn write_signal(fifo_path: &Path, signal: &InterruptSignal) -> Result<()> {
    let json = serde_json::to_string(signal).map_err(MantleError::JsonSerialize)?;

    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .open(fifo_path)
        .map_err(|e| MantleError::FifoOpen {
            path: fifo_path.to_path_buf(),
            source: e,
        })?;

    file.write_all(json.as_bytes())
        .map_err(|e| MantleError::FifoWrite {
            path: fifo_path.to_path_buf(),
            source: e,
        })?;
    file.write_all(b"\n").map_err(|e| MantleError::FifoWrite {
        path: fifo_path.to_path_buf(),
        source: e,
    })?;

    debug!(
        signal_type = %signal.signal_type,
        state = ?signal.state,
        "Sent interrupt signal"
    );
    Ok(())
}

// ============================================================================
// Write result to FIFO (for wrap command)
// ============================================================================

/// Write a RunResult to a result FIFO.
pub fn write_result(fifo_path: &Path, result: &RunResult) -> Result<()> {
    let json = serde_json::to_string(result).map_err(MantleError::JsonSerialize)?;

    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .open(fifo_path)
        .map_err(|e| MantleError::FifoOpen {
            path: fifo_path.to_path_buf(),
            source: e,
        })?;

    file.write_all(json.as_bytes())
        .map_err(|e| MantleError::FifoWrite {
            path: fifo_path.to_path_buf(),
            source: e,
        })?;

    debug!(
        session_id = %result.session_id,
        exit_code = result.exit_code,
        "Wrote result to FIFO"
    );
    Ok(())
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_fifo_guard_cleanup() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("test.fifo");
        
        {
            let _guard = FifoGuard::new(path.clone()).unwrap();
            assert!(path.exists());
        }
        
        assert!(!path.exists());
    }

    #[test]
    fn test_result_fifo_roundtrip() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("result.fifo");
        let guard = FifoGuard::new(path.clone()).unwrap();
        let result_fifo = ResultFifo { guard };

        let expected = RunResult {
            exit_code: 42,
            is_error: false,
            result: Some("done".to_string()),
            structured_output: None,
            session_id: "sess-123".to_string(),
            session_tag: None,
            total_cost_usd: 0.1,
            num_turns: 5,
            events: vec![],
            permission_denials: vec![],
            model_usage: std::collections::HashMap::new(),
            interrupts: vec![],
            tool_calls: None,
            stderr_output: None,
        };

        let path_clone = path.clone();
        let expected_clone = expected.clone();
        
        thread::spawn(move || {
            // Give reader a moment to open
            thread::sleep(Duration::from_millis(50));
            write_result(&path_clone, &expected_clone).unwrap();
        });

        let received = result_fifo.read_with_timeout(Duration::from_secs(2)).unwrap();
        assert_eq!(received.session_id, expected.session_id);
        assert_eq!(received.exit_code, expected.exit_code);
    }

    #[test]
    fn test_signal_fifo_roundtrip() {
        let signal_fifo = SignalFifo::new().unwrap();
        let path = signal_fifo.path().to_path_buf();

        let signal = InterruptSignal {
            signal_type: "transition".to_string(),
            state: Some("next".to_string()),
            reason: Some("Testing".to_string()),
        };

        write_signal(&path, &signal).unwrap();

        // Wait for reader thread to pick it up
        let mut received = None;
        for _ in 0..20 {
            if let Some(s) = signal_fifo.try_recv() {
                received = Some(s);
                break;
            }
            thread::sleep(Duration::from_millis(50));
        }

        let received = received.expect("Did not receive signal in time");
        assert_eq!(received.signal_type, signal.signal_type);
        assert_eq!(received.state, signal.state);
    }
}
