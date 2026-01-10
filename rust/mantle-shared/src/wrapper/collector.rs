//! Event collector for Claude Code stream processing.
//!
//! Accumulates streaming events and interrupt signals during a Claude session,
//! then builds the final [`RunResult`] when the session completes.

use crate::error::Result;
use crate::events::{InterruptSignal, ResultEvent, RunResult, StreamEvent};
use crate::fifo::{write_result, SignalFifo};
use crate::hub::{run_result_to_session_result, write_result_to_socket};
use std::path::Path;
use tracing::warn;

/// Destination for writing the final result.
#[derive(Debug, Clone)]
pub enum ResultDestination<'a> {
    /// Write to a FIFO (legacy mode).
    Fifo(&'a Path),
    /// Write to hub socket with session ID.
    HubSocket { path: &'a Path, session_id: &'a str },
    /// Write to stdout (attached container mode).
    Stdout,
}

/// Collects streaming events and builds the final RunResult.
///
/// The collector accumulates:
/// - All stream events from Claude's stdout
/// - The final result event (success/error)
/// - Any interrupt signals sent via FIFO
pub struct EventCollector {
    events: Vec<StreamEvent>,
    result_event: Option<ResultEvent>,
    interrupts: Vec<InterruptSignal>,
}

impl EventCollector {
    /// Create a new empty collector.
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
            result_event: None,
            interrupts: Vec::new(),
        }
    }

    /// Process a single JSON line. Returns parsed event on success.
    ///
    /// Automatically captures the result event when encountered.
    pub fn process_line(&mut self, line: &str) -> Option<StreamEvent> {
        if line.trim().is_empty() {
            return None;
        }
        match serde_json::from_str::<StreamEvent>(line) {
            Ok(event) => {
                if let StreamEvent::Result(ref r) = event {
                    self.result_event = Some(r.clone());
                }
                self.events.push(event.clone());
                Some(event)
            }
            Err(e) => {
                let truncated: String = line.chars().take(50).collect();
                warn!(error = %e, line = %truncated, "JSON parse error");
                None
            }
        }
    }

    /// Add an interrupt signal.
    pub fn add_interrupt(&mut self, signal: InterruptSignal) {
        self.interrupts.push(signal);
    }

    /// Drain all pending signals from FIFO.
    pub fn drain_signals(&mut self, fifo: &SignalFifo) {
        self.interrupts.extend(fifo.drain());
    }

    /// Build final result and write to FIFO.
    ///
    /// This is called when the Claude process exits. It builds a [`RunResult`]
    /// from all collected events and writes it to the result FIFO, which
    /// unblocks the waiting orchestrator process.
    pub fn finalize(
        self,
        exit_code: i32,
        session_tag: Option<&str>,
        result_fifo: &Path,
    ) -> Result<()> {
        self.finalize_to(exit_code, session_tag, ResultDestination::Fifo(result_fifo))
    }

    /// Build final result and write to the specified destination.
    ///
    /// Supports both FIFO (legacy) and hub socket (new) destinations.
    pub fn finalize_to(
        self,
        exit_code: i32,
        session_tag: Option<&str>,
        destination: ResultDestination<'_>,
    ) -> Result<()> {
        let result = RunResult::from_events(
            self.events,
            self.result_event,
            exit_code,
            session_tag.map(|s| s.to_string()),
            self.interrupts,
        );

        match destination {
            ResultDestination::Fifo(path) => write_result(path, &result),
            ResultDestination::HubSocket { path, session_id } => {
                let session_result = run_result_to_session_result(&result, session_id);
                write_result_to_socket(path, &session_result)
            }
            ResultDestination::Stdout => {
                // Print JSON to stdout - captured by parent process in attached mode
                let json = serde_json::to_string(&result)?;
                println!("{}", json);
                Ok(())
            }
        }
    }
}

impl Default for EventCollector {
    fn default() -> Self {
        Self::new()
    }
}
