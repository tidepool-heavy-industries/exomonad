//! TUI module for zellij-cc
//!
//! Provides an interactive terminal UI for viewing Claude Code session progress.

mod app;
mod events;
mod state;

use anyhow::Result;
use std::sync::mpsc::Receiver;

pub use events::TuiEvent;
pub use state::TuiResult;

/// Run the TUI with events from the given channel.
/// Returns the collected state when the TUI exits.
pub fn run_tui(event_rx: Receiver<TuiEvent>) -> Result<TuiResult> {
    app::run(event_rx)
}
