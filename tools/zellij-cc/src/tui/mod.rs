//! Terminal UI (TUI) integration for `zellij-cc`.
//!
//! This module provides an interactive terminal user interface for monitoring
//! Claude Code sessions inside a Zellij pane. It exposes:
//!
//! - [`TuiEvent`] - Events sent into the TUI (Claude events, interrupts, process exit)
//! - [`TuiResult`] - The final aggregated state returned when the TUI exits
//! - [`run_tui`] - The entry point that runs the TUI event loop
//!
//! ## Usage
//!
//! 1. Create an `mpsc::channel` for [`TuiEvent`]s
//! 2. Spawn producer threads that send events as the Claude session progresses
//! 3. Call [`run_tui`] with the receiver; it blocks until the user quits
//! 4. Inspect the returned [`TuiResult`] for collected events and final state
//!
//! ## Keyboard Controls
//!
//! - `j`/`k` or arrows: Scroll up/down
//! - `J`/`K` or Page Up/Down: Page scroll
//! - `g`/`G`: Jump to top/bottom
//! - `Enter`: Toggle expand/collapse on tool details
//! - `e`/`c`: Expand all / Collapse all
//! - `q`: Quit (only after session completes)
//! - `Ctrl-C`: Force quit (even if session is running)

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
