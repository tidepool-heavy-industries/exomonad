//! `Tmux` capability — pane lifecycle + the tmux-paste delivery last-hop. Signatures
//! firm up in Wave 1 (adapt exomonad-core `TmuxIpc`, incl. the buffer-paste pattern).

use crate::types::PaneId;
use async_trait::async_trait;
use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TmuxError {
    #[error("tmux {op} failed: {detail}")]
    Failed { op: &'static str, detail: String },
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[async_trait]
pub trait Tmux {
    async fn new_pane(&self, cwd: &Path, cmd: &str) -> Result<PaneId, TmuxError>;
    /// Like `new_pane` but a fresh tmux WINDOW (tab) rather than a split, named `name` (so
    /// the tab shows the agent, not the bare process). Worktree children (each agent = its
    /// own window) use this; `new_pane` (split) is for inline workers.
    async fn new_window(&self, name: &str, cwd: &Path, cmd: &str) -> Result<PaneId, TmuxError>;
    /// The non-CC delivery last-hop: paste rendered `[from: X] …` text into the pane.
    async fn paste(&self, pane: &PaneId, text: &str) -> Result<(), TmuxError>;
    async fn kill_pane(&self, pane: &PaneId) -> Result<(), TmuxError>;
}
