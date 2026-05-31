//! `Tmux` capability — pane lifecycle + the tmux-paste delivery last-hop. Signatures
//! firm up in Wave 1 (adapt exomonad-core `TmuxIpc`, incl. the buffer-paste pattern).

use crate::error::CapResult;
use crate::types::PaneId;
use async_trait::async_trait;
use std::path::Path;

#[async_trait]
pub trait Tmux {
    async fn new_pane(&self, cwd: &Path, cmd: &str) -> CapResult<PaneId>;
    /// The non-CC delivery last-hop: paste rendered `[from: X] …` text into the pane.
    async fn paste(&self, pane: &PaneId, text: &str) -> CapResult<()>;
    async fn kill_pane(&self, pane: &PaneId) -> CapResult<()>;
}
