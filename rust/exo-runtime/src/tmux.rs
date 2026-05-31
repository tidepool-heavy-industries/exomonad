//! `impl Tmux for Runtime` — pane lifecycle + the tmux-paste delivery last-hop.
//!
//! **Leaf R2.** Adapt exomonad-core `TmuxIpc` (`services/tmux_ipc.rs`): `split_window`
//! /`new_window` for `new_pane`, the buffer-paste pattern (`load-buffer` + `paste-buffer`
//! + `send-keys Enter`) in `inject_input` for `paste`, `kill_pane` for `kill_pane`. Those
//! are already async (`tokio::process` under the hood) — do NOT reintroduce blocking calls.
//! `self.tmux_session` is the session name to target.
//!
//! Consumers (why this cap stays, despite "provisional"): the `Bus` last-hop (`paste`)
//! and the `Spawner` (`new_pane`/`kill_pane`) both call it — it is runtime-internal, not
//! policy-facing, but it is NOT zero-consumer.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{PaneId, Tmux, TmuxError};
use std::path::Path;

#[async_trait]
impl Tmux for Runtime {
    async fn new_pane(&self, _cwd: &Path, _cmd: &str) -> Result<PaneId, TmuxError> {
        todo!("R2: tmux split/new-window in self.tmux_session at cwd running cmd; parse %N")
    }

    async fn paste(&self, _pane: &PaneId, _text: &str) -> Result<(), TmuxError> {
        todo!("R2: buffer-paste pattern (load-buffer temp + paste-buffer + send-keys Enter)")
    }

    async fn kill_pane(&self, _pane: &PaneId) -> Result<(), TmuxError> {
        todo!("R2: tmux kill-pane -t <pane>")
    }
}
