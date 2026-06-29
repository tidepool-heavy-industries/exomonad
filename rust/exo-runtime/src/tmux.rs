//! `impl Tmux for Runtime` — all tmux primitives delegated to exomonad-shared `TmuxIpc`.
//! One argv-building site: `TmuxIpc` owns all tmux CLI construction.
//!
//! Consumers: the `Bus` last-hop (`paste`), the `Spawner` (`new_pane`/`new_window`/`paste`/
//! `kill_pane`), and the `Topology`/`ChildLiveness` probes (`list_panes`) — all through the
//! supertrait edges, runtime-internal rather than policy-facing.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{PaneId, Tmux, TmuxError};
use exomonad_shared::services::tmux_ipc::{PaneId as SharedPaneId, TmuxIpc};
use std::collections::HashSet;
use std::path::Path;

#[async_trait]
impl Tmux for Runtime {
    async fn new_pane(&self, cwd: &Path, cmd: &str) -> Result<PaneId, TmuxError> {
        let pane = TmuxIpc::new(&self.tmux_session)
            .spawn_pane(cwd, cmd)
            .await
            .map_err(|e| TmuxError::Failed {
                op: "split-window",
                detail: e.to_string(),
            })?;
        PaneId::new(pane.as_str().to_string()).map_err(|e| TmuxError::Failed {
            op: "split-window",
            detail: e.to_string(),
        })
    }

    async fn new_window(&self, name: &str, cwd: &Path, cmd: &str) -> Result<PaneId, TmuxError> {
        let pane = TmuxIpc::new(&self.tmux_session)
            .spawn_window(name, cwd, cmd)
            .await
            .map_err(|e| TmuxError::Failed {
                op: "new-window",
                detail: e.to_string(),
            })?;
        PaneId::new(pane.as_str().to_string()).map_err(|e| TmuxError::Failed {
            op: "new-window",
            detail: e.to_string(),
        })
    }

    async fn paste(&self, pane: &PaneId, text: &str) -> Result<(), TmuxError> {
        // Delegate to exomonad's hardened injection: per-target lock, copy/scroll-mode
        // cancel, 150ms debounce, and Enter-retry — the machinery that prevents the silent
        // paste failures a hand-rolled `load-buffer`/`send-keys` is prone to.
        TmuxIpc::new(&self.tmux_session)
            .inject_input(pane.as_str(), text)
            .await
            .map_err(|e| TmuxError::Failed {
                op: "paste",
                detail: e.to_string(),
            })
    }

    async fn kill_pane(&self, pane: &PaneId) -> Result<(), TmuxError> {
        let shared_pane =
            SharedPaneId::parse(pane.as_str()).map_err(|e| TmuxError::Failed {
                op: "kill_pane",
                detail: e.to_string(),
            })?;
        TmuxIpc::new(&self.tmux_session)
            .kill_pane(&shared_pane)
            .await
            .map_err(|e| TmuxError::Failed {
                op: "kill_pane",
                detail: e.to_string(),
            })
    }

    async fn list_panes(&self) -> Result<HashSet<String>, TmuxError> {
        // `-a` = all sessions: a child's pane lives in this session, but the probe is a raw
        // existence set, so the wider net costs nothing and never misses a relocated pane.
        TmuxIpc::new(&self.tmux_session)
            .list_panes_all()
            .await
            .map_err(|e| TmuxError::Failed {
                op: "list_panes",
                detail: e.to_string(),
            })
    }
}
