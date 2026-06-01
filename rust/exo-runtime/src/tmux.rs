//! `impl Tmux for Runtime` — pane lifecycle + the tmux-paste delivery last-hop.
//!
//! **Leaf R2.** Adapt exomonad-core `TmuxIpc` (`services/tmux_ipc.rs`): `split_window`
//! or `new_window` for `new_pane`, the buffer-paste pattern (`load-buffer` +
//! `paste-buffer` + `send-keys Enter`) in `inject_input` for `paste`,
//! `kill_pane` for `kill_pane`. Those are already async (`tokio::process`
//! under the hood) — do NOT reintroduce blocking calls.
//!
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
    async fn new_pane(&self, cwd: &Path, cmd: &str) -> Result<PaneId, TmuxError> {
        self.spawn_in_session("split-window", cwd, cmd).await
    }

    async fn new_window(&self, cwd: &Path, cmd: &str) -> Result<PaneId, TmuxError> {
        self.spawn_in_session("new-window", cwd, cmd).await
    }

    async fn paste(&self, pane: &PaneId, text: &str) -> Result<(), TmuxError> {
        crate::session_boot::paste_to_pane(&self.tmux_session, pane, text).await
    }

    async fn kill_pane(&self, pane: &PaneId) -> Result<(), TmuxError> {
        self.tmux("kill_pane", &["kill-pane", "-t", pane.as_str()])
            .await?;
        Ok(())
    }
}

impl Runtime {
    /// Spawn `cmd` in `cwd` under this session via `subcmd` (`split-window` for a pane,
    /// `new-window` for a tab), printing + parsing the new pane's `%id`. Shared by
    /// `new_pane` / `new_window` so there is one spawn-and-capture path.
    async fn spawn_in_session(
        &self,
        subcmd: &'static str,
        cwd: &Path,
        cmd: &str,
    ) -> Result<PaneId, TmuxError> {
        let output = self
            .tmux(
                subcmd,
                &[
                    subcmd,
                    "-t",
                    &self.tmux_session,
                    "-c",
                    &cwd.to_string_lossy(),
                    "-P",
                    "-F",
                    "#{pane_id}",
                    cmd,
                ],
            )
            .await?;
        let s = String::from_utf8_lossy(&output.stdout).trim().to_string();
        PaneId::new(s).map_err(|e| TmuxError::Failed {
            op: subcmd,
            detail: e.to_string(),
        })
    }

    /// Private async helper for tmux CLI calls.
    /// Maps non-success exits to TmuxError::Failed { op, detail }.
    async fn tmux(
        &self,
        op: &'static str,
        args: &[&str],
    ) -> Result<std::process::Output, TmuxError> {
        let output = tokio::process::Command::new("tmux")
            .args(args)
            .output()
            .await
            .map_err(TmuxError::Io)?;

        if !output.status.success() {
            return Err(TmuxError::Failed {
                op,
                detail: String::from_utf8_lossy(&output.stderr).to_string(),
            });
        }
        Ok(output)
    }
}
