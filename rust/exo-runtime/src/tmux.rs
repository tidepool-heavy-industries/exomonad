//! `impl Tmux for Runtime` — pane lifecycle, the pane-liveness probe, and the tmux-paste
//! delivery last-hop. `paste` delegates to exomonad-shared's hardened `TmuxIpc::inject_input`;
//! everything else is direct `tokio::process` tmux CLI (do NOT reintroduce blocking calls).
//! `self.tmux_session` is the session name to target.
//!
//! Consumers: the `Bus` last-hop (`paste`), the `Spawner` (`new_pane`/`new_window`/`paste`/
//! `kill_pane`), and the `Topology`/`ChildLiveness` probes (`list_panes`) — all through the
//! supertrait edges, runtime-internal rather than policy-facing.

use crate::runtime::Runtime;
use async_trait::async_trait;
use exo_caps::{PaneId, Tmux, TmuxError};
use std::collections::HashSet;
use std::path::Path;

#[async_trait]
impl Tmux for Runtime {
    async fn new_pane(&self, cwd: &Path, cmd: &str) -> Result<PaneId, TmuxError> {
        self.spawn_in_session("split-window", None, cwd, cmd).await
    }

    async fn new_window(&self, name: &str, cwd: &Path, cmd: &str) -> Result<PaneId, TmuxError> {
        self.spawn_in_session("new-window", Some(name), cwd, cmd)
            .await
    }

    async fn paste(&self, pane: &PaneId, text: &str) -> Result<(), TmuxError> {
        // Delegate to exomonad's hardened injection: per-target lock, copy/scroll-mode
        // cancel, 150ms debounce, and Enter-retry — the machinery that prevents the silent
        // paste failures a hand-rolled `load-buffer`/`send-keys` is prone to.
        exomonad_shared::services::tmux_ipc::TmuxIpc::new(&self.tmux_session)
            .inject_input(pane.as_str(), text)
            .await
            .map_err(|e| TmuxError::Failed {
                op: "paste",
                detail: e.to_string(),
            })
    }

    async fn kill_pane(&self, pane: &PaneId) -> Result<(), TmuxError> {
        self.tmux("kill_pane", &["kill-pane", "-t", pane.as_str()])
            .await?;
        Ok(())
    }

    async fn list_panes(&self) -> Result<HashSet<String>, TmuxError> {
        // `-a` = all sessions: a child's pane lives in this session, but the probe is a raw
        // existence set, so the wider net costs nothing and never misses a relocated pane.
        let output = self
            .tmux("list_panes", &["list-panes", "-a", "-F", "#{pane_id}"])
            .await?;
        Ok(String::from_utf8_lossy(&output.stdout)
            .lines()
            .map(|l| l.trim().to_string())
            .filter(|l| !l.is_empty())
            .collect())
    }
}

impl Runtime {
    /// Spawn `cmd` in `cwd` under this session via `subcmd` (`split-window` for a pane,
    /// `new-window` for a tab), printing + parsing the new pane's `%id`. Shared by
    /// `new_pane` / `new_window` so there is one spawn-and-capture path.
    async fn spawn_in_session(
        &self,
        subcmd: &'static str,
        name: Option<&str>,
        cwd: &Path,
        cmd: &str,
    ) -> Result<PaneId, TmuxError> {
        let cwd_str = cwd.to_string_lossy();
        // `-d` spawns the window/pane WITHOUT stealing focus — the human keeps typing where they
        // are (the agent is reached by pane-id, not by being current). `-n <name>` names the
        // window (new-window only; split-window has no window name).
        let mut args: Vec<&str> = vec![subcmd, "-d", "-t", &self.tmux_session, "-c", &cwd_str];
        if let Some(name) = name {
            args.push("-n");
            args.push(name);
        }
        args.extend(["-P", "-F", "#{pane_id}", cmd]);
        let output = self.tmux(subcmd, &args).await?;
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
