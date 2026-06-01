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
        let output = self
            .tmux(
                "new_pane",
                &[
                    "split-window",
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
            op: "new_pane",
            detail: e.to_string(),
        })
    }

    async fn paste(&self, pane: &PaneId, text: &str) -> Result<(), TmuxError> {
        let target = pane.as_str();

        // 1. Exit copy/scroll mode if active — copy mode intercepts input (matches TmuxIpc)
        let mode_output = tokio::process::Command::new("tmux")
            .args(["display-message", "-p", "-t", target, "#{pane_in_mode}"])
            .output()
            .await;

        if let Ok(output) = mode_output {
            if output.status.success() && String::from_utf8_lossy(&output.stdout).trim() == "1" {
                let _ = tokio::process::Command::new("tmux")
                    .args(["send-keys", "-t", target, "-X", "cancel"])
                    .output()
                    .await;
                tokio::time::sleep(std::time::Duration::from_millis(50)).await;
            }
        }

        // 2. Buffer paste pattern — load-buffer temp + paste-buffer + send-keys Enter
        let payload = text.trim_end_matches('\n').trim_end_matches('\r');
        // NamedTempFile::new() does sync filesystem work; keep it off the async executor.
        let tmp = tokio::task::spawn_blocking(tempfile::NamedTempFile::new)
            .await
            .map_err(|e| TmuxError::Failed {
                op: "paste",
                detail: format!("temp-file task join: {e}"),
            })?
            .map_err(TmuxError::Io)?;
        let tmp_path = tmp.path().to_path_buf();
        tokio::fs::write(&tmp_path, payload)
            .await
            .map_err(TmuxError::Io)?;

        // Use temp file name as a unique buffer name
        let buf_name = format!(
            "exo_{}",
            tmp_path.file_name().unwrap_or_default().to_string_lossy()
        );
        let tmp_path_str = tmp_path.to_string_lossy().into_owned();

        self.tmux("paste", &["load-buffer", "-b", &buf_name, &tmp_path_str])
            .await?;

        self.tmux(
            "paste",
            &["paste-buffer", "-t", target, "-b", &buf_name, "-d"],
        )
        .await?;

        // Debounce: allow TUI (Claude Code Ink, Gemini CLI readline) to process pasted text
        tokio::time::sleep(std::time::Duration::from_millis(150)).await;

        self.tmux("paste", &["send-keys", "-t", target, "Enter"])
            .await?;

        Ok(())
    }

    async fn kill_pane(&self, pane: &PaneId) -> Result<(), TmuxError> {
        self.tmux("kill_pane", &["kill-pane", "-t", pane.as_str()])
            .await?;
        Ok(())
    }
}

impl Runtime {
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
