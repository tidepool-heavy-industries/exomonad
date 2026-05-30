//! The single hop that shells out: a tmux pane id → its pane pid.

use crate::error::{Result, ScryError};

/// Resolve a tmux pane id (e.g. `"%306"`) to the pid of the process running in
/// that pane (its shell). The caller then walks the subtree to Claude Code.
pub fn pane_pid(pane: &str) -> Result<i32> {
    let out = std::process::Command::new("tmux")
        .args(["display-message", "-p", "-t", pane, "#{pane_pid}"])
        .output()
        .map_err(|e| ScryError::Tmux(format!("could not run tmux: {e}")))?;
    if !out.status.success() {
        return Err(ScryError::Tmux(format!(
            "tmux display-message failed for pane {pane}: {}",
            String::from_utf8_lossy(&out.stderr).trim()
        )));
    }
    let s = String::from_utf8_lossy(&out.stdout);
    let trimmed = s.trim();
    // tmux exits 0 with empty stdout when the pane id doesn't resolve.
    if trimmed.is_empty() {
        return Err(ScryError::Tmux(format!("pane {pane} not found")));
    }
    trimmed
        .parse::<i32>()
        .map_err(|_| ScryError::Tmux(format!("unexpected pane_pid output: {trimmed:?}")))
}
