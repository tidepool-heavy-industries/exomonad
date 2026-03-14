use anyhow::{Context, Result};
use std::path::Path;
use tracing::{debug, info, warn};

/// Stable tmux window identifier (@N format, base-index immune).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WindowId(String);

impl WindowId {
    pub fn parse(s: &str) -> Result<Self> {
        anyhow::ensure!(s.starts_with('@'), "WindowId must start with '@': {}", s);
        Ok(Self(s.to_string()))
    }
    pub fn as_str(&self) -> &str { &self.0 }
}

impl std::fmt::Display for WindowId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// Tmux pane identifier (%N format).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PaneId(String);

impl PaneId {
    pub fn parse(s: &str) -> Result<Self> {
        anyhow::ensure!(s.starts_with('%'), "PaneId must start with '%': {}", s);
        Ok(Self(s.to_string()))
    }
    pub fn as_str(&self) -> &str { &self.0 }
}

impl std::fmt::Display for PaneId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// tmux CLI wrapper for a specific session.
#[derive(Debug, Clone)]
pub struct TmuxIpc {
    session_name: String,
}

/// Information about a tmux window.
pub struct WindowInfo {
    pub window_id: WindowId,
    pub window_name: String,
    pub pane_id: PaneId,
}

impl TmuxIpc {
    pub fn new(session_name: &str) -> Self {
        Self {
            session_name: session_name.to_string(),
        }
    }

    pub fn session_name(&self) -> &str {
        &self.session_name
    }

    // -- Session management (static, no &self) --

    /// Create a new tmux session. Returns the stable window ID (@N) of the initial window.
    pub fn new_session(name: &str, cwd: &Path) -> Result<WindowId> {
        let output = std::process::Command::new("tmux")
            .args(["new-session", "-d", "-s", name, "-P", "-F", "#{window_id}", "-c", &cwd.to_string_lossy()])
            .output()
            .context("Failed to run tmux new-session")?;
        if !output.status.success() {
            anyhow::bail!("tmux new-session failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let window_id = WindowId::parse(&raw)?;
        info!(session = %name, window = %window_id, "Created tmux session");
        Ok(window_id)
    }

    pub fn has_session(name: &str) -> Result<bool> {
        let status = std::process::Command::new("tmux")
            .args(["has-session", "-t", name])
            .status()
            .context("Failed to run tmux has-session")?;
        Ok(status.success())
    }

    pub fn kill_session(name: &str) -> Result<()> {
        let output = std::process::Command::new("tmux")
            .args(["kill-session", "-t", name])
            .output()
            .context("Failed to run tmux kill-session")?;
        if !output.status.success() {
            anyhow::bail!("tmux kill-session failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        info!(session = %name, "Killed tmux session");
        Ok(())
    }

    /// Exec into tmux attach (replaces current process).
    pub fn attach_session(name: &str) -> Result<()> {
        use std::os::unix::process::CommandExt;
        let err = std::process::Command::new("tmux")
            .args(["attach-session", "-t", name])
            .exec();
        Err(anyhow::anyhow!("exec tmux attach failed: {}", err))
    }

    // -- Window management --

    /// Create a new window. Returns window_id (@N).
    pub fn new_window(&self, name: &str, cwd: &Path, shell: &str, command: &str) -> Result<WindowId> {
        let output = std::process::Command::new("tmux")
            .args([
                "new-window", "-P", "-F", "#{window_id}",
                "-t", &self.session_name,
                "-n", name,
                "-c", &cwd.to_string_lossy(),
                shell, "-l", "-c", command,
            ])
            .output()
            .context("Failed to run tmux new-window")?;
        if !output.status.success() {
            anyhow::bail!("tmux new-window failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let window_id = WindowId::parse(&raw)?;
        info!(session = %self.session_name, window = %window_id, name, "Created tmux window");
        Ok(window_id)
    }

    pub fn list_windows(&self) -> Result<Vec<WindowInfo>> {
        let output = std::process::Command::new("tmux")
            .args([
                "list-windows", "-t", &self.session_name,
                "-F", "#{window_id}	#{window_name}	#{pane_id}",
            ])
            .output()
            .context("Failed to run tmux list-windows")?;
        if !output.status.success() {
            anyhow::bail!("tmux list-windows failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        let windows = String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter_map(|line| {
                let parts: Vec<&str> = line.split('\t').collect();
                if parts.len() >= 3 {
                    Some(WindowInfo {
                        window_id: WindowId::parse(parts[0]).ok()?,
                        window_name: parts[1].to_string(),
                        pane_id: PaneId::parse(parts[2]).ok()?,
                    })
                } else {
                    None
                }
            })
            .collect();
        Ok(windows)
    }

    pub fn kill_window(&self, window_id: &WindowId) -> Result<()> {
        let output = std::process::Command::new("tmux")
            .args(["kill-window", "-t", window_id.as_str()])
            .output()
            .context("Failed to run tmux kill-window")?;
        if !output.status.success() {
            anyhow::bail!("tmux kill-window failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        info!(window = %window_id, "Killed tmux window");
        Ok(())
    }

    pub fn select_window(&self, window_id: &WindowId) -> Result<()> {
        let output = std::process::Command::new("tmux")
            .args(["select-window", "-t", window_id.as_str()])
            .output()
            .context("Failed to run tmux select-window")?;
        if !output.status.success() {
            anyhow::bail!("tmux select-window failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        Ok(())
    }

    // -- Pane management --

    /// Split the window to create a new pane. Returns pane_id (%N).
    pub fn split_window(&self, window_id: &WindowId, cwd: &Path, shell: &str, command: &str) -> Result<PaneId> {
        let output = std::process::Command::new("tmux")
            .args([
                "split-window", "-P", "-F", "#{pane_id}",
                "-t", window_id.as_str(),
                "-c", &cwd.to_string_lossy(),
                shell, "-l", "-c", command,
            ])
            .output()
            .context("Failed to run tmux split-window")?;
        if !output.status.success() {
            anyhow::bail!("tmux split-window failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let pane_id = PaneId::parse(&raw)?;
        info!(window = %window_id, pane = %pane_id, "Created tmux pane");
        Ok(pane_id)
    }

    pub fn kill_pane(&self, pane_id: &PaneId) -> Result<()> {
        let output = std::process::Command::new("tmux")
            .args(["kill-pane", "-t", pane_id.as_str()])
            .output()
            .context("Failed to run tmux kill-pane")?;
        if !output.status.success() {
            anyhow::bail!("tmux kill-pane failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        info!(pane = %pane_id, "Killed tmux pane");
        Ok(())
    }

    // -- Input injection (buffer pattern for multiline safety) --

    /// Inject text into a target pane via tmux buffer pattern.
    ///
    /// Uses load-buffer → paste-buffer → send-keys Enter. The text is written
    /// without a trailing newline so that send-keys Enter is the sole execution
    /// trigger (avoids double-submit). No bracketed paste (-p) — Claude Code's
    /// Ink TUI and Gemini CLI's readline can't handle the escape sequences.
    pub fn inject_input(&self, target: &str, text: &str) -> Result<()> {
        let buf_name = format!("exo_{}", uuid::Uuid::new_v4().as_simple());
        let tmp_path = format!("/tmp/exomonad_buf_{}", buf_name);

        // Strip trailing newlines so paste-buffer doesn't trigger submission;
        // send-keys Enter below is the sole execution trigger.
        let payload = text.trim_end_matches('\n').trim_end_matches('\r');
        std::fs::write(&tmp_path, payload).context("Failed to write temp buffer file")?;

        let load_result = std::process::Command::new("tmux")
            .args(["load-buffer", "-b", &buf_name, &tmp_path])
            .output();

        // Clean up temp file regardless of result
        let _ = std::fs::remove_file(&tmp_path);

        let load_output = load_result.context("Failed to run tmux load-buffer")?;
        if !load_output.status.success() {
            anyhow::bail!("tmux load-buffer failed: {}", String::from_utf8_lossy(&load_output.stderr));
        }

        // No -p flag: bracketed paste (\e[200~...\e[201~) crashes Claude Code's
        // Ink TUI and breaks Gemini CLI's readline. Plain paste streams bytes
        // as standard keyboard input.
        let paste_output = std::process::Command::new("tmux")
            .args(["paste-buffer", "-b", &buf_name, "-t", target])
            .output()
            .context("Failed to run tmux paste-buffer")?;

        // Delete the named buffer
        match std::process::Command::new("tmux")
            .args(["delete-buffer", "-b", &buf_name])
            .output()
        {
            Ok(output) if !output.status.success() => {
                warn!("tmux delete-buffer failed: {}", String::from_utf8_lossy(&output.stderr));
            }
            Err(e) => {
                warn!("failed to run tmux delete-buffer: {}", e);
            }
            _ => {}
        }

        if !paste_output.status.success() {
            anyhow::bail!("tmux paste-buffer failed: {}", String::from_utf8_lossy(&paste_output.stderr));
        }

        // Sole execution trigger — decoupled from data injection
        let send_output = std::process::Command::new("tmux")
            .args(["send-keys", "-t", target, "Enter"])
            .output()
            .context("Failed to run tmux send-keys")?;

        if !send_output.status.success() {
            anyhow::bail!("tmux send-keys failed: {}", String::from_utf8_lossy(&send_output.stderr));
        }

        debug!(target, chars = text.len(), "Injected input via tmux buffer");
        Ok(())
    }

    // -- Query --

    pub fn pane_exists(&self, pane_id: &PaneId) -> Result<bool> {
        let status = std::process::Command::new("tmux")
            .args(["display-message", "-t", pane_id.as_str(), "-p", ""])
            .status()
            .context("Failed to run tmux display-message")?;
        Ok(status.success())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_session_name() {
        let ipc = TmuxIpc::new("test-session");
        assert_eq!(ipc.session_name(), "test-session");
    }

    #[test]
    fn test_window_id_parse_valid() {
        let id = WindowId::parse("@0").unwrap();
        assert_eq!(id.as_str(), "@0");
        let id = WindowId::parse("@42").unwrap();
        assert_eq!(id.as_str(), "@42");
    }

    #[test]
    fn test_window_id_parse_invalid() {
        assert!(WindowId::parse("0").is_err());
        assert!(WindowId::parse("%0").is_err());
        assert!(WindowId::parse("").is_err());
        assert!(WindowId::parse("window").is_err());
    }

    #[test]
    fn test_pane_id_parse_valid() {
        let id = PaneId::parse("%0").unwrap();
        assert_eq!(id.as_str(), "%0");
        let id = PaneId::parse("%99").unwrap();
        assert_eq!(id.as_str(), "%99");
    }

    #[test]
    fn test_pane_id_parse_invalid() {
        assert!(PaneId::parse("0").is_err());
        assert!(PaneId::parse("@0").is_err());
        assert!(PaneId::parse("").is_err());
        assert!(PaneId::parse("pane").is_err());
    }

    #[test]
    fn test_window_id_display() {
        let id = WindowId::parse("@5").unwrap();
        assert_eq!(format!("{}", id), "@5");
    }

    #[test]
    fn test_pane_id_display() {
        let id = PaneId::parse("%12").unwrap();
        assert_eq!(format!("{}", id), "%12");
    }
}
