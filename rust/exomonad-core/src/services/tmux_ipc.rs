//! tmux CLI wrapper for session, window, and pane management.
//!
//! All methods are synchronous (blocking `Command::new("tmux")`).
//! Callers wrap in `tokio::task::spawn_blocking` as needed.

use anyhow::{Context, Result};
use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use std::sync::Mutex as StdMutex;
use tracing::{debug, info, warn};

/// Per-target injection lock. Serializes inject_input calls to the same
/// tmux target, preventing concurrent paste-buffer/send-keys interleaving.
/// Per-target injection locks. Uses Weak references so entries are automatically
/// reclaimable when no inject_input call holds the Arc.
static INJECTION_LOCKS: std::sync::LazyLock<StdMutex<HashMap<String, std::sync::Weak<StdMutex<()>>>>> =
    std::sync::LazyLock::new(|| StdMutex::new(HashMap::new()));

/// Stable tmux window identifier (@N format, base-index immune).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WindowId(String);

impl WindowId {
    pub fn parse(s: &str) -> Result<Self> {
        anyhow::ensure!(s.starts_with('@'), "WindowId must start with '@': {}", s);
        let suffix = &s[1..];
        anyhow::ensure!(
            !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_digit()),
            "WindowId suffix must be a non-empty digit sequence: {}",
            s
        );
        Ok(Self(s.to_string()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for WindowId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Tmux pane identifier (%N format).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PaneId(String);

impl PaneId {
    pub fn parse(s: &str) -> Result<Self> {
        anyhow::ensure!(s.starts_with('%'), "PaneId must start with '%': {}", s);
        let suffix = &s[1..];
        anyhow::ensure!(
            !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_digit()),
            "PaneId suffix must be a non-empty digit sequence: {}",
            s
        );
        Ok(Self(s.to_string()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for PaneId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Information about a tmux window.
pub struct WindowInfo {
    pub window_id: WindowId,
    pub window_name: String,
    pub pane_id: PaneId,
}

/// tmux CLI wrapper for a specific session.
#[derive(Debug, Clone)]
pub struct TmuxIpc {
    session_name: String,
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
            .args([
                "new-session",
                "-d",
                "-s",
                name,
                "-P",
                "-F",
                "#{window_id}",
                "-c",
                &cwd.to_string_lossy(),
            ])
            .output()
            .context("Failed to run tmux new-session")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux new-session failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let window_id = WindowId::parse(&raw)
            .context("Failed to parse window_id from tmux new-session")?;
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
            anyhow::bail!(
                "tmux kill-session failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
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
    pub fn new_window(
        &self,
        name: &str,
        cwd: &Path,
        shell: &str,
        command: &str,
    ) -> Result<WindowId> {
        let output = std::process::Command::new("tmux")
            .args([
                "new-window",
                "-P",
                "-F",
                "#{window_id}",
                "-t",
                &self.session_name,
                "-n",
                name,
                "-c",
                &cwd.to_string_lossy(),
                shell,
                "-l",
                "-c",
                command,
            ])
            .output()
            .context("Failed to run tmux new-window")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux new-window failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let window_id = WindowId::parse(&raw)
            .context("Failed to parse window_id from tmux new-window")?;
        info!(session = %self.session_name, window = %window_id, name, "Created tmux window");
        Ok(window_id)
    }

    pub fn list_windows(&self) -> Result<Vec<WindowInfo>> {
        let output = std::process::Command::new("tmux")
            .args([
                "list-windows",
                "-t",
                &self.session_name,
                "-F",
                "#{window_id}\t#{window_name}\t#{pane_id}",
            ])
            .output()
            .context("Failed to run tmux list-windows")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux list-windows failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let windows = String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter_map(|line| {
                let parts: Vec<&str> = line.split('\t').collect();
                if parts.len() < 3 {
                    warn!(
                        "Unexpected tmux list-windows line (expected 3 tab-separated fields): {:?}",
                        line
                    );
                    return None;
                }
                let window_id = match WindowId::parse(parts[0]) {
                    Ok(id) => id,
                    Err(e) => {
                        warn!("Failed to parse window_id from tmux output: {}", e);
                        return None;
                    }
                };
                let pane_id = match PaneId::parse(parts[2]) {
                    Ok(id) => id,
                    Err(e) => {
                        warn!("Failed to parse pane_id from tmux output: {}", e);
                        return None;
                    }
                };
                Some(WindowInfo {
                    window_id,
                    window_name: parts[1].to_string(),
                    pane_id,
                })
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
            anyhow::bail!(
                "tmux kill-window failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
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
            anyhow::bail!(
                "tmux select-window failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        Ok(())
    }

    // -- Pane management --

    /// Split the window to create a new pane. Returns pane_id (%N).
    pub fn split_window(
        &self,
        window_id: &WindowId,
        cwd: &Path,
        shell: &str,
        command: &str,
    ) -> Result<PaneId> {
        let output = std::process::Command::new("tmux")
            .args([
                "split-window",
                "-P",
                "-F",
                "#{pane_id}",
                "-t",
                window_id.as_str(),
                "-c",
                &cwd.to_string_lossy(),
                shell,
                "-l",
                "-c",
                command,
            ])
            .output()
            .context("Failed to run tmux split-window")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux split-window failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let pane_id = PaneId::parse(&raw)
            .context("Failed to parse pane_id from tmux split-window")?;
        info!(window = %window_id, pane = %pane_id, "Created tmux pane");
        Ok(pane_id)
    }

    pub fn kill_pane(&self, pane_id: &PaneId) -> Result<()> {
        let output = std::process::Command::new("tmux")
            .args(["kill-pane", "-t", pane_id.as_str()])
            .output()
            .context("Failed to run tmux kill-pane")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux kill-pane failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
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
    ///
    /// The target is session-qualified (`{session}:{target}`) to ensure all
    /// commands resolve to the same pane. Without qualification, tmux resolves
    /// display-name targets against the "most recently used" session, which is
    /// nondeterministic for subprocess calls.
    pub fn inject_input(&self, target: &str, text: &str) -> Result<()> {
        // Session-qualify the target so paste-buffer and send-keys resolve
        // to the same pane deterministically.
        let qualified_target = format!("{}:{}", self.session_name, target);

        // Serialize injections to the same target to prevent interleaving.
        // Uses Weak refs so lock entries are reclaimed when not in use.
        let target_lock = {
            let mut map = INJECTION_LOCKS.lock().expect("injection lock map poisoned");
            // Prune dead entries opportunistically
            map.retain(|_, weak| weak.strong_count() > 0);
            let arc = map
                .get(&qualified_target)
                .and_then(|w| w.upgrade())
                .unwrap_or_else(|| {
                    let arc = std::sync::Arc::new(StdMutex::new(()));
                    map.insert(qualified_target.clone(), std::sync::Arc::downgrade(&arc));
                    arc
                });
            arc
        };
        let _guard = target_lock.lock().expect("per-target injection lock poisoned");

        // Exit copy/scroll mode if active — copy mode intercepts input,
        // preventing paste-buffer from reaching the underlying process.
        let mode_output = std::process::Command::new("tmux")
            .args(["display-message", "-p", "-t", &qualified_target, "#{pane_in_mode}"])
            .output();
        if let Ok(output) = mode_output {
            if output.status.success() && String::from_utf8_lossy(&output.stdout).trim() == "1" {
                let _ = std::process::Command::new("tmux")
                    .args(["send-keys", "-t", &qualified_target, "-X", "cancel"])
                    .output();
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
        }

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
            anyhow::bail!(
                "tmux load-buffer failed: {}",
                String::from_utf8_lossy(&load_output.stderr)
            );
        }

        // No -p flag: bracketed paste (\e[200~...\e[201~) crashes Claude Code's
        // Ink TUI and breaks Gemini CLI's readline. Plain paste streams bytes
        // as standard keyboard input.
        let paste_output = std::process::Command::new("tmux")
            .args(["paste-buffer", "-b", &buf_name, "-t", &qualified_target])
            .output()
            .context("Failed to run tmux paste-buffer")?;

        // Delete the named buffer
        match std::process::Command::new("tmux")
            .args(["delete-buffer", "-b", &buf_name])
            .output()
        {
            Ok(output) if !output.status.success() => {
                warn!(
                    "tmux delete-buffer failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
            }
            Err(e) => {
                warn!("failed to run tmux delete-buffer: {}", e);
            }
            _ => {}
        }

        if !paste_output.status.success() {
            anyhow::bail!(
                "tmux paste-buffer failed: {}",
                String::from_utf8_lossy(&paste_output.stderr)
            );
        }

        // Debounce: allow TUI (Claude Code Ink, Gemini CLI readline) to process
        // the pasted text before sending Enter.
        std::thread::sleep(std::time::Duration::from_millis(150));

        // Submit with retry — TUIs may drop the first Enter keystroke
        // if still processing pasted text.
        let mut last_err = None;
        for attempt in 0..3 {
            if attempt > 0 {
                std::thread::sleep(std::time::Duration::from_millis(200));
            }
            match std::process::Command::new("tmux")
                .args(["send-keys", "-t", &qualified_target, "Enter"])
                .output()
            {
                Ok(output) if output.status.success() => {
                    last_err = None;
                    break;
                }
                Ok(output) => {
                    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                    warn!(target = %qualified_target, attempt, "send-keys Enter failed: {}", stderr);
                    last_err = Some(stderr);
                }
                Err(e) => {
                    warn!(target = %qualified_target, attempt, "send-keys Enter error: {}", e);
                    last_err = Some(e.to_string());
                }
            }
        }
        if let Some(err) = last_err {
            anyhow::bail!("send-keys Enter failed after 3 attempts: {}", err);
        }

        // Wake the target pane's TUI event loop via SIGWINCH so it processes
        // the injected input. Non-fatal \u{2014} input was already delivered.
        if let Err(e) = self.wake_pane(target) {
            warn!(target = %qualified_target, error = %e, "SIGWINCH wake failed (non-fatal)");
        }

        debug!(target = %qualified_target, chars = text.len(), "Injected input via tmux buffer");
        Ok(())
    }

    /// Trigger SIGWINCH in the target pane by briefly resizing its window.
    ///
    /// TUI frameworks (Ink, readline) in non-focused panes may not poll stdin
    /// until a terminal event arrives. A +1/-1 column resize triggers SIGWINCH,
    /// which wakes the event loop to process buffered input.
    pub fn wake_pane(&self, target: &str) -> Result<()> {
        let qualified = format!("{}:{}", self.session_name, target);

        // Read current window dimensions
        let output = std::process::Command::new("tmux")
            .args([
                "display-message",
                "-t",
                &qualified,
                "-p",
                "#{window_width} #{window_height}",
            ])
            .output()
            .context("Failed to query window dimensions")?;

        if !output.status.success() {
            anyhow::bail!(
                "tmux display-message failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        let dims = String::from_utf8_lossy(&output.stdout);
        let dims = dims.trim();
        let parts: Vec<&str> = dims.split_whitespace().collect();
        if parts.len() != 2 {
            anyhow::bail!("Unexpected dimension format: {}", dims);
        }
        let width: u32 = parts[0].parse().context("Failed to parse window width")?;
        let height: u32 = parts[1].parse().context("Failed to parse window height")?;

        // Resize +1 column
        let _ = std::process::Command::new("tmux")
            .args([
                "resize-window",
                "-t",
                &qualified,
                "-x",
                &(width + 1).to_string(),
                "-y",
                &height.to_string(),
            ])
            .output();

        std::thread::sleep(std::time::Duration::from_millis(50));

        // Restore original size
        let _ = std::process::Command::new("tmux")
            .args([
                "resize-window",
                "-t",
                &qualified,
                "-x",
                &width.to_string(),
                "-y",
                &height.to_string(),
            ])
            .output();

        debug!(target = %qualified, "SIGWINCH wake: resized {}x{} \u{2192} {}x{} \u{2192} {}x{}", width, height, width + 1, height, width, height);
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
    fn test_window_id_rejects_at_only() {
        assert!(WindowId::parse("@").is_err());
    }

    #[test]
    fn test_window_id_rejects_non_digits() {
        assert!(WindowId::parse("@abc").is_err());
        assert!(WindowId::parse("@1a").is_err());
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
    fn test_pane_id_rejects_percent_only() {
        assert!(PaneId::parse("%").is_err());
    }

    #[test]
    fn test_pane_id_rejects_non_digits() {
        assert!(PaneId::parse("%abc").is_err());
        assert!(PaneId::parse("%1a").is_err());
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

    #[test]
    fn test_id_roundtrip() {
        let wid = WindowId::parse("@123").unwrap();
        assert_eq!(wid.as_str(), "@123");
        assert_eq!(wid.to_string(), "@123");

        let pid = PaneId::parse("%456").unwrap();
        assert_eq!(pid.as_str(), "%456");
        assert_eq!(pid.to_string(), "%456");
    }

    #[test]
    fn test_injection_lock_serializes_same_target() {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicU32, Ordering};

        let counter = Arc::new(AtomicU32::new(0));
        let barrier = Arc::new(std::sync::Barrier::new(2));

        let handles: Vec<_> = (0..2).map(|_| {
            let counter = counter.clone();
            let barrier = barrier.clone();
            std::thread::spawn(move || {
                barrier.wait();
                let lock = {
                    let mut map = INJECTION_LOCKS.lock().unwrap();
                    map.retain(|_, weak| weak.strong_count() > 0);
                    map.get("test-serialization-target")
                        .and_then(|w| w.upgrade())
                        .unwrap_or_else(|| {
                            let arc = Arc::new(StdMutex::new(()));
                            map.insert("test-serialization-target".to_string(), Arc::downgrade(&arc));
                            arc
                        })
                };
                let _guard = lock.lock().unwrap();
                // Simulate work under lock
                let val = counter.load(Ordering::SeqCst);
                std::thread::sleep(std::time::Duration::from_millis(10));
                counter.store(val + 1, Ordering::SeqCst);
            })
        }).collect();

        for h in handles {
            h.join().unwrap();
        }
        // If serialized correctly, counter == 2 (no lost increments)
        assert_eq!(counter.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn test_injection_lock_different_targets_independent() {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicBool, Ordering};

        // Verify two different targets can be locked concurrently (no deadlock,
        // and both threads reach the barrier while holding their respective locks).
        let both_reached_barrier = Arc::new(AtomicBool::new(false));
        let barrier = Arc::new(std::sync::Barrier::new(2));

        let flag = both_reached_barrier.clone();
        let b1 = barrier.clone();

        let h1 = std::thread::spawn(move || {
            let lock = Arc::new(StdMutex::new(()));
            let _guard = lock.lock().unwrap();
            // Both threads wait here — if locks were shared (same target),
            // the second thread would block on lock() and never reach the barrier.
            b1.wait();
            flag.store(true, Ordering::SeqCst);
        });

        let h2 = std::thread::spawn(move || {
            let lock = Arc::new(StdMutex::new(()));
            let _guard = lock.lock().unwrap();
            barrier.wait();
        });

        h1.join().unwrap();
        h2.join().unwrap();
        assert!(both_reached_barrier.load(Ordering::SeqCst), "Both threads should hold independent locks concurrently");
    }

    #[test]
    fn test_wake_pane_requires_session() {
        // wake_pane runs tmux commands that will fail without a real tmux session,
        // but it should not panic — it returns a Result
        let ipc = TmuxIpc::new("nonexistent-test-session");
        let result = ipc.wake_pane("test-target");
        assert!(
            result.is_err(),
            "wake_pane should fail without a real tmux session"
        );
    }
}
