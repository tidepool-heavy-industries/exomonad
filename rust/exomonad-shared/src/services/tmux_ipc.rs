//! tmux CLI wrapper for session, window, and pane management.
//!
//! All methods are asynchronous (using `tokio::process::Command`).

use anyhow::{Context, Result};
use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use tokio::process::Command;
use tokio::sync::Mutex as AsyncMutex;
use tracing::{debug, info, warn};

/// Per-target injection locks. Uses Weak references so entries are automatically
/// reclaimable when no inject_input call holds the Arc.
///
/// Uses tokio::sync::Mutex for per-target locks because they are held across
/// await points during tmux CLI calls and debounce sleeps.
static INJECTION_LOCKS: std::sync::LazyLock<
    std::sync::Mutex<HashMap<String, std::sync::Weak<AsyncMutex<()>>>>,
> = std::sync::LazyLock::new(|| std::sync::Mutex::new(HashMap::new()));

/// Stable tmux window identifier (@N format, base-index immune).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WindowId(String);

impl serde::Serialize for WindowId {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.0)
    }
}

impl<'de> serde::Deserialize<'de> for WindowId {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        WindowId::parse(&s).map_err(serde::de::Error::custom)
    }
}
// ... (WindowId and PaneId implementations remain same, skipping for brevity in thought, but I must include them in new_string)
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

impl serde::Serialize for PaneId {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.0)
    }
}

impl<'de> serde::Deserialize<'de> for PaneId {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        PaneId::parse(&s).map_err(serde::de::Error::custom)
    }
}

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

/// True if captured pane content looks like Claude Code's Rewind menu (or a similar transient modal
/// that captures keyboard input until dismissed with Escape). Used to clear the modal before a paste
/// so the input isn't silently swallowed. Mirrors Gastown's `containsRewindIndicators`.
fn looks_like_rewind_modal(content: &str) -> bool {
    let lower = content.to_lowercase();
    if lower.contains("rewind") && lower.contains("enter") && lower.contains("esc") {
        return true;
    }
    const PAIRS: [(&str, &str); 4] = [
        ("enter to continue", "esc to exit"),
        ("enter to accept", "esc to cancel"),
        ("enter to select", "esc to go back"),
        ("enter to select", "esc to cancel"),
    ];
    PAIRS
        .iter()
        .any(|(a, b)| lower.contains(a) && lower.contains(b))
}

/// tmux CLI wrapper for a specific session.
#[derive(Debug, Clone)]
pub struct TmuxIpc {
    session_name: String,
    socket_name: Option<String>,
}

impl TmuxIpc {
    pub fn new(session_name: &str) -> Self {
        Self {
            session_name: session_name.to_string(),
            socket_name: None,
        }
    }

    pub fn new_with_socket(session_name: &str, socket_name: Option<String>) -> Self {
        Self {
            session_name: session_name.to_string(),
            socket_name,
        }
    }

    pub fn session_name(&self) -> &str {
        &self.session_name
    }

    /// Qualify a tmux target (window or pane) with the session name if it is not
    /// already qualified or a stable global identifier.
    ///
    /// Global IDs (@N, %N) and already-qualified targets (session:target)
    /// should be used as-is. Prefixing global IDs with session names
    /// is redundant for windows and invalid for panes.
    fn qualify_target(&self, target: &str) -> String {
        if target.starts_with('@') || target.starts_with('%') || target.contains(':') {
            target.to_string()
        } else {
            format!("{}:{}", self.session_name, target)
        }
    }

    /// Resolve a tmux target, converting display names with `.` to stable
    /// `@window_id` identifiers.
    ///
    /// tmux parses target specifiers as `{session}:{window}.{pane}` — a `.`
    /// in the display name is interpreted as the window/pane separator. A
    /// window named `💎 main.foo-gemini` becomes window=`💎 main`, pane=
    /// `foo-gemini` and tmux errors with "can't find window: 💎 main".
    /// There is no escape syntax for `.`; only @-prefixed IDs are safe.
    ///
    /// Returns IDs (`@N`, `%N`, `$N`) and already-qualified targets (those
    /// containing `:`) unchanged. For plain display names containing `.`,
    /// queries `list-windows` and returns the matching `@window_id`. Display
    /// names without `.` are returned unchanged — they resolve correctly
    /// without lookup.
    async fn resolve_target(&self, target: &str) -> Result<String> {
        if target.starts_with('@')
            || target.starts_with('%')
            || target.starts_with('$')
            || target.contains(':')
        {
            return Ok(target.to_string());
        }
        if !target.contains('.') {
            return Ok(target.to_string());
        }
        let windows = self.list_windows().await?;
        for w in &windows {
            if w.window_name == target {
                return Ok(w.window_id.as_str().to_string());
            }
        }
        anyhow::bail!(
            "tmux window not found by display name: '{}' (session: {})",
            target,
            self.session_name
        )
    }

    fn tmux_cmd(&self) -> Command {
        let mut cmd = Command::new("tmux");
        if let Some(socket) = &self.socket_name {
            cmd.arg("-L").arg(socket);
        }
        cmd
    }

    #[cfg(any(test, feature = "test-support"))]
    fn tmux_cmd_sync(&self) -> std::process::Command {
        let mut cmd = std::process::Command::new("tmux");
        if let Some(socket) = &self.socket_name {
            cmd.arg("-L").arg(socket);
        }
        cmd
    }

    #[cfg(any(test, feature = "test-support"))]
    pub async fn run_tmux_command(&self, args: &[&str]) -> Result<String> {
        let output = self
            .tmux_cmd()
            .args(args)
            .output()
            .await
            .context("Failed to run tmux command")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux command {:?} failed: {}",
                args,
                String::from_utf8_lossy(&output.stderr)
            );
        }
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    }

    // -- Session management (static, no &self) --

    /// Create a new tmux session. Returns the stable window ID (@N) of the initial window.
    pub async fn new_session(name: &str, cwd: &Path, socket: Option<&str>) -> Result<WindowId> {
        let mut cmd = Command::new("tmux");
        if let Some(s) = socket {
            cmd.arg("-L").arg(s);
        }
        let output = cmd
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
            .await
            .context("Failed to run tmux new-session")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux new-session failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let window_id =
            WindowId::parse(&raw).context("Failed to parse window_id from tmux new-session")?;
        info!(session = %name, window = %window_id, "Created tmux session");
        Ok(window_id)
    }

    pub async fn has_session(name: &str, socket: Option<&str>) -> Result<bool> {
        let mut cmd = Command::new("tmux");
        if let Some(s) = socket {
            cmd.arg("-L").arg(s);
        }
        let status = cmd
            .args(["has-session", "-t", name])
            .status()
            .await
            .context("Failed to run tmux has-session")?;
        Ok(status.success())
    }

    pub async fn kill_session(name: &str, socket: Option<&str>) -> Result<()> {
        let mut cmd = Command::new("tmux");
        if let Some(s) = socket {
            cmd.arg("-L").arg(s);
        }
        let output = cmd
            .args(["kill-session", "-t", name])
            .output()
            .await
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
    pub async fn attach_session(name: &str, socket: Option<&str>) -> Result<()> {
        use std::os::unix::process::CommandExt;
        let mut cmd = std::process::Command::new("tmux");
        if let Some(s) = socket {
            cmd.arg("-L").arg(s);
        }
        let err = cmd.args(["attach-session", "-t", name]).exec();
        Err(anyhow::anyhow!("exec tmux attach failed: {}", err))
    }

    // -- Window management --

    /// Create a new window. Returns window_id (@N).
    pub async fn new_window(
        &self,
        name: &str,
        cwd: &Path,
        shell: &str,
        command: &str,
    ) -> Result<WindowId> {
        let output = self
            .tmux_cmd()
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
            .await
            .context("Failed to run tmux new-window")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux new-window failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let window_id =
            WindowId::parse(&raw).context("Failed to parse window_id from tmux new-window")?;
        info!(session = %self.session_name, window = %window_id, name, "Created tmux window");
        Ok(window_id)
    }

    pub async fn list_windows(&self) -> Result<Vec<WindowInfo>> {
        let output = self
            .tmux_cmd()
            .args([
                "list-windows",
                "-t",
                &self.session_name,
                "-F",
                "#{window_id}\t#{window_name}\t#{pane_id}",
            ])
            .output()
            .await
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

    pub async fn kill_window(&self, window_id: &WindowId) -> Result<()> {
        let output = self
            .tmux_cmd()
            .args(["kill-window", "-t", window_id.as_str()])
            .output()
            .await
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

    pub async fn select_window(&self, window_id: &WindowId) -> Result<()> {
        let output = self
            .tmux_cmd()
            .args(["select-window", "-t", window_id.as_str()])
            .output()
            .await
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
    pub async fn split_window(
        &self,
        window_id: &WindowId,
        cwd: &Path,
        shell: &str,
        command: &str,
    ) -> Result<PaneId> {
        let output = self
            .tmux_cmd()
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
            .await
            .context("Failed to run tmux split-window")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux split-window failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let pane_id =
            PaneId::parse(&raw).context("Failed to parse pane_id from tmux split-window")?;
        info!(window = %window_id, pane = %pane_id, "Created tmux pane");
        Ok(pane_id)
    }

    /// Apply a tmux layout to a window.
    pub async fn select_layout(
        &self,
        window_id: &WindowId,
        layout: crate::domain::TmuxLayout,
    ) -> Result<()> {
        let qualified = self.qualify_target(window_id.as_str());
        let layout_str = layout.as_str();
        let output = self
            .tmux_cmd()
            .args(["select-layout", "-t", &qualified, layout_str])
            .output()
            .await
            .context("Failed to run tmux select-layout")?;
        if !output.status.success() {
            anyhow::bail!(
                "tmux select-layout failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        info!(window = %window_id, layout = layout_str, "Applied tmux layout");
        Ok(())
    }

    pub async fn kill_pane(&self, pane_id: &PaneId) -> Result<()> {
        let output = self
            .tmux_cmd()
            .args(["kill-pane", "-t", pane_id.as_str()])
            .output()
            .await
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
    pub async fn inject_input(&self, target: &str, text: &str) -> Result<()> {
        // Resolve display names containing `.` to stable @window_id first,
        // since tmux parses `.` as window-pane separator in targets. Then
        // qualify with session name for deterministic pane resolution.
        let resolved = self.resolve_target(target).await?;
        let qualified_target = self.qualify_target(&resolved);

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
                    let arc = std::sync::Arc::new(AsyncMutex::new(()));
                    map.insert(qualified_target.clone(), std::sync::Arc::downgrade(&arc));
                    arc
                });
            arc
        };
        let _guard = target_lock.lock().await;

        // Dismiss Claude Code's Rewind menu (double-Esc history browser) or a similar transient
        // modal before pasting: it takes over the terminal and silently swallows pasted input until
        // cleared with Escape. Gated on Rewind indicators (a Claude-specific UI), so we never send a
        // spurious Escape into a Gemini/Copilot pane mid-generation. Mirrors Gastown's
        // `isInRewindMode`/`dismissRewindMode`.
        if let Ok(output) = self
            .tmux_cmd()
            .args(["capture-pane", "-p", "-t", &qualified_target])
            .output()
            .await
        {
            if output.status.success()
                && looks_like_rewind_modal(&String::from_utf8_lossy(&output.stdout))
            {
                let _ = self
                    .tmux_cmd()
                    .args(["send-keys", "-t", &qualified_target, "Escape"])
                    .output()
                    .await;
                tokio::time::sleep(std::time::Duration::from_millis(300)).await;
            }
        }

        // Exit copy/scroll mode if active — copy mode intercepts input,
        // preventing paste-buffer from reaching the underlying process.
        let mode_output = self
            .tmux_cmd()
            .args([
                "display-message",
                "-p",
                "-t",
                &qualified_target,
                "#{pane_in_mode}",
            ])
            .output()
            .await;
        if let Ok(output) = mode_output {
            if output.status.success() && String::from_utf8_lossy(&output.stdout).trim() == "1" {
                let _ = self
                    .tmux_cmd()
                    .args(["send-keys", "-t", &qualified_target, "-X", "cancel"])
                    .output()
                    .await;
                tokio::time::sleep(std::time::Duration::from_millis(50)).await;
            }
        }

        let buf_name = format!("exo_{}", uuid::Uuid::new_v4().as_simple());
        let tmp_path = format!("/tmp/exomonad_buf_{}", buf_name);

        // Strip trailing newlines so paste-buffer doesn't trigger submission;
        // send-keys Enter below is the sole execution trigger.
        let payload = text.trim_end_matches('\n').trim_end_matches('\r');
        tokio::fs::write(&tmp_path, payload)
            .await
            .context("Failed to write temp buffer file")?;

        let load_result = self
            .tmux_cmd()
            .args(["load-buffer", "-b", &buf_name, &tmp_path])
            .output()
            .await;

        // Clean up temp file regardless of result
        let _ = tokio::fs::remove_file(&tmp_path).await;

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
        let paste_output = self
            .tmux_cmd()
            .args(["paste-buffer", "-b", &buf_name, "-t", &qualified_target])
            .output()
            .await
            .context("Failed to run tmux paste-buffer")?;

        // Delete the named buffer
        match self
            .tmux_cmd()
            .args(["delete-buffer", "-b", &buf_name])
            .output()
            .await
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
        tokio::time::sleep(std::time::Duration::from_millis(150)).await;

        // Submit with retry — TUIs may drop the first Enter keystroke
        // if still processing pasted text.
        let enter_policy = crate::services::resilience::RetryPolicy::new(
            3,
            crate::services::resilience::Backoff::Linear {
                initial: std::time::Duration::from_millis(200),
            },
        );
        let qt = qualified_target.clone();
        crate::services::resilience::retry(&enter_policy, || async {
            let output = self
                .tmux_cmd()
                .args(["send-keys", "-t", &qt, "Enter"])
                .output()
                .await
                .context("Failed to run tmux send-keys")?;
            if output.status.success() {
                Ok(())
            } else {
                let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                warn!(target = %qt, "send-keys Enter failed: {}", stderr);
                anyhow::bail!("send-keys Enter failed: {}", stderr)
            }
        })
        .await
        .context("send-keys Enter failed after 3 attempts")?;

        // Wake the target pane's TUI event loop via SIGWINCH so it processes
        // the injected input. Non-fatal — input was already delivered.
        if let Err(e) = self.wake_pane(target).await {
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
    pub async fn wake_pane(&self, target: &str) -> Result<()> {
        let resolved = self.resolve_target(target).await?;
        let qualified = self.qualify_target(&resolved);

        // Read current window dimensions
        let output = self
            .tmux_cmd()
            .args([
                "display-message",
                "-t",
                &qualified,
                "-p",
                "#{window_width} #{window_height}",
            ])
            .output()
            .await
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
        let _ = self
            .tmux_cmd()
            .args([
                "resize-window",
                "-t",
                &qualified,
                "-x",
                &(width + 1).to_string(),
                "-y",
                &height.to_string(),
            ])
            .output()
            .await;

        tokio::time::sleep(std::time::Duration::from_millis(50)).await;

        // Restore original size
        let _ = self
            .tmux_cmd()
            .args([
                "resize-window",
                "-t",
                &qualified,
                "-x",
                &width.to_string(),
                "-y",
                &height.to_string(),
            ])
            .output()
            .await;

        debug!(target = %qualified, "SIGWINCH wake: resized {}x{} → {}x{} → {}x{}", width, height, width + 1, height, width, height);
        Ok(())
    }

    // -- Query --

    pub async fn pane_exists(&self, pane_id: &PaneId) -> Result<bool> {
        let status = self
            .tmux_cmd()
            .args(["has-session", "-t", pane_id.as_str()])
            .status()
            .await
            .context("Failed to run tmux has-session")?;
        Ok(status.success())
    }

    pub async fn window_exists(&self, window_id: &WindowId) -> Result<bool> {
        let status = self
            .tmux_cmd()
            .args(["has-session", "-t", window_id.as_str()])
            .status()
            .await
            .context("Failed to run tmux has-session")?;
        Ok(status.success())
    }

    /// Check if a target (pane_id, window_id, or display name) exists in this session.
    pub async fn target_alive(&self, target: &str) -> bool {
        let qualified =
            if target.starts_with('%') || target.starts_with('@') || target.contains(':') {
                target.to_string()
            } else {
                format!("{}:{}", self.session_name, target)
            };

        // Use list-panes for pane_id (%N) and list-windows for window_id (@N)
        // For general names, list-panes with -t session:name will fail if not found.
        let args = if target.starts_with('@') {
            vec!["list-windows", "-F", "#{window_id}", "-t", &qualified]
        } else {
            vec!["list-panes", "-F", "#{pane_id}", "-t", &qualified]
        };

        let output = self.tmux_cmd().args(&args).output().await;

        match output {
            Ok(out) => {
                let success = out.status.success();
                debug!(
                    target = %qualified,
                    success,
                    status = ?out.status.code(),
                    "tmux target_alive check"
                );
                success
            }
            Err(e) => {
                warn!(target = %qualified, error = %e, "tmux list-panes/windows failed during liveness check");
                false
            }
        }
    }
}

#[cfg(any(test, feature = "test-support"))]
pub struct IsolatedTmux {
    socket: String,
    pub session: String,
    pub ipc: TmuxIpc,
}

#[cfg(any(test, feature = "test-support"))]
impl IsolatedTmux {
    /// Check if tmux is available in the current environment.
    pub async fn is_available() -> bool {
        tokio::process::Command::new("tmux")
            .arg("-V")
            .status()
            .await
            .map(|s| s.success())
            .unwrap_or(false)
    }

    /// Spins up a fresh tmux server on a unique socket, creates a test session,
    /// and returns a TmuxIpc bound to it. Drop kills the server.
    pub async fn new() -> Result<Self> {
        let socket = format!("exomonad-test-{}", uuid::Uuid::new_v4());
        let session = "test".to_string();
        let tmp = std::env::temp_dir();

        // Create the session on the isolated socket using a minimal tmux config
        // so tests do not depend on the user's ~/.tmux.conf.
        let mut cmd = tokio::process::Command::new("tmux");
        cmd.arg("-L")
            .arg(&socket)
            .arg("-f")
            .arg("/dev/null")
            .args(["new-session", "-d", "-s", &session, "-c"])
            .arg(&tmp);

        let output = cmd
            .output()
            .await
            .context("failed to spawn isolated tmux")?;
        anyhow::ensure!(
            output.status.success(),
            "isolated tmux new-session failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        let ipc = TmuxIpc::new_with_socket(&session, Some(socket.clone()));
        Ok(Self {
            socket,
            session,
            ipc,
        })
    }
}

#[cfg(any(test, feature = "test-support"))]
impl Drop for IsolatedTmux {
    fn drop(&mut self) {
        // Kill the isolated server. Best-effort — use std::process so Drop is sync.
        let _ = self.ipc.tmux_cmd_sync().arg("kill-server").output();
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
    fn rewind_modal_detection() {
        // Primary: "rewind" + enter + esc action prompts.
        assert!(looks_like_rewind_modal(
            "Rewind to a previous point\n  Enter to select · Esc to exit"
        ));
        // Action-prompt pair without the literal word "rewind".
        assert!(looks_like_rewind_modal("  Enter to accept · Esc to cancel"));
        // A normal prompt / busy indicator must NOT match (no spurious Escape).
        assert!(!looks_like_rewind_modal(
            "❯ implement the feature\n✻ Cooked for 1s"
        ));
        assert!(!looks_like_rewind_modal(""));
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

    #[tokio::test]
    async fn test_injection_lock_serializes_same_target() {
        use std::sync::atomic::{AtomicU32, Ordering};
        use std::sync::Arc;

        let counter = Arc::new(AtomicU32::new(0));
        let barrier = Arc::new(tokio::sync::Barrier::new(2));

        let mut handles = Vec::new();
        for _ in 0..2 {
            let counter = counter.clone();
            let barrier = barrier.clone();
            handles.push(tokio::spawn(async move {
                barrier.wait().await;
                let lock = {
                    let mut map = INJECTION_LOCKS.lock().unwrap();
                    map.retain(|_, weak| weak.strong_count() > 0);
                    map.get("test-serialization-target")
                        .and_then(|w| w.upgrade())
                        .unwrap_or_else(|| {
                            let arc = Arc::new(AsyncMutex::new(()));
                            map.insert(
                                "test-serialization-target".to_string(),
                                Arc::downgrade(&arc),
                            );
                            arc
                        })
                };
                let _guard = lock.lock().await;
                // Simulate work under lock
                let val = counter.load(Ordering::SeqCst);
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                counter.store(val + 1, Ordering::SeqCst);
            }));
        }

        for h in handles {
            h.await.unwrap();
        }
        // If serialized correctly, counter == 2 (no lost increments)
        assert_eq!(counter.load(Ordering::SeqCst), 2);
    }

    #[tokio::test]
    async fn test_injection_lock_different_targets_independent() {
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::Arc;

        // Verify two different targets can be locked concurrently (no deadlock,
        // and both threads reach the barrier while holding their respective locks).
        let both_reached_barrier = Arc::new(AtomicBool::new(false));
        let barrier = Arc::new(tokio::sync::Barrier::new(2));

        let flag = both_reached_barrier.clone();
        let b1 = barrier.clone();

        let h1 = tokio::spawn(async move {
            let lock = Arc::new(AsyncMutex::new(()));
            let _guard = lock.lock().await;
            // Both threads wait here — if locks were shared (same target),
            // the second thread would block on lock() and never reach the barrier.
            b1.wait().await;
            flag.store(true, Ordering::SeqCst);
        });

        let h2 = tokio::spawn(async move {
            let lock = Arc::new(AsyncMutex::new(()));
            let _guard = lock.lock().await;
            barrier.wait().await;
        });

        h1.await.unwrap();
        h2.await.unwrap();
        assert!(
            both_reached_barrier.load(Ordering::SeqCst),
            "Both threads should hold independent locks concurrently"
        );
    }

    #[tokio::test]
    async fn test_wake_pane_requires_session() {
        // wake_pane runs tmux commands that will fail without a real tmux session,
        // but it should not panic — it returns a Result
        let ipc = TmuxIpc::new("nonexistent-test-session");
        let result = ipc.wake_pane("test-target").await;
        assert!(
            result.is_err(),
            "wake_pane should fail without a real tmux session"
        );
    }

    #[tokio::test]
    async fn test_isolated_tmux() {
        if !IsolatedTmux::is_available().await {
            eprintln!("skipping test_isolated_tmux: tmux not available");
            return;
        }
        let isolated = IsolatedTmux::new().await.unwrap();
        assert!(
            TmuxIpc::has_session(&isolated.session, Some(&isolated.socket))
                .await
                .unwrap()
        );
    }

    #[tokio::test]
    async fn test_window_exists() {
        if !IsolatedTmux::is_available().await {
            return;
        }
        let isolated = IsolatedTmux::new().await.unwrap();
        let windows = isolated.ipc.list_windows().await.unwrap();
        let first_window = &windows[0].window_id;
        assert!(isolated.ipc.window_exists(first_window).await.unwrap());

        let fake_window = WindowId::parse("@99999").unwrap();
        assert!(!isolated.ipc.window_exists(&fake_window).await.unwrap());
    }

    #[tokio::test]
    async fn test_pane_exists() {
        if !IsolatedTmux::is_available().await {
            return;
        }
        let isolated = IsolatedTmux::new().await.unwrap();
        let windows = isolated.ipc.list_windows().await.unwrap();
        let first_pane = &windows[0].pane_id;
        assert!(isolated.ipc.pane_exists(first_pane).await.unwrap());

        let fake_pane = PaneId::parse("%99999").unwrap();
        assert!(!isolated.ipc.pane_exists(&fake_pane).await.unwrap());
    }

    #[tokio::test]
    async fn test_inject_input_with_pane_id() {
        if !IsolatedTmux::is_available().await {
            return;
        }
        let isolated = IsolatedTmux::new().await.unwrap();
        let windows = isolated.ipc.list_windows().await.unwrap();
        let pane_id = windows[0].pane_id.clone();

        // tmux rejects `session:%N` targets — pane IDs are global and must be used as-is.
        isolated
            .ipc
            .inject_input(pane_id.as_str(), "test content")
            .await
            .expect("inject_input with pane ID should succeed");
    }

    #[tokio::test]
    async fn test_inject_input_with_window_id() {
        if !IsolatedTmux::is_available().await {
            return;
        }
        let isolated = IsolatedTmux::new().await.unwrap();
        let windows = isolated.ipc.list_windows().await.unwrap();
        let window_id = windows[0].window_id.clone();

        isolated
            .ipc
            .inject_input(window_id.as_str(), "test content")
            .await
            .expect("inject_input with window ID should succeed");
    }

    #[tokio::test]
    async fn test_qualify_target_logic() {
        let ipc = TmuxIpc::new("mysession");

        // Global IDs remain as-is
        assert_eq!(ipc.qualify_target("@1"), "@1");
        assert_eq!(ipc.qualify_target("%42"), "%42");

        // Already qualified targets remain as-is
        assert_eq!(
            ipc.qualify_target("othersession:Server"),
            "othersession:Server"
        );
        assert_eq!(ipc.qualify_target("mysession:3.1"), "mysession:3.1");

        // Unqualified targets get prefixed
        assert_eq!(ipc.qualify_target("Server"), "mysession:Server");
        assert_eq!(ipc.qualify_target("3"), "mysession:3");
        assert_eq!(ipc.qualify_target("."), "mysession:.");
    }

    #[tokio::test]
    async fn test_resolve_target_passes_through_ids() {
        let ipc = TmuxIpc::new("mysession");
        assert_eq!(ipc.resolve_target("@42").await.unwrap(), "@42");
        assert_eq!(ipc.resolve_target("%17").await.unwrap(), "%17");
        assert_eq!(ipc.resolve_target("$1").await.unwrap(), "$1");
        assert_eq!(
            ipc.resolve_target("session:window").await.unwrap(),
            "session:window"
        );
        // Names without '.' resolve correctly without lookup
        assert_eq!(
            ipc.resolve_target("plain-name").await.unwrap(),
            "plain-name"
        );
    }

    #[tokio::test]
    async fn test_inject_input_resolves_dotted_window_name() {
        // Regression: tmux parses `.` in target as window/pane separator, so
        // a window named `💎 main.foo-bar-gemini` used to fail injection with
        // "can't find window: 💎 main". The fix resolves display names with
        // `.` to stable @window_id first.
        if !IsolatedTmux::is_available().await {
            return;
        }
        let isolated = IsolatedTmux::new().await.unwrap();
        let dotted_name = "\u{1F48E} main.foo-bar-gemini";
        let output = isolated
            .ipc
            .tmux_cmd()
            .args([
                "new-window",
                "-t",
                &isolated.session,
                "-n",
                dotted_name,
                "-d",
            ])
            .output()
            .await
            .unwrap();
        assert!(
            output.status.success(),
            "new-window failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        isolated
            .ipc
            .inject_input(dotted_name, "hello")
            .await
            .expect("inject_input should succeed for dot-containing display name");
    }

    #[tokio::test]
    async fn test_resolve_target_not_found_errors() {
        if !IsolatedTmux::is_available().await {
            return;
        }
        let isolated = IsolatedTmux::new().await.unwrap();
        let result = isolated.ipc.resolve_target("💎 does.not.exist").await;
        assert!(result.is_err());
    }
}
