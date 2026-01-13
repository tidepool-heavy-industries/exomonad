//! Docker container execution for mantle sessions.
//!
//! Runs Claude Code directly via `docker run -t` with TTY support.
//! Stream-json output is parsed on the host side.

use std::collections::HashMap;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::mpsc::TryRecvError;
use std::time::{Duration, Instant};

use crate::config::Config;
use crate::docker::control_listener::ControlListener;
use crate::stream_parser::StreamParser;
use mantle_shared::events::RunResult;
use mantle_shared::humanize::eprint_event_humanized;

/// Error types for Docker operations.
#[derive(Debug, thiserror::Error)]
pub enum DockerError {
    #[error("Failed to spawn docker process: {0}")]
    Spawn(#[source] std::io::Error),

    #[error("Failed to read container output: {0}")]
    OutputRead(String),

    #[error("Claude home not found at {0}")]
    ClaudeHomeNotFound(PathBuf),
}

pub type Result<T> = std::result::Result<T, DockerError>;

/// Kill a running Docker container by name.
///
/// Sends SIGKILL to the container, which terminates it immediately.
/// Returns Ok(()) if the kill command succeeded, or the container was already dead.
fn docker_kill(container_name: &str) -> std::io::Result<()> {
    let output = Command::new("docker")
        .args(["kill", container_name])
        .output()?;

    if output.status.success() {
        Ok(())
    } else {
        // Check if it's just "container not found" (already dead)
        let stderr = String::from_utf8_lossy(&output.stderr);
        if stderr.contains("No such container") || stderr.contains("is not running") {
            // Container already gone, that's fine
            Ok(())
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("docker kill failed: {}", stderr.trim()),
            ))
        }
    }
}

/// Per-session logger that writes timestamped logs to `.mantle/logs/{session_id}.log`.
///
/// Captures all mantle operations, Claude Code stderr, and stream-json events in chronological order.
struct SessionLogger {
    file: BufWriter<std::fs::File>,
}

impl SessionLogger {
    /// Create a new session logger.
    ///
    /// Creates `.mantle/logs/` directory if it doesn't exist.
    fn new(session_id: &str) -> Result<Self> {
        let log_dir = PathBuf::from(".mantle/logs");
        std::fs::create_dir_all(&log_dir)
            .map_err(|e| DockerError::OutputRead(format!("Failed to create log directory: {}", e)))?;

        let log_path = log_dir.join(format!("{}.log", session_id));

        let file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&log_path)
            .map_err(|e| DockerError::OutputRead(format!("Failed to open log file: {}", e)))?;

        Ok(Self {
            file: BufWriter::new(file),
        })
    }

    /// Log a message with timestamp and tag.
    ///
    /// Format: `[2026-01-10 15:23:41.123] [TAG] message`
    ///
    /// Writes to both the log file and stderr for real-time visibility.
    fn log(&mut self, tag: &str, message: &str) {
        let timestamp = chrono::Local::now().format("%Y-%m-%d %H:%M:%S%.3f");
        let line = format!("[{}] [{}] {}\n", timestamp, tag, message);

        // Write to file
        let _ = self.file.write_all(line.as_bytes());
        let _ = self.file.flush();

        // Also write to stderr for real-time visibility
        eprint!("{}", line);
    }
}

/// How to provide Claude authentication credentials to the container.
#[derive(Debug, Clone)]
pub enum AuthMount {
    /// Mount a Docker named volume (e.g., "tidepool-claude-auth").
    /// Use this when auth was done in a shared container via `claude login`.
    Volume(String),
    /// Bind mount a host directory (e.g., "/Users/foo/.claude").
    /// Use this for local development with host auth.
    BindMount(PathBuf),
}

/// Configuration for a container instance.
#[derive(Debug, Clone)]
pub struct ContainerConfig {
    /// Docker image to use
    pub image: String,
    /// Path to the worktree to mount
    pub worktree_path: PathBuf,
    /// Path to hook socket (optional, for hook event forwarding)
    pub hub_socket: Option<PathBuf>,
    /// How to provide Claude auth credentials
    pub auth_mount: AuthMount,
    /// Named Docker volume for shared cabal package store (optional)
    pub cabal_store_volume: Option<String>,
    /// Named Docker volume for Stack build cache (optional)
    pub stack_cache_volume: Option<String>,
    /// Session ID (used for container naming)
    pub session_id: String,
    /// Claude Code arguments
    pub claude_args: Vec<String>,
    /// Timeout in seconds (0 = no timeout)
    pub timeout_secs: u64,
    /// Environment variables
    pub env_vars: HashMap<String, String>,
    /// Path to decision tools JSON file on host (mounted into container)
    pub decision_tools_file: Option<PathBuf>,
}

impl ContainerConfig {
    /// Create a new container config.
    ///
    /// Loads mantle config from `~/.config/mantle/config.toml` to determine:
    /// - Auth mount (named volume vs host bind mount)
    /// - Docker image
    ///
    /// Falls back to host's `~/.claude` if no auth_volume configured.
    pub fn new(
        session_id: String,
        worktree_path: PathBuf,
        claude_args: Vec<String>,
    ) -> Result<Self> {
        // Load config (returns defaults if file doesn't exist)
        let config = Config::load().map_err(|e| {
            DockerError::OutputRead(format!("Failed to load config: {}", e))
        })?;

        // Determine auth mount from config
        let auth_mount = if let Some(volume) = config.docker.auth_volume {
            tracing::info!("Using auth volume from config: {}", volume);
            AuthMount::Volume(volume)
        } else {
            // Fall back to host's ~/.claude
            let claude_home = dirs::home_dir()
                .map(|h| h.join(".claude"))
                .filter(|p| p.exists())
                .ok_or_else(|| DockerError::ClaudeHomeNotFound(PathBuf::from("~/.claude")))?;
            tracing::info!("Using host auth bind mount: {:?}", claude_home);
            AuthMount::BindMount(claude_home)
        };

        // Use image from config or default
        let image = config
            .docker
            .image
            .unwrap_or_else(|| "mantle-agent:latest".to_string());

        // Get cabal store volume from config
        let cabal_store_volume = config.docker.cabal_store_volume;

        // Get stack cache volume from config
        let stack_cache_volume = config.docker.stack_cache_volume;

        Ok(Self {
            image,
            worktree_path,
            hub_socket: None,
            auth_mount,
            cabal_store_volume,
            stack_cache_volume,
            session_id,
            claude_args,
            timeout_secs: 0,
            env_vars: HashMap::new(),
            decision_tools_file: None,
        })
    }

    /// Set the Docker image.
    pub fn with_image(mut self, image: impl Into<String>) -> Self {
        self.image = image.into();
        self
    }

    /// Set the timeout.
    pub fn with_timeout(mut self, timeout_secs: u64) -> Self {
        self.timeout_secs = timeout_secs;
        self
    }

    /// Add an environment variable.
    pub fn with_env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env_vars.insert(key.into(), value.into());
        self
    }

    /// Set the hook socket path.
    pub fn with_hub_socket(mut self, socket: PathBuf) -> Self {
        self.hub_socket = Some(socket);
        self
    }

    /// Set decision tool definitions (JSON array).
    ///
    /// Writes the tools JSON to a temp file on the host, which is then
    /// mounted into the container. This avoids shell escaping issues with
    /// passing JSON through environment variables.
    ///
    /// These tools are served by `mantle-agent mcp` to Claude Code for
    /// sum type structured outputs.
    pub fn with_decision_tools(mut self, tools_json: &str) -> Result<Self> {
        // Write tools JSON to a temp file
        let tools_file = PathBuf::from(format!("/tmp/mantle-tools-{}.json", &self.session_id[..8]));
        std::fs::write(&tools_file, tools_json).map_err(|e| {
            DockerError::OutputRead(format!("Failed to write decision tools file: {}", e))
        })?;
        self.decision_tools_file = Some(tools_file);
        Ok(self)
    }

    /// Generate container name from session ID.
    pub fn container_name(&self) -> String {
        format!("mantle-{}", &self.session_id[..8.min(self.session_id.len())])
    }
}

/// Run Claude Code directly in container with TTY for stream-json support.
///
/// Uses `docker run -t` to allocate a TTY, enabling Claude's stream-json output.
/// Parses the stream and returns a [`RunResult`] directly.
///
/// Container dies when this process dies - no orphans possible.
///
/// # Arguments
/// * `config` - Container configuration
/// * `event_sink` - Optional callback invoked for each parsed `StreamEvent`.
///   Used for forwarding events to hub for real-time supervision.
pub fn run_claude_direct<F>(config: &ContainerConfig, mut event_sink: Option<F>) -> Result<RunResult>
where
    F: FnMut(&mantle_shared::events::StreamEvent),
{
    let container_name = config.container_name();

    // Create session logger for file-based logging
    let mut logger = SessionLogger::new(&config.session_id)?;
    logger.log("MANTLE", &format!("Starting container {}", container_name));
    logger.log("MANTLE", &format!("Image: {}", config.image));
    logger.log("MANTLE", &format!("Worktree: {}", config.worktree_path.display()));

    // Create TCP control listener if decision tools are configured
    let has_decision_tools = config.decision_tools_file.is_some();
    let (control_port, tool_call_collector, decision_rx) = if has_decision_tools {
        let listener = ControlListener::bind().map_err(|e| {
            DockerError::OutputRead(format!("Failed to create control listener: {}", e))
        })?;
        let port = listener.port();
        logger.log("MANTLE", &format!("Control listener bound to port {}", port));

        let (collector, rx) = listener.spawn();

        (Some(port), Some(collector), Some(rx))
    } else {
        (None, None, None)
    };

    let mut cmd = Command::new("docker");
    cmd.arg("run")
        .arg("-t") // Allocate TTY for stream-json
        .arg("--rm") // Auto-remove on exit
        .arg("--name")
        .arg(&container_name)
        // Mount worktree (read-write)
        .arg("-v")
        .arg(format!("{}:/workspace", config.worktree_path.display()));

    // Mount main repo's .git directory at same host path so worktree gitdir resolves
    // Worktree is at: repo/.mantle/worktrees/slug-xxx
    // Main .git is at: repo/.git
    if let Some(mantle_dir) = config.worktree_path.parent() {  // .mantle/worktrees
        if let Some(mantle_parent) = mantle_dir.parent() {     // .mantle
            if let Some(repo_root) = mantle_parent.parent() {  // repo
                let git_dir = repo_root.join(".git");
                if git_dir.exists() {
                    logger.log("MANTLE", &format!("Mounting .git for worktree support: {}", git_dir.display()));
                    // Mount read-write: worktrees need to write to .git/worktrees/<name>/index.lock
                    cmd.arg("-v")
                        .arg(format!("{}:{}", git_dir.display(), git_dir.display()));
                }
            }
        }
    }

    // Mount claude auth credentials
    // Either a named Docker volume (shared auth) or host bind mount (local dev)
    match &config.auth_mount {
        AuthMount::Volume(vol_name) => {
            let mount_arg = format!("{}:/home/user/.claude", vol_name);
            logger.log("MANTLE", &format!("Using auth volume: {}", vol_name));
            logger.log("MANTLE", &format!("Auth mount arg: {}", mount_arg));
            cmd.arg("-v").arg(&mount_arg);
        }
        AuthMount::BindMount(path) => {
            let mount_arg = format!("{}:/home/user/.claude", path.display());
            logger.log("MANTLE", &format!("Using auth bind mount: {:?}", path));
            logger.log("MANTLE", &format!("Auth mount arg: {}", mount_arg));
            logger.log("MANTLE", &format!("Auth path exists: {}", path.exists()));
            cmd.arg("-v").arg(&mount_arg);
        }
    }

    // Mount cabal store volume if configured (shared package cache)
    if let Some(ref cabal_vol) = config.cabal_store_volume {
        logger.log("MANTLE", &format!("Using cabal store volume: {}", cabal_vol));
        cmd.arg("-v")
            .arg(format!("{}:/home/user/.cabal/store", cabal_vol));
    }

    // Mount stack cache volume if configured (shared snapshots/programs/indices)
    if let Some(ref stack_vol) = config.stack_cache_volume {
        logger.log("MANTLE", &format!("Using stack cache volume: {}", stack_vol));
        cmd.arg("-v")
            .arg(format!("{}:/home/user/.stack", stack_vol));
    }

    // Working directory
    cmd.arg("-w").arg("/workspace");

    // Claude Code auth is handled via mounted volume (~/.claude)

    // Add custom env vars
    for (k, v) in &config.env_vars {
        cmd.arg("-e").arg(format!("{}={}", k, v));
    }

    // Configure TCP control connection for decision tools
    // Container connects back to host via host.docker.internal
    if let Some(port) = control_port {
        // Add host.docker.internal mapping (required on Linux, built-in on macOS)
        cmd.arg("--add-host=host.docker.internal:host-gateway");
        cmd.arg("-e").arg("MANTLE_CONTROL_HOST=host.docker.internal");
        cmd.arg("-e").arg(format!("MANTLE_CONTROL_PORT={}", port));
    }

    // Mount decision tools file if configured
    // This avoids passing JSON through env vars which causes shell escaping issues
    if let Some(ref tools_file) = config.decision_tools_file {
        logger.log("MANTLE", &format!("Mounting decision tools file: {}", tools_file.display()));
        cmd.arg("-v")
            .arg(format!("{}:/decision-tools.json:ro", tools_file.display()));
        cmd.arg("-e").arg("MANTLE_DECISION_TOOLS_FILE=/decision-tools.json");
    }

    // Image and command
    cmd.arg(&config.image);
    cmd.arg("claude");
    cmd.args(&config.claude_args);

    logger.log("MANTLE", &format!("Docker command: {:?}", cmd));
    logger.log("MANTLE", &format!("Config - image: {}", config.image));
    logger.log("MANTLE", &format!("Config - session_id: {}", config.session_id));
    logger.log("MANTLE", &format!("Config - worktree_path: {}", config.worktree_path.display()));

    // Pipe both stdout (for stream-json) and stderr (for logging)
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    logger.log("MANTLE", "Spawning Docker container...");
    let mut child = cmd.spawn().map_err(|e| {
        logger.log("MANTLE", &format!("Failed to spawn container: {}", e));
        DockerError::Spawn(e)
    })?;
    logger.log("MANTLE", "Container spawned successfully");
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| DockerError::OutputRead("Failed to capture stdout".to_string()))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| DockerError::OutputRead("Failed to capture stderr".to_string()))?;

    // Spawn thread to capture Claude's stderr to log file AND collect for error reporting
    let session_id_for_stderr = config.session_id.clone();
    let (stderr_tx, stderr_rx) = std::sync::mpsc::channel::<String>();
    std::thread::spawn(move || {
        let mut stderr_logger = SessionLogger::new(&session_id_for_stderr).ok()?;
        let reader = BufReader::new(stderr);
        let mut collected_lines = Vec::new();
        for line in reader.lines().flatten() {
            stderr_logger.log("CLAUDE", &line);
            collected_lines.push(line);
        }
        // Send collected stderr back to main thread
        let _ = stderr_tx.send(collected_lines.join("\n"));
        Some(())
    });

    // Parse stream-json output with detailed logging
    let mut parser = StreamParser::new();
    let reader = BufReader::new(stdout);
    let mut turn_count = 0;
    let mut tool_use_count = 0;

    for line_result in reader.lines() {
        match line_result {
            Ok(line) => {
                if let Some(event) = parser.process_line(&line) {
                    // Log raw event
                    if let Ok(json) = serde_json::to_string(&event) {
                        logger.log("EVENT", &json);
                    }

                    // Track turn count from assistant events
                    if let mantle_shared::events::StreamEvent::Assistant(ref a) = event {
                        for block in &a.message.content {
                            if let mantle_shared::events::ContentBlock::ToolUse { name, .. } = block {
                                tool_use_count += 1;
                                logger.log("MANTLE", &format!("Turn {} - Tool: {}", turn_count, name));
                            }
                        }
                        turn_count += 1;

                        // Log turn milestones
                        if turn_count % 10 == 0 {
                            logger.log("MANTLE", &format!("Turn {} milestone ({} tools used)", turn_count, tool_use_count));
                        }
                        if turn_count >= 100 {
                            logger.log("MANTLE", &format!("⚠️  EXCESSIVE TURNS: {} (possible infinite loop)", turn_count));
                        }
                    }

                    // Print humanized output to stderr for visibility
                    eprint_event_humanized(&event);

                    // Forward event to sink (for hub streaming)
                    if let Some(ref mut sink) = event_sink {
                        sink(&event);
                    }
                } else if !line.is_empty() {
                    // Failed to parse as JSON - log raw line
                    logger.log("UNKNOWN", &line);
                }
            }
            Err(e) => {
                logger.log("ERROR", &format!("Error reading stream-json line: {}", e));
                break;
            }
        }
    }

    logger.log("MANTLE", &format!("Stream parsing complete: {} turns, {} tools", turn_count, tool_use_count));

    // Wait for container to exit with decision deadline tracking
    // If a decision:: tool is called, we set a 30s deadline for graceful exit
    // After deadline, we kill the container (not an error - tool calls are captured)
    const DECISION_TIMEOUT_SECS: u64 = 30;
    const POLL_INTERVAL_MS: u64 = 50;
    const LOG_INTERVAL_SECS: u64 = 10;

    let mut deadline: Option<Instant> = None;
    let mut last_log_remaining: Option<u64> = None;
    let mut killed_by_deadline = false;

    let exit_code = loop {
        // Check if child has exited
        match child.try_wait() {
            Ok(Some(status)) => {
                // Child exited naturally
                if deadline.is_some() {
                    let elapsed = deadline.map(|d| Instant::now().duration_since(d.checked_sub(Duration::from_secs(DECISION_TIMEOUT_SECS)).unwrap_or(d)));
                    logger.log("DECISION", &format!("Claude exited naturally (took {:?})", elapsed.unwrap_or_default()));
                }
                break status.code().unwrap_or(-1);
            }
            Ok(None) => {
                // Still running, continue polling
            }
            Err(e) => {
                logger.log("ERROR", &format!("Failed to wait for container: {}", e));
                break -1;
            }
        }

        // Check for decision tool notification (only if we have a receiver)
        if let Some(ref rx) = decision_rx {
            match rx.try_recv() {
                Ok(tool_call) => {
                    if deadline.is_none() {
                        logger.log("DECISION", &format!(
                            "Tool received: {}, starting {}s deadline",
                            tool_call.name, DECISION_TIMEOUT_SECS
                        ));
                        deadline = Some(Instant::now() + Duration::from_secs(DECISION_TIMEOUT_SECS));
                        last_log_remaining = Some(DECISION_TIMEOUT_SECS);
                    }
                    // Subsequent decision tools don't reset the deadline
                }
                Err(TryRecvError::Empty) => {
                    // No decision yet, continue
                }
                Err(TryRecvError::Disconnected) => {
                    // Channel closed (listener thread exited), continue waiting
                }
            }
        }

        // Check deadline and log countdown
        if let Some(dl) = deadline {
            let now = Instant::now();
            if now >= dl {
                // Deadline exceeded - kill the container
                logger.log("DECISION", "Deadline reached, killing container");
                if let Err(e) = docker_kill(&container_name) {
                    logger.log("ERROR", &format!("Failed to kill container: {}", e));
                } else {
                    logger.log("DECISION", "Container killed successfully");
                }
                killed_by_deadline = true;

                // Wait for container to actually exit after kill
                match child.wait() {
                    Ok(status) => break status.code().unwrap_or(-1),
                    Err(e) => {
                        logger.log("ERROR", &format!("Failed to wait after kill: {}", e));
                        break -1;
                    }
                }
            }

            // Log countdown every 10 seconds
            let remaining_secs = (dl - now).as_secs();
            let log_bucket = (remaining_secs / LOG_INTERVAL_SECS) * LOG_INTERVAL_SECS;
            if let Some(last) = last_log_remaining {
                if log_bucket < last && remaining_secs > 0 {
                    logger.log("DECISION", &format!("Waiting for exit... {}s remaining", remaining_secs));
                    last_log_remaining = Some(log_bucket);
                }
            }
        }

        // Sleep before next poll
        std::thread::sleep(Duration::from_millis(POLL_INTERVAL_MS));
    };

    // Log if killed by deadline (this is not an error, just unusual)
    if killed_by_deadline {
        logger.log("DECISION", "Session terminated by deadline (tool calls captured)");
    }

    logger.log("MANTLE", &format!(
        "Container exited: exit_code={}, turns={}, tools={}",
        exit_code, turn_count, tool_use_count
    ));

    // Check for missing result event (known Claude bug #1920)
    let has_result = parser.has_result();
    if !has_result {
        logger.log("WARNING", "No result event received from Claude (possible bug #1920)");
    }

    // Collect tool calls from control socket listener
    let tool_calls = if let Some(collector) = tool_call_collector {
        let calls = collector.collect();
        if !calls.is_empty() {
            logger.log("MANTLE", &format!("Collected {} decision tool calls", calls.len()));
            for call in &calls {
                logger.log("TOOL_CALL", &format!("{}: {}", call.name, call.input));
            }
        }
        calls
    } else {
        vec![]
    };

    // Collect stderr from background thread (with timeout to avoid blocking forever)
    // Only include stderr in result when there's an error (to avoid bloating successful results)
    let stderr_output = if exit_code != 0 || !parser.has_result() {
        // Wait briefly for stderr thread to finish (it should be done by now since process exited)
        stderr_rx.recv_timeout(std::time::Duration::from_secs(2)).ok()
            .filter(|s| !s.is_empty())
    } else {
        None
    };

    if let Some(ref stderr) = stderr_output {
        let line_count = stderr.lines().count();
        logger.log("STDERR", &format!("Captured {} lines of stderr for error diagnosis", line_count));
    }

    // Build result from parsed events
    let result = parser.build_result(exit_code, Some(config.session_id.clone()), tool_calls, stderr_output);

    // Save event stream to debug file if turn count is suspiciously high
    if turn_count > 50 || exit_code != 0 {
        let debug_path = format!(".mantle/debug/{}.events.json", config.session_id);
        if let Some(parent) = std::path::Path::new(&debug_path).parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        if let Ok(json) = serde_json::to_string_pretty(&result.events) {
            if let Err(e) = std::fs::write(&debug_path, json) {
                logger.log("WARNING", &format!("Failed to write debug events to {}: {}", debug_path, e));
            } else {
                logger.log("MANTLE", &format!("Saved debug event stream: {} ({} events)", debug_path, result.events.len()));
            }
        }
    }

    // Log structured output for debugging
    if let Some(ref output) = result.structured_output {
        if let Ok(json) = serde_json::to_string_pretty(output) {
            logger.log("STRUCTURED_OUTPUT", &json);
        }
    } else {
        logger.log("WARNING", "No structured output in result");
    }

    // Log result_text for debugging
    if let Some(ref text) = result.result {
        // Truncate if very long, but log enough to be useful
        let truncated = if text.len() > 2000 {
            format!("{}... [truncated, {} total chars]", &text[..2000], text.len())
        } else {
            text.clone()
        };
        logger.log("RESULT_TEXT", &truncated);
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_container_name_generation() {
        let config = ContainerConfig {
            image: "test".to_string(),
            worktree_path: PathBuf::from("/tmp"),
            hub_socket: None,
            auth_mount: AuthMount::BindMount(PathBuf::from("/tmp/.claude")),
            cabal_store_volume: None,
            stack_cache_volume: None,
            session_id: "abc12345-6789".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
            decision_tools_file: None,
        };

        assert_eq!(config.container_name(), "mantle-abc12345");
    }

    #[test]
    fn test_container_name_short_id() {
        let config = ContainerConfig {
            image: "test".to_string(),
            worktree_path: PathBuf::from("/tmp"),
            hub_socket: None,
            auth_mount: AuthMount::BindMount(PathBuf::from("/tmp/.claude")),
            cabal_store_volume: None,
            stack_cache_volume: None,
            session_id: "abc".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
            decision_tools_file: None,
        };

        assert_eq!(config.container_name(), "mantle-abc");
    }

    #[test]
    fn test_config_builder_methods() {
        let config = ContainerConfig {
            image: "original".to_string(),
            worktree_path: PathBuf::from("/tmp"),
            hub_socket: None,
            auth_mount: AuthMount::Volume("test-vol".to_string()),
            cabal_store_volume: None,
            stack_cache_volume: None,
            session_id: "test".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
            decision_tools_file: None,
        }
        .with_image("custom-image")
        .with_timeout(300)
        .with_env("MY_VAR", "my_value");

        assert_eq!(config.image, "custom-image");
        assert_eq!(config.timeout_secs, 300);
        assert_eq!(config.env_vars.get("MY_VAR"), Some(&"my_value".to_string()));
    }

    #[test]
    fn test_with_decision_tools() {
        let tools_json = r#"[{"name":"decision::approve","description":"Approve"}]"#;
        let config = ContainerConfig {
            image: "test".to_string(),
            worktree_path: PathBuf::from("/tmp"),
            hub_socket: None,
            auth_mount: AuthMount::BindMount(PathBuf::from("/tmp/.claude")),
            cabal_store_volume: None,
            stack_cache_volume: None,
            session_id: "testabcd".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
            decision_tools_file: None,
        }
        .with_decision_tools(tools_json)
        .expect("with_decision_tools should succeed");

        // Should have written to a file
        assert!(config.decision_tools_file.is_some());
        let file_path = config.decision_tools_file.as_ref().unwrap();
        assert!(file_path.exists());

        // File should contain the tools JSON
        let contents = std::fs::read_to_string(file_path).unwrap();
        assert_eq!(contents, tools_json);

        // Cleanup
        let _ = std::fs::remove_file(file_path);
    }
}
