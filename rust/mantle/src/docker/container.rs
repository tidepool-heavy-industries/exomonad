//! Docker container execution for mantle sessions.
//!
//! Runs Claude Code directly via `docker run -t` with TTY support.
//! Stream-json output is parsed on the host side.

use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use tracing::{debug, info, warn};

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

/// Configuration for a container instance.
#[derive(Debug, Clone)]
pub struct ContainerConfig {
    /// Docker image to use
    pub image: String,
    /// Path to the worktree to mount
    pub worktree_path: PathBuf,
    /// Path to hook socket (optional, for hook event forwarding)
    pub hub_socket: Option<PathBuf>,
    /// Path to Claude config directory (~/.claude)
    pub claude_home: PathBuf,
    /// Session ID (used for container naming)
    pub session_id: String,
    /// Claude Code arguments
    pub claude_args: Vec<String>,
    /// Timeout in seconds (0 = no timeout)
    pub timeout_secs: u64,
    /// Environment variables
    pub env_vars: HashMap<String, String>,
}

impl ContainerConfig {
    /// Create a new container config.
    pub fn new(
        session_id: String,
        worktree_path: PathBuf,
        claude_args: Vec<String>,
    ) -> Result<Self> {
        // Find Claude home directory
        let claude_home = dirs::home_dir()
            .map(|h| h.join(".claude"))
            .filter(|p| p.exists())
            .ok_or_else(|| DockerError::ClaudeHomeNotFound(PathBuf::from("~/.claude")))?;

        Ok(Self {
            image: "mantle-agent:latest".to_string(),
            worktree_path,
            hub_socket: None,
            claude_home,
            session_id,
            claude_args,
            timeout_secs: 0,
            env_vars: HashMap::new(),
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
pub fn run_claude_direct(config: &ContainerConfig) -> Result<RunResult> {
    let container_name = config.container_name();

    info!(
        container_name = %container_name,
        image = %config.image,
        session_id = %config.session_id,
        "Starting container"
    );

    let mut cmd = Command::new("docker");
    cmd.arg("run")
        .arg("-t") // Allocate TTY for stream-json
        .arg("--rm") // Auto-remove on exit
        .arg("--name")
        .arg(&container_name)
        // Mount worktree (read-write)
        .arg("-v")
        .arg(format!("{}:/workspace", config.worktree_path.display()))
        // Mount claude config to container user's home (image runs as 'user')
        .arg("-v")
        .arg(format!("{}:/home/user/.claude", config.claude_home.display()))
        // Working directory
        .arg("-w")
        .arg("/workspace");

    // Pass through auth env vars (API key or OAuth token from `claude setup-token`)
    if let Ok(key) = std::env::var("ANTHROPIC_API_KEY") {
        cmd.arg("-e").arg(format!("ANTHROPIC_API_KEY={}", key));
    }
    if let Ok(token) = std::env::var("CLAUDE_CODE_OAUTH_TOKEN") {
        cmd.arg("-e").arg(format!("CLAUDE_CODE_OAUTH_TOKEN={}", token));
    }

    // Add custom env vars
    for (k, v) in &config.env_vars {
        cmd.arg("-e").arg(format!("{}={}", k, v));
    }

    // Mount hook socket if configured
    if let Some(hub_socket) = &config.hub_socket {
        cmd.arg("-v")
            .arg(format!("{}:/mantle.sock", hub_socket.display()));
        cmd.arg("-e").arg("MANTLE_HOOK_SOCKET=/mantle.sock");
    }

    // Image and command
    cmd.arg(&config.image);
    cmd.arg("claude");
    cmd.args(&config.claude_args);

    debug!(command = ?cmd, "Spawning docker run");

    // Pipe stdout to parse stream-json, inherit stderr for Docker messages
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::inherit());

    let mut child = cmd.spawn().map_err(DockerError::Spawn)?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| DockerError::OutputRead("Failed to capture stdout".to_string()))?;

    // Parse stream-json output
    let mut parser = StreamParser::new();
    let reader = BufReader::new(stdout);

    for line_result in reader.lines() {
        match line_result {
            Ok(line) => {
                if let Some(event) = parser.process_line(&line) {
                    // Print humanized output to stderr for visibility
                    eprint_event_humanized(&event);
                } else if !line.is_empty() {
                    // Failed to parse as JSON - print raw line to stderr
                    eprintln!("{}", line);
                }
            }
            Err(e) => {
                warn!(error = %e, "Error reading stream-json line");
                break;
            }
        }
    }

    // Wait for container to exit
    let status = child.wait().map_err(|e| {
        DockerError::OutputRead(format!("Failed to wait for container: {}", e))
    })?;

    let exit_code = status.code().unwrap_or(-1);

    info!(
        container_name = %container_name,
        exit_code = %exit_code,
        "Container exited"
    );

    // Check for missing result event (known Claude bug #1920)
    let has_result = parser.has_result();
    if !has_result {
        warn!("No result event received from Claude (possible bug #1920)");
    }

    // Build result from parsed events
    let result = parser.build_result(exit_code, Some(config.session_id.clone()));

    // Debug log structured output presence
    if result.structured_output.is_some() {
        info!("Structured output received ({} bytes)",
              serde_json::to_string(&result.structured_output).unwrap_or_default().len());
    } else {
        warn!("No structured output in result. Full result: {:?}", result);
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
            claude_home: PathBuf::from("/tmp/.claude"),
            session_id: "abc12345-6789".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        };

        assert_eq!(config.container_name(), "mantle-abc12345");
    }

    #[test]
    fn test_container_name_short_id() {
        let config = ContainerConfig {
            image: "test".to_string(),
            worktree_path: PathBuf::from("/tmp"),
            hub_socket: None,
            claude_home: PathBuf::from("/tmp/.claude"),
            session_id: "abc".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        };

        assert_eq!(config.container_name(), "mantle-abc");
    }

    #[test]
    fn test_config_builder_methods() {
        let config = ContainerConfig {
            image: "original".to_string(),
            worktree_path: PathBuf::from("/tmp"),
            hub_socket: None,
            claude_home: PathBuf::from("/tmp/.claude"),
            session_id: "test".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        }
        .with_image("custom-image")
        .with_timeout(300)
        .with_env("MY_VAR", "my_value");

        assert_eq!(config.image, "custom-image");
        assert_eq!(config.timeout_secs, 300);
        assert_eq!(config.env_vars.get("MY_VAR"), Some(&"my_value".to_string()));
    }
}
