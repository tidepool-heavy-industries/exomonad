//! Docker container lifecycle management.
//!
//! Provides two execution modes:
//! - **Attached mode** (primary): Container runs in foreground via `docker run`,
//!   dies when parent dies, result written to stdout
//! - **Detached mode** (via bollard): For cleanup operations and hub socket mode

use bollard::container::LogOutput;
use bollard::models::{ContainerCreateBody, HostConfig, Mount, MountTypeEnum};
use bollard::query_parameters::{
    CreateContainerOptions, LogsOptions, RemoveContainerOptions, StartContainerOptions,
    StopContainerOptions, WaitContainerOptions,
};
use bollard::Docker;
use futures_util::StreamExt;
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use tracing::{debug, info, warn};

/// Error types for Docker operations.
#[derive(Debug, thiserror::Error)]
pub enum DockerError {
    #[error("Failed to connect to Docker daemon: {0}")]
    Connection(#[source] bollard::errors::Error),

    #[error("Failed to create container: {0}")]
    Create(#[source] bollard::errors::Error),

    #[error("Failed to start container: {0}")]
    Start(#[source] bollard::errors::Error),

    #[error("Failed to wait for container: {0}")]
    Wait(#[source] bollard::errors::Error),

    #[error("Failed to stop container: {0}")]
    Stop(#[source] bollard::errors::Error),

    #[error("Failed to remove container: {0}")]
    Remove(#[source] bollard::errors::Error),

    #[error("Failed to get container logs: {0}")]
    Logs(#[source] bollard::errors::Error),

    #[error("Container exited with code {0}")]
    ExitCode(i64),

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
    /// Path to hub socket for result communication (optional, for mantle-hub mode)
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
    /// Create a new container config for attached mode.
    ///
    /// In attached mode, the container runs in the foreground and writes
    /// its result to stdout, which the parent process reads directly.
    /// Container dies when parent dies - no orphans.
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

    /// Create a new container config with hub socket mode.
    ///
    /// Used when coordinating with mantle-hub for session visualization.
    pub fn new_with_hub(
        session_id: String,
        worktree_path: PathBuf,
        hub_socket: PathBuf,
        claude_args: Vec<String>,
    ) -> Result<Self> {
        let claude_home = dirs::home_dir()
            .map(|h| h.join(".claude"))
            .filter(|p| p.exists())
            .ok_or_else(|| DockerError::ClaudeHomeNotFound(PathBuf::from("~/.claude")))?;

        Ok(Self {
            image: "mantle-agent:latest".to_string(),
            worktree_path,
            hub_socket: Some(hub_socket),
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

    /// Generate container name from session ID.
    pub fn container_name(&self) -> String {
        format!("mantle-{}", &self.session_id[..8.min(self.session_id.len())])
    }

    /// Build the command for attached mode (--stdout).
    fn build_attached_command(&self) -> Vec<String> {
        let mut cmd = vec!["wrap".to_string(), "--stdout".to_string()];

        cmd.push("--cwd".to_string());
        cmd.push("/workspace".to_string());
        cmd.push("--session-tag".to_string());
        cmd.push(self.session_id.clone());

        if self.timeout_secs > 0 {
            cmd.push("--timeout".to_string());
            cmd.push(self.timeout_secs.to_string());
        }

        cmd.push("--".to_string());
        cmd.extend(self.claude_args.clone());

        cmd
    }

    /// Build the command for hub socket mode.
    fn build_hub_command(&self) -> Vec<String> {
        let mut cmd = vec![
            "wrap".to_string(),
            "--hub-socket".to_string(),
            "/tmp/mantle.sock".to_string(),
        ];

        cmd.push("--cwd".to_string());
        cmd.push("/workspace".to_string());
        cmd.push("--session-tag".to_string());
        cmd.push(self.session_id.clone());

        if self.timeout_secs > 0 {
            cmd.push("--timeout".to_string());
            cmd.push(self.timeout_secs.to_string());
        }

        cmd.push("--".to_string());
        cmd.extend(self.claude_args.clone());

        cmd
    }

    /// Build environment variables for the container.
    fn build_env(&self) -> Vec<String> {
        let mut env: Vec<String> = self
            .env_vars
            .iter()
            .map(|(k, v)| format!("{}={}", k, v))
            .collect();

        // Pass through ANTHROPIC_API_KEY if set
        if let Ok(key) = std::env::var("ANTHROPIC_API_KEY") {
            env.push(format!("ANTHROPIC_API_KEY={}", key));
        }

        env
    }

    /// Build mount configuration for attached mode.
    fn build_mounts(&self) -> Vec<Mount> {
        let mut mounts = vec![
            // Worktree: read-write for Claude to modify code
            Mount {
                target: Some("/workspace".to_string()),
                source: Some(self.worktree_path.display().to_string()),
                typ: Some(MountTypeEnum::BIND),
                read_only: Some(false),
                ..Default::default()
            },
            // Claude config: read-only (contains credentials)
            Mount {
                target: Some("/root/.claude".to_string()),
                source: Some(self.claude_home.display().to_string()),
                typ: Some(MountTypeEnum::BIND),
                read_only: Some(true),
                ..Default::default()
            },
        ];

        // Add hub socket mount if configured
        if let Some(hub_socket) = &self.hub_socket {
            mounts.push(Mount {
                target: Some("/tmp/mantle.sock".to_string()),
                source: Some(hub_socket.display().to_string()),
                typ: Some(MountTypeEnum::BIND),
                read_only: Some(false),
                ..Default::default()
            });
        }

        mounts
    }
}

// ============================================================================
// Attached Mode Execution (Primary)
// ============================================================================

/// Run container in attached mode - container dies when this process dies.
///
/// Uses `docker run` (not bollard) so the container is a child process.
/// When the parent process dies (SIGTERM, crash, etc.), Docker kills the container.
///
/// Returns the JSON result from stdout.
pub fn run_attached(config: &ContainerConfig) -> Result<String> {
    let container_name = config.container_name();

    info!(
        container_name = %container_name,
        image = %config.image,
        session_id = %config.session_id,
        "Starting container (attached mode)"
    );

    let mut cmd = Command::new("docker");
    cmd.arg("run")
        .arg("--rm") // Auto-remove on exit
        .arg("--name")
        .arg(&container_name)
        // Mount worktree (read-write)
        .arg("-v")
        .arg(format!(
            "{}:/workspace",
            config.worktree_path.display()
        ))
        // Mount claude home (read-only)
        .arg("-v")
        .arg(format!(
            "{}:/root/.claude:ro",
            config.claude_home.display()
        ))
        // Working directory
        .arg("-w")
        .arg("/workspace");

    // Pass through ANTHROPIC_API_KEY
    if let Ok(key) = std::env::var("ANTHROPIC_API_KEY") {
        cmd.arg("-e").arg(format!("ANTHROPIC_API_KEY={}", key));
    }

    // Add custom env vars
    for (k, v) in &config.env_vars {
        cmd.arg("-e").arg(format!("{}={}", k, v));
    }

    // Mount hub socket if configured
    if let Some(hub_socket) = &config.hub_socket {
        cmd.arg("-v")
            .arg(format!("{}:/tmp/mantle.sock", hub_socket.display()));
    }

    // Image
    cmd.arg(&config.image);

    // Command args (use hub or stdout mode)
    if config.hub_socket.is_some() {
        cmd.args(config.build_hub_command());
    } else {
        cmd.args(config.build_attached_command());
    }

    debug!(command = ?cmd, "Spawning docker run");

    // Capture stdout (JSON result), pass through stderr (humanized output)
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::inherit());

    let output = cmd.output().map_err(DockerError::Spawn)?;

    let exit_code = output.status.code().unwrap_or(-1);

    info!(
        container_name = %container_name,
        exit_code = %exit_code,
        "Container exited"
    );

    if !output.status.success() {
        return Err(DockerError::ExitCode(exit_code as i64));
    }

    String::from_utf8(output.stdout)
        .map_err(|e| DockerError::OutputRead(e.to_string()))
}

// ============================================================================
// Detached Mode (Bollard) - For cleanup and hub socket mode
// ============================================================================

/// Manages Docker container lifecycle for mantle sessions.
///
/// Used for:
/// - Pre-run cleanup of orphaned containers
/// - Stop/remove operations
/// - Hub socket mode (detached with async wait)
pub struct ContainerManager {
    docker: Docker,
}

impl ContainerManager {
    /// Create a new container manager.
    pub fn new() -> Result<Self> {
        let docker =
            Docker::connect_with_local_defaults().map_err(DockerError::Connection)?;

        Ok(Self { docker })
    }

    /// Create a new container manager with a custom Docker connection.
    pub fn with_docker(docker: Docker) -> Self {
        Self { docker }
    }

    /// Spawn a container and return its ID.
    ///
    /// The container is created and started but not waited on.
    /// Used for hub socket mode where we need async wait.
    pub async fn spawn(&self, config: &ContainerConfig) -> Result<String> {
        let name = config.container_name();

        info!(
            container_name = %name,
            image = %config.image,
            session_id = %config.session_id,
            "Creating container (detached)"
        );

        let cmd = if config.hub_socket.is_some() {
            config.build_hub_command()
        } else {
            config.build_attached_command()
        };

        let container_config = ContainerCreateBody {
            image: Some(config.image.clone()),
            cmd: Some(cmd),
            env: Some(config.build_env()),
            working_dir: Some("/workspace".to_string()),
            host_config: Some(HostConfig {
                mounts: Some(config.build_mounts()),
                auto_remove: Some(false),
                ..Default::default()
            }),
            ..Default::default()
        };

        let options = CreateContainerOptions {
            name: Some(name.clone()),
            ..Default::default()
        };

        let response = self
            .docker
            .create_container(Some(options), container_config)
            .await
            .map_err(DockerError::Create)?;

        debug!(container_id = %response.id, "Container created");

        self.docker
            .start_container(&response.id, None::<StartContainerOptions>)
            .await
            .map_err(DockerError::Start)?;

        info!(container_id = %response.id, "Container started");

        Ok(response.id)
    }

    /// Wait for a container to exit and return the exit code.
    pub async fn wait(&self, container_id: &str) -> Result<i64> {
        debug!(container_id = %container_id, "Waiting for container");

        let options = WaitContainerOptions {
            condition: "not-running".to_string(),
        };

        let mut stream = self.docker.wait_container(container_id, Some(options));

        if let Some(result) = stream.next().await {
            match result {
                Ok(response) => {
                    let exit_code = response.status_code;
                    info!(
                        container_id = %container_id,
                        exit_code = %exit_code,
                        "Container exited"
                    );
                    Ok(exit_code)
                }
                Err(e) => Err(DockerError::Wait(e)),
            }
        } else {
            Ok(0)
        }
    }

    /// Stop a running container.
    pub async fn stop(&self, container_id: &str, timeout_secs: Option<i64>) -> Result<()> {
        info!(container_id = %container_id, "Stopping container");

        let options = StopContainerOptions {
            t: Some(timeout_secs.unwrap_or(10) as i32),
            ..Default::default()
        };

        self.docker
            .stop_container(container_id, Some(options))
            .await
            .map_err(DockerError::Stop)?;

        Ok(())
    }

    /// Remove a container.
    pub async fn remove(&self, container_id: &str, force: bool) -> Result<()> {
        debug!(container_id = %container_id, force = %force, "Removing container");

        let options = RemoveContainerOptions {
            force,
            v: true,
            ..Default::default()
        };

        self.docker
            .remove_container(container_id, Some(options))
            .await
            .map_err(DockerError::Remove)?;

        Ok(())
    }

    /// Get container logs.
    pub async fn logs(&self, container_id: &str) -> Result<String> {
        let options = LogsOptions {
            stdout: true,
            stderr: true,
            follow: false,
            ..Default::default()
        };

        let mut stream = self.docker.logs(container_id, Some(options));
        let mut output = String::new();

        while let Some(result) = stream.next().await {
            match result {
                Ok(log) => match log {
                    LogOutput::StdOut { message } | LogOutput::StdErr { message } => {
                        output.push_str(&String::from_utf8_lossy(&message));
                    }
                    _ => {}
                },
                Err(e) => {
                    warn!(error = %e, "Error reading container logs");
                    break;
                }
            }
        }

        Ok(output)
    }

    /// Run a container to completion and return exit code.
    pub async fn run(&self, config: &ContainerConfig) -> Result<i64> {
        let container_id = self.spawn(config).await?;
        let exit_code = self.wait(&container_id).await?;

        if let Err(e) = self.remove(&container_id, false).await {
            warn!(
                container_id = %container_id,
                error = %e,
                "Failed to remove container"
            );
        }

        Ok(exit_code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_config() -> ContainerConfig {
        ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            hub_socket: None,
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123-def-456".to_string(),
            claude_args: vec!["-p".to_string(), "test prompt".to_string()],
            timeout_secs: 300,
            env_vars: HashMap::new(),
        }
    }

    #[test]
    fn test_attached_command() {
        let config = test_config();
        let cmd = config.build_attached_command();

        assert!(cmd.contains(&"wrap".to_string()));
        assert!(cmd.contains(&"--stdout".to_string()));
        assert!(cmd.contains(&"--timeout".to_string()));
        assert!(cmd.contains(&"300".to_string()));
        assert!(cmd.contains(&"-p".to_string()));
        assert!(cmd.contains(&"test prompt".to_string()));
    }

    #[test]
    fn test_hub_command() {
        let mut config = test_config();
        config.hub_socket = Some(PathBuf::from("/tmp/hub.sock"));
        let cmd = config.build_hub_command();

        assert!(cmd.contains(&"wrap".to_string()));
        assert!(cmd.contains(&"--hub-socket".to_string()));
        assert!(cmd.contains(&"/tmp/mantle.sock".to_string()));
        assert!(!cmd.contains(&"--stdout".to_string()));
    }

    #[test]
    fn test_container_name() {
        let config = test_config();
        assert_eq!(config.container_name(), "mantle-abc-123-");
    }

    #[test]
    fn test_container_name_short_session_id() {
        let mut config = test_config();
        config.session_id = "abc".to_string();
        assert_eq!(config.container_name(), "mantle-abc");
    }

    #[test]
    fn test_build_mounts_without_hub() {
        let config = test_config();
        let mounts = config.build_mounts();

        assert_eq!(mounts.len(), 2);

        let workspace = mounts
            .iter()
            .find(|m| m.target == Some("/workspace".to_string()));
        assert!(workspace.is_some());
        assert_eq!(workspace.unwrap().read_only, Some(false));

        let claude = mounts
            .iter()
            .find(|m| m.target == Some("/root/.claude".to_string()));
        assert!(claude.is_some());
        assert_eq!(claude.unwrap().read_only, Some(true));
    }

    #[test]
    fn test_build_mounts_with_hub() {
        let mut config = test_config();
        config.hub_socket = Some(PathBuf::from("/tmp/hub.sock"));
        let mounts = config.build_mounts();

        assert_eq!(mounts.len(), 3);

        let hub = mounts
            .iter()
            .find(|m| m.target == Some("/tmp/mantle.sock".to_string()));
        assert!(hub.is_some());
    }

    #[test]
    fn test_build_env() {
        let mut config = test_config();
        config.env_vars.insert("FOO".to_string(), "bar".to_string());

        let env = config.build_env();
        assert!(env.contains(&"FOO=bar".to_string()));
    }

    #[test]
    fn test_command_without_timeout() {
        let mut config = test_config();
        config.timeout_secs = 0;

        let cmd = config.build_attached_command();
        assert!(!cmd.contains(&"--timeout".to_string()));
    }

    #[test]
    fn test_with_image_builder() {
        let config = test_config().with_image("custom:v1.0");
        assert_eq!(config.image, "custom:v1.0");
    }

    #[test]
    fn test_with_timeout_builder() {
        let config = test_config().with_timeout(600);
        assert_eq!(config.timeout_secs, 600);
    }

    #[test]
    fn test_with_env_builder() {
        let config = test_config()
            .with_env("MY_VAR", "my_value")
            .with_env("ANOTHER", "value2");

        assert_eq!(config.env_vars.get("MY_VAR"), Some(&"my_value".to_string()));
        assert_eq!(config.env_vars.get("ANOTHER"), Some(&"value2".to_string()));
    }

    #[test]
    fn test_session_tag_in_command() {
        let config = test_config();
        let cmd = config.build_attached_command();

        let tag_idx = cmd.iter().position(|s| s == "--session-tag");
        assert!(tag_idx.is_some());
        assert_eq!(cmd[tag_idx.unwrap() + 1], "abc-123-def-456");
    }
}
