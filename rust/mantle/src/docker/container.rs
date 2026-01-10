//! Docker container lifecycle management via bollard.
//!
//! Handles spawning, monitoring, and cleaning up containers for
//! Claude Code sessions.

use bollard::container::LogOutput;
use bollard::models::{ContainerCreateBody, HostConfig, Mount, MountTypeEnum};
use bollard::query_parameters::{
    CreateContainerOptions, LogsOptions, RemoveContainerOptions, StartContainerOptions,
    StopContainerOptions, WaitContainerOptions,
};
use bollard::Docker;
use futures_util::StreamExt;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
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

    #[error("FIFO directory error: {0}")]
    FifoDir(#[source] std::io::Error),

    #[error("Failed to read FIFO: {0}")]
    FifoRead(String),

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
    /// Path to the FIFO directory for result communication
    pub fifo_dir: PathBuf,
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
    /// Create a new container config with defaults.
    pub fn new(
        session_id: String,
        worktree_path: PathBuf,
        fifo_dir: PathBuf,
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
            fifo_dir,
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
    fn container_name(&self) -> String {
        format!("mantle-{}", &self.session_id[..8])
    }

    /// Build the command to run inside the container.
    fn build_command(&self) -> Vec<String> {
        let mut cmd = vec![
            "mantle-agent".to_string(),
            "wrap".to_string(),
            "--result-fifo".to_string(),
            "/tmp/mantle/result.fifo".to_string(),
            "--cwd".to_string(),
            "/workspace".to_string(),
            "--session-tag".to_string(),
            self.session_id.clone(),
        ];

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

    /// Build mount configuration.
    fn build_mounts(&self) -> Vec<Mount> {
        vec![
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
            // FIFO directory: read-write for result communication
            Mount {
                target: Some("/tmp/mantle".to_string()),
                source: Some(self.fifo_dir.display().to_string()),
                typ: Some(MountTypeEnum::BIND),
                read_only: Some(false),
                ..Default::default()
            },
        ]
    }
}

/// Manages Docker container lifecycle for mantle sessions.
pub struct ContainerManager {
    docker: Docker,
}

impl ContainerManager {
    /// Create a new container manager.
    ///
    /// Connects to the Docker daemon using platform defaults:
    /// - Unix: /var/run/docker.sock
    /// - macOS: /var/run/docker.sock or Docker Desktop socket
    /// - Windows: Named pipe
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
    pub async fn spawn(&self, config: &ContainerConfig) -> Result<String> {
        let name = config.container_name();

        info!(
            container_name = %name,
            image = %config.image,
            session_id = %config.session_id,
            "Creating container"
        );

        let container_config = ContainerCreateBody {
            image: Some(config.image.clone()),
            cmd: Some(config.build_command()),
            env: Some(config.build_env()),
            working_dir: Some("/workspace".to_string()),
            host_config: Some(HostConfig {
                mounts: Some(config.build_mounts()),
                auto_remove: Some(false), // We'll remove manually after reading result
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

        debug!(
            container_id = %response.id,
            "Container created"
        );

        // Start the container
        self.docker
            .start_container(&response.id, None::<StartContainerOptions>)
            .await
            .map_err(DockerError::Start)?;

        info!(
            container_id = %response.id,
            "Container started"
        );

        Ok(response.id)
    }

    /// Wait for a container to exit and return the exit code.
    pub async fn wait(&self, container_id: &str) -> Result<i64> {
        debug!(container_id = %container_id, "Waiting for container");

        let options = WaitContainerOptions {
            condition: "not-running".to_string(),
        };

        let mut stream = self.docker.wait_container(container_id, Some(options));

        while let Some(result) = stream.next().await {
            match result {
                Ok(response) => {
                    let exit_code = response.status_code;
                    info!(
                        container_id = %container_id,
                        exit_code = %exit_code,
                        "Container exited"
                    );
                    return Ok(exit_code);
                }
                Err(e) => {
                    return Err(DockerError::Wait(e));
                }
            }
        }

        // Stream ended without result - container may have been removed
        Ok(0)
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
            v: true, // Remove volumes
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
    ///
    /// This is a convenience method that spawns, waits, and cleans up.
    pub async fn run(&self, config: &ContainerConfig) -> Result<i64> {
        let container_id = self.spawn(config).await?;

        let exit_code = self.wait(&container_id).await?;

        // Clean up container
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

/// Create the FIFO directory and result FIFO for container communication.
pub fn create_fifo_dir(session_id: &str) -> Result<PathBuf> {
    let fifo_dir = std::env::temp_dir().join(format!("mantle-{}", &session_id[..8]));

    std::fs::create_dir_all(&fifo_dir).map_err(DockerError::FifoDir)?;

    // Create the result FIFO
    let fifo_path = fifo_dir.join("result.fifo");
    if !fifo_path.exists() {
        nix::unistd::mkfifo(&fifo_path, nix::sys::stat::Mode::S_IRWXU).map_err(|e| {
            DockerError::FifoDir(std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))
        })?;
    }

    Ok(fifo_dir)
}

/// Clean up the FIFO directory after container completion.
pub fn cleanup_fifo_dir(fifo_dir: &Path) -> Result<()> {
    if fifo_dir.exists() {
        std::fs::remove_dir_all(fifo_dir).map_err(DockerError::FifoDir)?;
    }
    Ok(())
}

/// A FIFO reader that handles the blocking semantics correctly.
///
/// Opening a FIFO for reading blocks until a writer opens it.
/// This struct spawns a background thread to handle that blocking,
/// allowing the container to be spawned concurrently.
///
/// # Usage
/// ```ignore
/// let reader = FifoReader::spawn(fifo_path)?;
/// // Spawn container here - the reader is already waiting
/// container.wait().await?;
/// let content = reader.join()?;
/// ```
pub struct FifoReader {
    handle: std::thread::JoinHandle<std::result::Result<String, std::io::Error>>,
}

impl FifoReader {
    /// Spawn a background thread that reads from the FIFO.
    ///
    /// This starts the reader immediately, which will block until
    /// a writer (the container) opens the FIFO.
    pub fn spawn(fifo_path: PathBuf) -> Self {
        let handle = std::thread::spawn(move || std::fs::read_to_string(&fifo_path));
        Self { handle }
    }

    /// Wait for the reader to complete and return the content.
    ///
    /// Call this after the container has exited.
    pub fn join(self) -> Result<String> {
        self.handle
            .join()
            .map_err(|_| DockerError::FifoRead("Reader thread panicked".to_string()))?
            .map_err(|e| DockerError::FifoRead(e.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_container_config_command() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123-def-456".to_string(),
            claude_args: vec!["-p".to_string(), "test prompt".to_string()],
            timeout_secs: 300,
            env_vars: HashMap::new(),
        };

        let cmd = config.build_command();
        assert!(cmd.contains(&"mantle-agent".to_string()));
        assert!(cmd.contains(&"wrap".to_string()));
        assert!(cmd.contains(&"--timeout".to_string()));
        assert!(cmd.contains(&"300".to_string()));
        assert!(cmd.contains(&"-p".to_string()));
        assert!(cmd.contains(&"test prompt".to_string()));
    }

    #[test]
    fn test_container_name() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc12345-def-456".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        };

        assert_eq!(config.container_name(), "mantle-abc12345");
    }

    #[test]
    fn test_build_mounts() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        };

        let mounts = config.build_mounts();
        assert_eq!(mounts.len(), 3);

        // Check worktree mount is read-write
        let workspace = mounts
            .iter()
            .find(|m| m.target == Some("/workspace".to_string()));
        assert!(workspace.is_some());
        assert_eq!(workspace.unwrap().read_only, Some(false));

        // Check claude home is read-only
        let claude = mounts
            .iter()
            .find(|m| m.target == Some("/root/.claude".to_string()));
        assert!(claude.is_some());
        assert_eq!(claude.unwrap().read_only, Some(true));
    }

    #[test]
    fn test_build_env() {
        let mut config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        };

        config.env_vars.insert("FOO".to_string(), "bar".to_string());

        let env = config.build_env();
        assert!(env.contains(&"FOO=bar".to_string()));
    }

    #[test]
    fn test_command_without_timeout() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123-def-456".to_string(),
            claude_args: vec!["-p".to_string(), "hello".to_string()],
            timeout_secs: 0, // No timeout
            env_vars: HashMap::new(),
        };

        let cmd = config.build_command();
        // Should NOT contain --timeout when timeout_secs is 0
        assert!(!cmd.contains(&"--timeout".to_string()));
        // Should still have the basic structure
        assert!(cmd.contains(&"mantle-agent".to_string()));
        assert!(cmd.contains(&"wrap".to_string()));
        assert!(cmd.contains(&"--".to_string()));
    }

    #[test]
    fn test_fifo_mount_path() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        };

        let mounts = config.build_mounts();
        let fifo_mount = mounts
            .iter()
            .find(|m| m.target == Some("/tmp/mantle".to_string()));

        assert!(fifo_mount.is_some());
        let mount = fifo_mount.unwrap();
        assert_eq!(mount.source, Some("/tmp/fifo".to_string()));
        assert_eq!(mount.read_only, Some(false)); // Must be writable
    }

    #[test]
    fn test_create_fifo_dir() {
        let session_id = "test-session-12345678";
        let fifo_dir = create_fifo_dir(session_id).unwrap();

        // Verify directory was created
        assert!(fifo_dir.exists());
        assert!(fifo_dir.is_dir());

        // Verify FIFO was created
        let fifo_path = fifo_dir.join("result.fifo");
        assert!(fifo_path.exists());

        // Verify directory name uses session ID prefix
        let dir_name = fifo_dir.file_name().unwrap().to_str().unwrap();
        assert!(dir_name.starts_with("mantle-"));

        // Cleanup
        cleanup_fifo_dir(&fifo_dir).unwrap();
        assert!(!fifo_dir.exists());
    }

    #[test]
    fn test_cleanup_nonexistent_fifo_dir() {
        // Cleaning up a non-existent directory should succeed
        let result = cleanup_fifo_dir(Path::new("/tmp/nonexistent-mantle-test-dir"));
        assert!(result.is_ok());
    }

    #[test]
    fn test_with_image_builder() {
        let config = ContainerConfig {
            image: "default:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        }
        .with_image("custom:v1.0");

        assert_eq!(config.image, "custom:v1.0");
    }

    #[test]
    fn test_with_timeout_builder() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        }
        .with_timeout(600);

        assert_eq!(config.timeout_secs, 600);
    }

    #[test]
    fn test_with_env_builder() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        }
        .with_env("MY_VAR", "my_value")
        .with_env("ANOTHER", "value2");

        assert_eq!(config.env_vars.get("MY_VAR"), Some(&"my_value".to_string()));
        assert_eq!(config.env_vars.get("ANOTHER"), Some(&"value2".to_string()));
    }

    #[test]
    fn test_session_tag_in_command() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "my-unique-session-id".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        };

        let cmd = config.build_command();

        // Find --session-tag and verify the next arg is the session ID
        let tag_idx = cmd.iter().position(|s| s == "--session-tag");
        assert!(tag_idx.is_some());
        assert_eq!(cmd[tag_idx.unwrap() + 1], "my-unique-session-id");
    }

    #[test]
    fn test_result_fifo_path_in_command() {
        let config = ContainerConfig {
            image: "test:latest".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            fifo_dir: PathBuf::from("/tmp/fifo"),
            claude_home: PathBuf::from("/home/user/.claude"),
            session_id: "abc-123".to_string(),
            claude_args: vec![],
            timeout_secs: 0,
            env_vars: HashMap::new(),
        };

        let cmd = config.build_command();

        // Verify the container-internal FIFO path is used
        let fifo_idx = cmd.iter().position(|s| s == "--result-fifo");
        assert!(fifo_idx.is_some());
        assert_eq!(cmd[fifo_idx.unwrap() + 1], "/tmp/mantle/result.fifo");
    }
}
