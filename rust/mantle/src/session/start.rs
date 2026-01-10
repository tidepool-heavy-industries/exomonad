//! Session start command implementation.
//!
//! Creates a new session with:
//! - Unique session ID and branch name
//! - Git worktree for isolation
//! - Claude Code execution (local or Docker)
//! - State persistence

use std::path::Path;
use std::time::Instant;
use tracing::{debug, info, warn};

use super::state::StateManager;
use super::types::{generate_branch_name, generate_session_id, SessionMetadata, SessionOutput};
use super::worktree::WorktreeManager;
use crate::docker::{cleanup_fifo_dir, create_fifo_dir, ContainerConfig, ContainerManager, FifoReader};

/// RAII guard that cleans up FIFO directory on drop.
/// Ensures cleanup happens even on early returns or panics.
struct FifoGuard {
    path: std::path::PathBuf,
    cleaned: bool,
}

impl FifoGuard {
    fn new(path: std::path::PathBuf) -> Self {
        Self { path, cleaned: false }
    }

    /// Mark as cleaned (call this if you want to suppress automatic cleanup).
    #[allow(dead_code)]
    fn defuse(&mut self) {
        self.cleaned = true;
    }
}

impl Drop for FifoGuard {
    fn drop(&mut self) {
        if !self.cleaned {
            if let Err(e) = cleanup_fifo_dir(&self.path) {
                // Can't use tracing in Drop easily, just ignore errors
                eprintln!("Warning: Failed to cleanup FIFO directory {}: {}", self.path.display(), e);
            }
        }
    }
}

/// Error types for session start operations.
#[derive(Debug, thiserror::Error)]
pub enum StartError {
    #[error("State error: {0}")]
    State(#[from] super::state::StateError),

    #[error("Worktree error: {0}")]
    Worktree(#[from] super::worktree::WorktreeError),

    #[error("Docker error: {0}")]
    Docker(#[from] crate::docker::DockerError),

    #[error("Session execution error: {0}")]
    Execution(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
}

pub type Result<T> = std::result::Result<T, StartError>;

/// Configuration for starting a session.
#[derive(Debug, Clone)]
pub struct StartConfig {
    /// Semantic slug (e.g., "implement/user-auth")
    pub slug: String,
    /// Prompt for Claude Code
    pub prompt: String,
    /// Model to use (haiku, sonnet, opus)
    pub model: String,
    /// Timeout in seconds (0 = no timeout)
    pub timeout_secs: u64,
    /// Use Docker container
    pub docker: bool,
    /// Optional base branch (defaults to HEAD)
    pub base_branch: Option<String>,
}

/// Start a new session.
///
/// This function:
/// 1. Generates unique session ID and branch name
/// 2. Creates git worktree for isolation
/// 3. Records session in state file
/// 4. Executes Claude Code (or returns metadata for Docker execution)
///
/// # Arguments
/// * `repo_root` - Path to the git repository root
/// * `config` - Session configuration
///
/// # Returns
/// Session output with results
pub fn start_session(repo_root: &Path, config: &StartConfig) -> Result<SessionOutput> {
    let start_time = Instant::now();

    // Initialize managers
    let state_manager = StateManager::new(repo_root)?;
    let worktree_manager =
        WorktreeManager::new(repo_root, &state_manager.worktrees_dir())?;

    // Generate unique identifiers
    let session_id = generate_session_id();
    let branch = generate_branch_name(&config.slug);

    info!(
        session_id = %session_id,
        slug = %config.slug,
        branch = %branch,
        model = %config.model,
        "Starting new session"
    );

    // Create worktree
    let worktree_path = worktree_manager.create(&branch, config.base_branch.as_deref())?;

    debug!(
        worktree = %worktree_path.display(),
        "Created worktree"
    );

    // Create session metadata
    let session = SessionMetadata::new(
        session_id.clone(),
        config.slug.clone(),
        branch.clone(),
        worktree_path.clone(),
        config.model.clone(),
    );

    // Save session to state file
    state_manager.insert_session(session)?;

    // Execute Claude Code via Docker or locally
    if config.docker {
        let result = execute_docker(&session_id, &branch, &worktree_path, config, &state_manager)?;

        let duration = start_time.elapsed().as_secs_f64();

        // Update session state based on result
        state_manager.update_session(&session_id, |s| {
            s.mark_completed(
                result.exit_code,
                result.total_cost_usd,
                result.num_turns,
                result.cc_session_id.clone(),
            );
        })?;

        return Ok(SessionOutput {
            duration_secs: duration,
            ..result
        });
    }

    // Direct execution mode - run mantle-agent wrap
    let result = execute_local(&session_id, &branch, &worktree_path, config, &state_manager)?;

    let duration = start_time.elapsed().as_secs_f64();

    // Update session state based on result
    state_manager.update_session(&session_id, |s| {
        s.mark_completed(
            result.exit_code,
            result.total_cost_usd,
            result.num_turns,
            result.cc_session_id.clone(),
        );
    })?;

    Ok(SessionOutput {
        duration_secs: duration,
        ..result
    })
}

/// Execute Claude Code locally (not in Docker).
fn execute_local(
    session_id: &str,
    branch: &str,
    worktree_path: &Path,
    config: &StartConfig,
    state_manager: &StateManager,
) -> Result<SessionOutput> {
    use std::process::{Command, Stdio};
    use mantle_shared::{RunResult, ResultFifo};

    // Mark session as running
    state_manager.update_session(session_id, |s| {
        s.mark_running(None);
    })?;

    // Create FIFO for result communication
    let result_fifo = ResultFifo::new()
        .map_err(|e| StartError::Execution(format!("Failed to create result FIFO: {}", e)))?;

    // Build claude args
    let claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--verbose".to_string(),
        "--model".to_string(),
        config.model.clone(),
        "-p".to_string(),
        config.prompt.clone(),
    ];

    // Build mantle-agent command
    let mut cmd = Command::new("mantle-agent");
    cmd.arg("wrap")
        .arg("--result-fifo")
        .arg(result_fifo.path())
        .arg("--cwd")
        .arg(worktree_path)
        .arg("--session-tag")
        .arg(session_id);

    if config.timeout_secs > 0 {
        cmd.arg("--timeout").arg(config.timeout_secs.to_string());
    }

    cmd.arg("--");
    for arg in &claude_args {
        cmd.arg(arg);
    }

    // Run in background and read from FIFO
    cmd.stdout(Stdio::null())
        .stderr(Stdio::null());

    debug!(cmd = ?cmd, "Spawning mantle-agent");

    let _child = cmd.spawn()
        .map_err(|e| StartError::Execution(format!("Failed to spawn mantle-agent: {}", e)))?;

    // Read result from FIFO (blocks until agent writes result)
    let timeout = if config.timeout_secs > 0 {
        std::time::Duration::from_secs(config.timeout_secs)
    } else {
        std::time::Duration::ZERO
    };

    let run_result: RunResult = result_fifo
        .read_with_timeout(timeout)
        .map_err(|e| StartError::Execution(format!("Failed to read result: {}", e)))?;

    // Convert RunResult to SessionOutput
    Ok(SessionOutput {
        session_id: session_id.to_string(),
        branch: branch.to_string(),
        worktree: worktree_path.to_path_buf(),
        exit_code: run_result.exit_code,
        is_error: run_result.is_error,
        result_text: run_result.result,
        structured_output: run_result.structured_output,
        total_cost_usd: run_result.total_cost_usd,
        num_turns: run_result.num_turns,
        interrupts: run_result.interrupts,
        duration_secs: 0.0, // Will be filled in by caller
        error: None,
        model_usage: run_result.model_usage,
        cc_session_id: Some(run_result.session_id),
    })
}

/// Execute Claude Code in a Docker container.
fn execute_docker(
    session_id: &str,
    branch: &str,
    worktree_path: &Path,
    config: &StartConfig,
    state_manager: &StateManager,
) -> Result<SessionOutput> {
    use mantle_shared::RunResult;
    use std::collections::HashMap;

    info!(session_id = %session_id, "Starting Docker execution");

    // Create FIFO directory for result communication
    // Use guard to ensure cleanup even on early returns
    let fifo_dir = create_fifo_dir(session_id)?;
    let _fifo_guard = FifoGuard::new(fifo_dir.clone());
    let fifo_path = fifo_dir.join("result.fifo");

    debug!(fifo_dir = %fifo_dir.display(), "Created FIFO directory");

    // Build Claude args
    let claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--verbose".to_string(),
        "--model".to_string(),
        config.model.clone(),
        "-p".to_string(),
        config.prompt.clone(),
    ];

    // Create container configuration
    let container_config = ContainerConfig::new(
        session_id.to_string(),
        worktree_path.to_path_buf(),
        fifo_dir.clone(),
        claude_args,
    )?
    .with_timeout(config.timeout_secs);

    // Mark session as running
    state_manager.update_session(session_id, |s| {
        s.mark_running(None);
    })?;

    // Create tokio runtime for async Docker operations
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| StartError::Execution(format!("Failed to create tokio runtime: {}", e)))?;

    // IMPORTANT: Spawn FIFO reader BEFORE container.
    // A FIFO blocks on open until both reader and writer are present.
    // If we wait for container to exit before opening the FIFO, we deadlock:
    // - Container blocks on write (waiting for reader)
    // - We block on container wait (waiting for container)
    // By spawning the reader first, we ensure the read side is ready when
    // the container opens the write side.
    let fifo_reader = FifoReader::spawn(fifo_path);

    // Spawn container and wait for completion
    // Uses a cleanup guard pattern to ensure container is always removed
    let container_result = runtime.block_on(async {
        let manager = ContainerManager::new()?;
        let container_id = manager.spawn(&container_config).await?;

        info!(container_id = %container_id, "Container spawned, waiting for completion");

        // Wait for container to exit - capture result before cleanup
        let wait_result = manager.wait(&container_id).await;

        // Always cleanup container, regardless of wait result
        // Get logs first if there was an error
        if wait_result.as_ref().map(|c| *c != 0).unwrap_or(true) {
            match manager.logs(&container_id).await {
                Ok(logs) => {
                    let code = wait_result.as_ref().map(|c| *c).unwrap_or(-1);
                    warn!(exit_code = %code, logs = %logs, "Container exited with non-zero code or error");
                }
                Err(e) => {
                    warn!(error = %e, "Failed to get container logs");
                }
            }
        }

        // Always remove container (force=true to handle stuck containers)
        if let Err(e) = manager.remove(&container_id, true).await {
            warn!(container_id = %container_id, error = %e, "Failed to remove container");
        }

        // Now propagate any wait error
        wait_result
    })?;

    // Join the FIFO reader thread - it should have completed when container wrote and exited
    let content = fifo_reader.join()
        .map_err(|e| StartError::Execution(format!("Failed to read result FIFO: {}", e)))?;

    // Parse the result from FIFO content
    let run_result: RunResult = if content.is_empty() {
        // Container exited without writing result - create error result
        warn!("Container exited without writing result to FIFO");
        RunResult {
            exit_code: container_result as i32,
            is_error: true,
            result: None,
            structured_output: None,
            session_id: String::new(),
            session_tag: Some(session_id.to_string()),
            total_cost_usd: 0.0,
            num_turns: 0,
            events: vec![],
            permission_denials: vec![],
            model_usage: HashMap::new(),
            interrupts: vec![],
        }
    } else {
        serde_json::from_str(&content)?
    };

    // FIFO directory cleanup handled by _fifo_guard drop

    // Convert RunResult to SessionOutput
    Ok(SessionOutput {
        session_id: session_id.to_string(),
        branch: branch.to_string(),
        worktree: worktree_path.to_path_buf(),
        exit_code: run_result.exit_code,
        is_error: run_result.is_error,
        result_text: run_result.result,
        structured_output: run_result.structured_output,
        total_cost_usd: run_result.total_cost_usd,
        num_turns: run_result.num_turns,
        interrupts: run_result.interrupts,
        duration_secs: 0.0, // Will be filled in by caller
        error: None,
        model_usage: run_result.model_usage,
        cc_session_id: Some(run_result.session_id),
    })
}

/// Prepare session for execution (creates worktree and metadata, doesn't execute).
///
/// Use this when you need to set up a session but execute it separately
/// (e.g., via Docker).
///
/// # Returns
/// Tuple of (session_id, branch, worktree_path)
pub fn prepare_session(
    repo_root: &Path,
    config: &StartConfig,
) -> Result<(String, String, std::path::PathBuf)> {
    // Initialize managers
    let state_manager = StateManager::new(repo_root)?;
    let worktree_manager =
        WorktreeManager::new(repo_root, &state_manager.worktrees_dir())?;

    // Generate unique identifiers
    let session_id = generate_session_id();
    let branch = generate_branch_name(&config.slug);

    info!(
        session_id = %session_id,
        slug = %config.slug,
        branch = %branch,
        "Preparing session"
    );

    // Create worktree
    let worktree_path = worktree_manager.create(&branch, config.base_branch.as_deref())?;

    // Create session metadata
    let session = SessionMetadata::new(
        session_id.clone(),
        config.slug.clone(),
        branch.clone(),
        worktree_path.clone(),
        config.model.clone(),
    );

    // Save session to state file
    state_manager.insert_session(session)?;

    Ok((session_id, branch, worktree_path))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;
    use tempfile::TempDir;

    fn setup_git_repo() -> TempDir {
        let temp_dir = TempDir::new().unwrap();

        Command::new("git")
            .args(["init"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        Command::new("git")
            .args(["config", "user.email", "test@test.com"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        Command::new("git")
            .args(["config", "user.name", "Test"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        std::fs::write(temp_dir.path().join("README.md"), "# Test").unwrap();

        Command::new("git")
            .args(["add", "."])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        Command::new("git")
            .args(["commit", "-m", "Initial commit"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        temp_dir
    }

    #[test]
    fn test_prepare_session() {
        let temp_dir = setup_git_repo();

        let config = StartConfig {
            slug: "test/prepare".to_string(),
            prompt: "Test prompt".to_string(),
            model: "sonnet".to_string(),
            timeout_secs: 0,
            docker: false,
            base_branch: None,
        };

        let (session_id, branch, worktree_path) = prepare_session(temp_dir.path(), &config).unwrap();

        // Verify session ID is a UUID
        assert_eq!(session_id.len(), 36); // UUID format

        // Verify branch name follows pattern
        assert!(branch.starts_with("test/prepare-"));
        assert_eq!(branch.len(), "test/prepare-".len() + 6);

        // Verify worktree was created
        assert!(worktree_path.exists());

        // Verify session was recorded
        let state_manager = StateManager::new(temp_dir.path()).unwrap();
        let session = state_manager.get_session(&session_id).unwrap();
        assert_eq!(session.slug, "test/prepare");
        assert_eq!(session.model, "sonnet");
    }

    #[test]
    fn test_prepare_session_with_nested_slug() {
        let temp_dir = setup_git_repo();

        let config = StartConfig {
            slug: "epic/feature/subtask".to_string(),
            prompt: "Test prompt".to_string(),
            model: "haiku".to_string(),
            timeout_secs: 300,
            docker: false,
            base_branch: None,
        };

        let (_, branch, worktree_path) = prepare_session(temp_dir.path(), &config).unwrap();

        // Verify branch name preserves nested structure
        assert!(branch.starts_with("epic/feature/subtask-"));

        // Verify worktree directory uses dashes
        let dir_name = worktree_path.file_name().unwrap().to_str().unwrap();
        assert!(dir_name.starts_with("epic-feature-subtask-"));
    }

    #[test]
    fn test_prepare_session_creates_pending_state() {
        let temp_dir = setup_git_repo();

        let config = StartConfig {
            slug: "test/pending".to_string(),
            prompt: "Test prompt".to_string(),
            model: "sonnet".to_string(),
            timeout_secs: 0,
            docker: false,
            base_branch: None,
        };

        let (session_id, _, _) = prepare_session(temp_dir.path(), &config).unwrap();

        // Session should start in Pending state
        let state_manager = StateManager::new(temp_dir.path()).unwrap();
        let session = state_manager.get_session(&session_id).unwrap();
        assert!(matches!(session.state, super::super::types::SessionState::Pending));
    }

    #[test]
    fn test_prepare_multiple_sessions() {
        let temp_dir = setup_git_repo();

        // Create first session
        let config1 = StartConfig {
            slug: "feature/auth".to_string(),
            prompt: "Add authentication".to_string(),
            model: "sonnet".to_string(),
            timeout_secs: 0,
            docker: false,
            base_branch: None,
        };

        let (id1, branch1, _) = prepare_session(temp_dir.path(), &config1).unwrap();

        // Create second session
        let config2 = StartConfig {
            slug: "feature/api".to_string(),
            prompt: "Add API endpoints".to_string(),
            model: "haiku".to_string(),
            timeout_secs: 0,
            docker: false,
            base_branch: None,
        };

        let (id2, branch2, _) = prepare_session(temp_dir.path(), &config2).unwrap();

        // Verify both sessions exist
        let state_manager = StateManager::new(temp_dir.path()).unwrap();
        assert!(state_manager.get_session(&id1).is_ok());
        assert!(state_manager.get_session(&id2).is_ok());

        // Verify unique branches
        assert_ne!(branch1, branch2);
        assert!(branch1.starts_with("feature/auth-"));
        assert!(branch2.starts_with("feature/api-"));
    }

    #[test]
    fn test_prepare_session_with_base_branch() {
        let temp_dir = setup_git_repo();

        // Create a feature branch to use as base
        Command::new("git")
            .args(["checkout", "-b", "develop"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        std::fs::write(temp_dir.path().join("feature.txt"), "new feature").unwrap();

        Command::new("git")
            .args(["add", "."])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        Command::new("git")
            .args(["commit", "-m", "Add feature"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        // Go back to main
        Command::new("git")
            .args(["checkout", "master"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        // Create session based on develop branch
        let config = StartConfig {
            slug: "hotfix/urgent".to_string(),
            prompt: "Fix urgent bug".to_string(),
            model: "sonnet".to_string(),
            timeout_secs: 0,
            docker: false,
            base_branch: Some("develop".to_string()),
        };

        let (_, _, worktree_path) = prepare_session(temp_dir.path(), &config).unwrap();

        // Verify worktree has the file from develop branch
        assert!(worktree_path.join("feature.txt").exists());
    }

    #[test]
    fn test_start_config_default_values() {
        let config = StartConfig {
            slug: "test".to_string(),
            prompt: "test".to_string(),
            model: "sonnet".to_string(),
            timeout_secs: 0,
            docker: false,
            base_branch: None,
        };

        // Verify zero timeout means no timeout
        assert_eq!(config.timeout_secs, 0);
        // Verify docker is opt-in
        assert!(!config.docker);
        // Verify no base branch means HEAD
        assert!(config.base_branch.is_none());
    }

    #[test]
    fn test_session_output_error_constructor() {
        let output = SessionOutput::error(
            "test-session-123".to_string(),
            "test/branch-abc123".to_string(),
            std::path::PathBuf::from("/tmp/worktree"),
            "Something went wrong".to_string(),
            1.5,
        );

        assert!(output.is_error);
        assert_eq!(output.exit_code, 1);
        assert_eq!(output.error, Some("Something went wrong".to_string()));
        assert_eq!(output.session_id, "test-session-123");
        assert_eq!(output.duration_secs, 1.5);
    }
}
