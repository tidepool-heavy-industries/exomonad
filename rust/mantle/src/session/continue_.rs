//! Session continue command implementation.
//!
//! Resumes an existing session with a new prompt while preserving
//! Claude Code's conversation context via --resume.

use std::path::Path;
use std::time::Instant;
use tracing::{debug, info, warn};

use super::state::StateManager;
use super::types::{SessionOutput, SessionState};
use crate::docker::{cleanup_fifo_dir, create_fifo_dir, ContainerConfig, ContainerManager, FifoReader};

/// Error types for session continue operations.
#[derive(Debug, thiserror::Error)]
pub enum ContinueError {
    #[error("State error: {0}")]
    State(#[from] super::state::StateError),

    #[error("Session not found: {0}")]
    NotFound(String),

    #[error("Session is in state '{0}', cannot continue")]
    InvalidState(SessionState),

    #[error("Session has no Claude Code session ID (never ran successfully)")]
    NoCcSessionId,

    #[error("Worktree not found: {0}")]
    WorktreeNotFound(String),

    #[error("Worktree is corrupted (missing .git): {0}")]
    WorktreeCorrupted(String),

    #[error("Branch not found in git: {0}")]
    BranchNotFound(String),

    #[error("Execution error: {0}")]
    Execution(String),

    #[error("Docker error: {0}")]
    Docker(#[from] crate::docker::DockerError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
}

pub type Result<T> = std::result::Result<T, ContinueError>;

/// RAII guard that cleans up FIFO directory on drop.
struct FifoGuard {
    path: std::path::PathBuf,
    cleaned: bool,
}

impl FifoGuard {
    fn new(path: std::path::PathBuf) -> Self {
        Self { path, cleaned: false }
    }
}

impl Drop for FifoGuard {
    fn drop(&mut self) {
        if !self.cleaned {
            if let Err(e) = cleanup_fifo_dir(&self.path) {
                eprintln!("Warning: Failed to cleanup FIFO directory {}: {}", self.path.display(), e);
            }
        }
    }
}

/// Configuration for continuing a session.
#[derive(Debug, Clone)]
pub struct ContinueConfig {
    /// Session ID to continue
    pub session_id: String,
    /// New prompt for the continuation
    pub prompt: String,
    /// Timeout in seconds (0 = no timeout)
    pub timeout_secs: u64,
    /// Use Docker container
    pub docker: bool,
}

/// Continue an existing session with a new prompt.
///
/// This function:
/// 1. Loads existing session metadata
/// 2. Validates session is in a continuable state
/// 3. Resumes Claude Code with --resume flag
/// 4. Updates session state on completion
///
/// # Arguments
/// * `repo_root` - Path to the git repository root
/// * `config` - Continue configuration
///
/// # Returns
/// Session output with results
pub fn continue_session(repo_root: &Path, config: &ContinueConfig) -> Result<SessionOutput> {
    let start_time = Instant::now();

    // Load session metadata
    let state_manager = StateManager::new(repo_root)?;
    let session = state_manager.get_session(&config.session_id)?;

    // Validate session can be continued
    match session.state {
        SessionState::Completed | SessionState::Failed => {
            // These states are fine to continue from
        }
        SessionState::Pending => {
            return Err(ContinueError::InvalidState(SessionState::Pending));
        }
        SessionState::Running => {
            return Err(ContinueError::InvalidState(SessionState::Running));
        }
        SessionState::Cancelled => {
            // Allow continuing cancelled sessions
        }
    }

    // Need cc_session_id for --resume
    let cc_session_id = session.cc_session_id.clone().ok_or(ContinueError::NoCcSessionId)?;

    // Validate worktree exists and is intact
    if !session.worktree.exists() {
        return Err(ContinueError::WorktreeNotFound(
            session.worktree.display().to_string(),
        ));
    }

    // Check worktree has .git (file or directory)
    let git_path = session.worktree.join(".git");
    if !git_path.exists() {
        return Err(ContinueError::WorktreeCorrupted(
            session.worktree.display().to_string(),
        ));
    }

    // Verify branch still exists in git
    let branch_check = std::process::Command::new("git")
        .arg("-C")
        .arg(&session.worktree)
        .arg("rev-parse")
        .arg("--verify")
        .arg(&session.branch)
        .output();

    match branch_check {
        Ok(output) if !output.status.success() => {
            return Err(ContinueError::BranchNotFound(session.branch.clone()));
        }
        Err(e) => {
            debug!(error = %e, "Failed to verify branch, continuing anyway");
            // Git not available or other error - continue anyway
        }
        _ => {}
    }

    info!(
        session_id = %config.session_id,
        cc_session_id = %cc_session_id,
        branch = %session.branch,
        "Continuing session"
    );

    if config.docker {
        let result = execute_docker_continue(
            &config.session_id,
            &session.branch,
            &session.worktree,
            &cc_session_id,
            &session.model,
            &config.prompt,
            config.timeout_secs,
            &state_manager,
        )?;

        let duration = start_time.elapsed().as_secs_f64();

        // Update session state
        state_manager.update_session(&config.session_id, |s| {
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

    // Execute with --resume flag (local mode)
    let result = execute_continue(
        &config.session_id,
        &session.branch,
        &session.worktree,
        &cc_session_id,
        &session.model,
        &config.prompt,
        config.timeout_secs,
        &state_manager,
    )?;

    let duration = start_time.elapsed().as_secs_f64();

    // Update session state
    state_manager.update_session(&config.session_id, |s| {
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

fn execute_continue(
    session_id: &str,
    branch: &str,
    worktree_path: &Path,
    cc_session_id: &str,
    model: &str,
    prompt: &str,
    timeout_secs: u64,
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
        .map_err(|e| ContinueError::Execution(format!("Failed to create result FIFO: {}", e)))?;

    // Build claude args with --resume
    let claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--verbose".to_string(),
        "--model".to_string(),
        model.to_string(),
        "--resume".to_string(),
        cc_session_id.to_string(),
        "-p".to_string(),
        prompt.to_string(),
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

    if timeout_secs > 0 {
        cmd.arg("--timeout").arg(timeout_secs.to_string());
    }

    cmd.arg("--");
    for arg in &claude_args {
        cmd.arg(arg);
    }

    cmd.stdout(Stdio::null())
        .stderr(Stdio::null());

    debug!(cmd = ?cmd, "Spawning mantle-agent for continue");

    let _child = cmd.spawn()
        .map_err(|e| ContinueError::Execution(format!("Failed to spawn mantle-agent: {}", e)))?;

    // Read result from FIFO
    let timeout = if timeout_secs > 0 {
        std::time::Duration::from_secs(timeout_secs)
    } else {
        std::time::Duration::ZERO
    };

    let run_result: RunResult = result_fifo
        .read_with_timeout(timeout)
        .map_err(|e| ContinueError::Execution(format!("Failed to read result: {}", e)))?;

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
        duration_secs: 0.0,
        error: None,
        model_usage: run_result.model_usage,
        cc_session_id: Some(run_result.session_id),
    })
}

/// Execute session continue in a Docker container.
fn execute_docker_continue(
    session_id: &str,
    branch: &str,
    worktree_path: &Path,
    cc_session_id: &str,
    model: &str,
    prompt: &str,
    timeout_secs: u64,
    state_manager: &StateManager,
) -> Result<SessionOutput> {
    use mantle_shared::RunResult;
    use std::collections::HashMap;

    info!(session_id = %session_id, "Starting Docker continue execution");

    // Create FIFO directory for result communication
    let fifo_dir = create_fifo_dir(session_id)?;
    let _fifo_guard = FifoGuard::new(fifo_dir.clone());
    let fifo_path = fifo_dir.join("result.fifo");

    debug!(fifo_dir = %fifo_dir.display(), "Created FIFO directory");

    // Build Claude args with --resume flag
    let claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--verbose".to_string(),
        "--model".to_string(),
        model.to_string(),
        "--resume".to_string(),
        cc_session_id.to_string(),
        "-p".to_string(),
        prompt.to_string(),
    ];

    // Create container configuration
    let container_config = ContainerConfig::new(
        session_id.to_string(),
        worktree_path.to_path_buf(),
        fifo_dir.clone(),
        claude_args,
    )?
    .with_timeout(timeout_secs);

    // Mark session as running
    state_manager.update_session(session_id, |s| {
        s.mark_running(None);
    })?;

    // Create tokio runtime for async Docker operations
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| ContinueError::Execution(format!("Failed to create tokio runtime: {}", e)))?;

    // IMPORTANT: Spawn FIFO reader BEFORE container to avoid deadlock.
    // See start.rs for detailed explanation.
    let fifo_reader = FifoReader::spawn(fifo_path);

    // Spawn container and wait for completion
    let container_result = runtime.block_on(async {
        let manager = ContainerManager::new()?;
        let container_id = manager.spawn(&container_config).await?;

        info!(container_id = %container_id, "Container spawned, waiting for completion");

        // Wait for container to exit
        let wait_result = manager.wait(&container_id).await;

        // Get logs on error
        if wait_result.as_ref().map(|c| *c != 0).unwrap_or(true) {
            if let Ok(logs) = manager.logs(&container_id).await {
                let code = wait_result.as_ref().map(|c| *c).unwrap_or(-1);
                warn!(exit_code = %code, logs = %logs, "Container exited with non-zero code");
            }
        }

        // Always remove container
        if let Err(e) = manager.remove(&container_id, true).await {
            warn!(container_id = %container_id, error = %e, "Failed to remove container");
        }

        wait_result
    })?;

    // Join the FIFO reader thread
    let content = fifo_reader.join()
        .map_err(|e| ContinueError::Execution(format!("Failed to read result FIFO: {}", e)))?;

    // Parse the result from FIFO content
    let run_result: RunResult = if content.is_empty() {
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
        duration_secs: 0.0,
        error: None,
        model_usage: run_result.model_usage,
        cc_session_id: Some(run_result.session_id),
    })
}
