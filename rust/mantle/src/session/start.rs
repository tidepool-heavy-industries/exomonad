//! Session start command implementation.
//!
//! Creates a new session with:
//! - Unique session ID and branch name
//! - Git worktree for isolation
//! - Claude Code execution (local or Docker)
//! - State persistence

use std::path::Path;
use std::time::Instant;
use tracing::{debug, info};

use super::state::StateManager;
use super::types::{generate_branch_name, generate_session_id, SessionMetadata, SessionOutput};
use super::worktree::WorktreeManager;
use crate::docker::{run_claude_direct, ContainerConfig};

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
    /// Optional base branch (defaults to HEAD)
    pub base_branch: Option<String>,
    /// Optional JSON schema for structured output
    pub json_schema: Option<String>,
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

    // Execute Claude Code via Docker
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

    let mut output = SessionOutput {
        duration_secs: duration,
        ..result
    };

    // Sanitize output to remove control characters before returning for JSON serialization
    output.sanitize();

    Ok(output)
}

/// Execute Claude Code directly in a Docker container.
///
/// Uses TTY for stream-json support. Container runs in foreground - dies when
/// this process dies. Streaming output visible in terminal.
fn execute_docker(
    session_id: &str,
    branch: &str,
    worktree_path: &Path,
    config: &StartConfig,
    state_manager: &StateManager,
) -> Result<SessionOutput> {
    info!(session_id = %session_id, "Starting Docker execution (direct mode)");

    // Build Claude args - stream-json for real-time visibility
    let mut claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--verbose".to_string(),
        "--model".to_string(),
        config.model.clone(),
        "-p".to_string(),
        config.prompt.clone(),
    ];

    // Add JSON schema if provided (for structured output)
    if let Some(ref schema) = config.json_schema {
        claude_args.push("--json-schema".to_string());
        claude_args.push(schema.clone());
    }

    // Create container configuration
    let container_config = ContainerConfig::new(
        session_id.to_string(),
        worktree_path.to_path_buf(),
        claude_args,
    )?
    .with_timeout(config.timeout_secs);

    // Mark session as running
    state_manager.update_session(session_id, |s| {
        s.mark_running(None);
    })?;

    // Run Claude directly - stream-json output parsed on the fly
    let run_result = run_claude_direct(&container_config)?;

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
            base_branch: None,
            json_schema: None,
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
            base_branch: None,
            json_schema: None,
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
            base_branch: None,
            json_schema: None,
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
            base_branch: None,
            json_schema: None,
        };

        let (id1, branch1, _) = prepare_session(temp_dir.path(), &config1).unwrap();

        // Create second session
        let config2 = StartConfig {
            slug: "feature/api".to_string(),
            prompt: "Add API endpoints".to_string(),
            model: "haiku".to_string(),
            timeout_secs: 0,
            base_branch: None,
            json_schema: None,
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
            base_branch: Some("develop".to_string()),
            json_schema: None,
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
            base_branch: None,
            json_schema: None,
        };

        // Verify zero timeout means no timeout
        assert_eq!(config.timeout_secs, 0);
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
