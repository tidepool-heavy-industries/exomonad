//! Session fork command implementation.
//!
//! Creates a child session from a parent, preserving context via --fork-session.
//! The child gets its own worktree and branch while inheriting parent's conversation.
//!
//! Hub integration: Forked sessions register with parent_id for lineage tracking.

use std::cell::RefCell;
use std::path::Path;
use std::time::Instant;
use tracing::{debug, info, warn};

use super::state::StateManager;
use super::types::{generate_branch_name, generate_session_id, SessionMetadata, SessionOutput};
use super::worktree::WorktreeManager;
use crate::docker::{run_claude_direct, ContainerConfig};

use mantle_shared::hub::{
    HubClient, HubConfig, ModelUsage as HubModelUsage, SessionRegister,
    SessionResult as HubSessionResult, SyncEventStream,
};

/// Error types for session fork operations.
#[derive(Debug, thiserror::Error)]
pub enum ForkError {
    #[error("State error: {0}")]
    State(#[from] super::state::StateError),

    #[error("Worktree error: {0}")]
    Worktree(#[from] super::worktree::WorktreeError),

    #[error("Parent session not found: {0}")]
    ParentNotFound(String),

    #[error("Parent session has no Claude Code session ID")]
    NoCcSessionId,

    #[error("Parent worktree not found: {0}")]
    ParentWorktreeNotFound(String),

    #[error("Parent worktree is corrupted (missing .git): {0}")]
    ParentWorktreeCorrupted(String),

    #[error("Parent branch not found in git: {0}")]
    ParentBranchNotFound(String),

    #[error("Execution error: {0}")]
    Execution(String),

    #[error("Docker error: {0}")]
    Docker(#[from] crate::docker::DockerError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Hub error: {0}")]
    Hub(#[from] mantle_shared::error::MantleError),
}

pub type Result<T> = std::result::Result<T, ForkError>;

/// Configuration for forking a session.
#[derive(Debug, Clone)]
pub struct ForkConfig {
    /// Parent session ID to fork from
    pub parent_id: String,
    /// Slug for the child session
    pub child_slug: String,
    /// Prompt for the child session
    pub child_prompt: String,
    /// Timeout in seconds (0 = no timeout)
    pub timeout_secs: u64,
    /// Use Docker container
    pub docker: bool,
}

/// Fork a session to create a child with inherited context.
///
/// This function:
/// 1. Loads parent session metadata
/// 2. Creates new worktree branching from parent's branch
/// 3. Creates child session with parent reference
/// 4. Executes Claude Code with --fork-session flag
/// 5. Updates both parent and child state
///
/// # Arguments
/// * `repo_root` - Path to the git repository root
/// * `config` - Fork configuration
///
/// # Returns
/// Session output for the child session
pub fn fork_session(repo_root: &Path, config: &ForkConfig) -> Result<SessionOutput> {
    let start_time = Instant::now();

    // Initialize managers
    let state_manager = StateManager::new(repo_root)?;
    let worktree_manager = WorktreeManager::new(repo_root, &state_manager.worktrees_dir())?;

    // Load parent session
    let parent = state_manager.get_session(&config.parent_id)?;

    // Validate parent worktree exists and is intact
    if !parent.worktree.exists() {
        return Err(ForkError::ParentWorktreeNotFound(
            parent.worktree.display().to_string(),
        ));
    }

    let git_path = parent.worktree.join(".git");
    if !git_path.exists() {
        return Err(ForkError::ParentWorktreeCorrupted(
            parent.worktree.display().to_string(),
        ));
    }

    // Verify parent branch still exists
    let branch_check = std::process::Command::new("git")
        .arg("-C")
        .arg(&parent.worktree)
        .arg("rev-parse")
        .arg("--verify")
        .arg(&parent.branch)
        .output();

    match branch_check {
        Ok(output) if !output.status.success() => {
            return Err(ForkError::ParentBranchNotFound(parent.branch.clone()));
        }
        Err(e) => {
            debug!(error = %e, "Failed to verify parent branch, continuing anyway");
        }
        _ => {}
    }

    // Need parent's cc_session_id for --fork-session
    let parent_cc_session_id = parent.cc_session_id.clone().ok_or(ForkError::NoCcSessionId)?;

    // Generate child identifiers
    let child_id = generate_session_id();
    let child_branch = generate_branch_name(&config.child_slug);

    info!(
        parent_id = %config.parent_id,
        child_id = %child_id,
        child_slug = %config.child_slug,
        child_branch = %child_branch,
        "Forking session"
    );

    // Create child worktree from parent's branch
    let child_worktree = worktree_manager.create(&child_branch, Some(&parent.branch))?;

    debug!(
        worktree = %child_worktree.display(),
        base_branch = %parent.branch,
        "Created child worktree"
    );

    // Create child session metadata
    let child_session = SessionMetadata::fork_from(
        &parent,
        child_id.clone(),
        config.child_slug.clone(),
        child_branch.clone(),
        child_worktree.clone(),
    );

    // Save child session
    state_manager.insert_session(child_session)?;

    // Update parent's child_ids
    state_manager.with_state(|state| {
        state.add_child(&config.parent_id, &child_id)
    })?;

    if config.docker {
        let result = execute_docker_fork(
            &child_id,
            &child_branch,
            &child_worktree,
            &parent_cc_session_id,
            &parent.model,
            &config.child_prompt,
            config.timeout_secs,
            &state_manager,
        )?;

        let duration = start_time.elapsed().as_secs_f64();

        // Update child session state
        state_manager.update_session(&child_id, |s| {
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

        return Ok(output);
    }

    // Execute with --fork-session flag (local mode)
    let result = execute_fork(
        &child_id,
        &child_branch,
        &child_worktree,
        &parent_cc_session_id,
        &parent.model,
        &config.child_prompt,
        config.timeout_secs,
        &state_manager,
    )?;

    let duration = start_time.elapsed().as_secs_f64();

    // Update child session state
    state_manager.update_session(&child_id, |s| {
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

fn execute_fork(
    session_id: &str,
    branch: &str,
    worktree_path: &Path,
    parent_cc_session_id: &str,
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
        .map_err(|e| ForkError::Execution(format!("Failed to create result FIFO: {}", e)))?;

    // Build claude args with --resume and --fork-session
    // --fork-session makes it read-only fork (doesn't modify parent's context)
    let claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "json".to_string(),
        "--verbose".to_string(),
        "--model".to_string(),
        model.to_string(),
        "--resume".to_string(),
        parent_cc_session_id.to_string(),
        "--fork-session".to_string(),
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

    debug!(cmd = ?cmd, "Spawning mantle-agent for fork");

    let _child = cmd.spawn()
        .map_err(|e| ForkError::Execution(format!("Failed to spawn mantle-agent: {}", e)))?;

    // Read result from FIFO
    let timeout = if timeout_secs > 0 {
        std::time::Duration::from_secs(timeout_secs)
    } else {
        std::time::Duration::ZERO
    };

    let run_result: RunResult = result_fifo
        .read_with_timeout(timeout)
        .map_err(|e| ForkError::Execution(format!("Failed to read result: {}", e)))?;

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

/// Check if hub is reachable.
fn check_hub_reachable(hub_config: &HubConfig) -> Result<()> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| ForkError::Execution(format!("Failed to create runtime: {}", e)))?;

    rt.block_on(async {
        let client = HubClient::from_config(hub_config)?;
        client.health_check().await?;
        Ok(())
    })
}

/// Register a session with the hub.
fn register_with_hub(
    hub_config: &HubConfig,
    branch: &str,
    worktree_path: &Path,
    model: &str,
    prompt: &str,
    parent_id: Option<String>,
) -> Result<String> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| ForkError::Execution(format!("Failed to create runtime: {}", e)))?;

    rt.block_on(async {
        let client = HubClient::from_config(hub_config)?;

        let req = SessionRegister {
            branch: branch.to_string(),
            worktree: worktree_path.to_path_buf(),
            prompt: prompt.to_string(),
            model: model.to_string(),
            parent_id,
        };

        let info = client.register_session(&req).await?;
        Ok(info.id)
    })
}

/// Submit a session result to the hub.
fn submit_result_to_hub(
    hub_config: &HubConfig,
    hub_session_id: &str,
    output: &SessionOutput,
) -> Result<()> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| ForkError::Execution(format!("Failed to create runtime: {}", e)))?;

    rt.block_on(async {
        let client = HubClient::from_config(hub_config)?;

        // Convert model_usage from events::ModelUsage to hub::ModelUsage
        let model_usage = output
            .model_usage
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    HubModelUsage {
                        input_tokens: v.input_tokens,
                        output_tokens: v.output_tokens,
                        cache_read_input_tokens: v.cache_read_input_tokens,
                        cache_creation_input_tokens: v.cache_creation_input_tokens,
                        cost_usd: v.cost_usd,
                    },
                )
            })
            .collect();

        let result = HubSessionResult {
            session_id: hub_session_id.to_string(),
            exit_code: output.exit_code,
            is_error: output.is_error,
            result_text: output.result_text.clone(),
            structured_output: output.structured_output.clone(),
            total_cost_usd: output.total_cost_usd,
            num_turns: output.num_turns,
            cc_session_id: output.cc_session_id.clone().unwrap_or_default(),
            duration_secs: output.duration_secs,
            model_usage,
        };

        client.submit_result(&result).await?;
        Ok(())
    })
}

/// Execute session fork directly in a Docker container with hub streaming.
fn execute_docker_fork(
    session_id: &str,
    branch: &str,
    worktree_path: &Path,
    parent_cc_session_id: &str,
    model: &str,
    prompt: &str,
    timeout_secs: u64,
    state_manager: &StateManager,
) -> Result<SessionOutput> {
    // Load hub config
    let hub_config = HubConfig::load();

    // 1. Health check hub - FAIL if unreachable
    check_hub_reachable(&hub_config)?;

    info!(session_id = %session_id, "Starting Docker fork execution with hub streaming");

    // 2. Register session with hub (with parent_id for lineage tracking)
    // Note: We use the mantle session_id as parent_id for simplicity
    // In production, you might want to track the hub_session_id of the parent
    let hub_session_id = register_with_hub(
        &hub_config,
        branch,
        worktree_path,
        model,
        &format!("[fork] {}", prompt),
        None, // Could track parent hub session id for full lineage
    )?;
    info!(hub_session_id = %hub_session_id, "Registered fork with hub");

    // 3. Connect WebSocket for event streaming
    let event_stream = SyncEventStream::connect(&hub_config.http_url, &hub_session_id)?;
    info!(hub_session_id = %hub_session_id, "Connected event stream to hub");

    // Build Claude args with --resume and --fork-session flags and stream-json
    // --fork-session makes it a read-only fork (doesn't modify parent's context)
    let claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--verbose".to_string(),
        "--model".to_string(),
        model.to_string(),
        "--resume".to_string(),
        parent_cc_session_id.to_string(),
        "--fork-session".to_string(),
        "-p".to_string(),
        prompt.to_string(),
    ];

    // Create container configuration
    let container_config = ContainerConfig::new(
        session_id.to_string(),
        worktree_path.to_path_buf(),
        claude_args,
    )?
    .with_timeout(timeout_secs);

    // Mark session as running
    state_manager.update_session(session_id, |s| {
        s.mark_running(None);
    })?;

    // Wrap event_stream in RefCell for interior mutability in the closure
    let event_stream = RefCell::new(event_stream);

    // Run Claude with event sink that forwards to hub
    let run_result = run_claude_direct(&container_config, Some(|event: &_| {
        if let Err(e) = event_stream.borrow_mut().send_event(event) {
            warn!("Failed to forward event to hub: {}", e);
        }
    }))?;

    // Close the WebSocket gracefully
    if let Err(e) = event_stream.into_inner().close() {
        warn!("Failed to close event stream: {}", e);
    }

    let output = SessionOutput {
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
    };

    // 5. Submit final result to hub
    submit_result_to_hub(&hub_config, &hub_session_id, &output)?;
    info!(hub_session_id = %hub_session_id, "Submitted fork result to hub");

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::types::SessionState;
    use std::process::Command as ProcessCommand;
    use tempfile::TempDir;

    fn setup_git_repo() -> TempDir {
        let temp_dir = TempDir::new().unwrap();

        ProcessCommand::new("git")
            .args(["init"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        ProcessCommand::new("git")
            .args(["config", "user.email", "test@test.com"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        ProcessCommand::new("git")
            .args(["config", "user.name", "Test"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        std::fs::write(temp_dir.path().join("README.md"), "# Test").unwrap();

        ProcessCommand::new("git")
            .args(["add", "."])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        ProcessCommand::new("git")
            .args(["commit", "-m", "Initial commit"])
            .current_dir(temp_dir.path())
            .output()
            .unwrap();

        temp_dir
    }

    #[test]
    fn test_fork_requires_cc_session_id() {
        let temp_dir = setup_git_repo();
        let state_manager = StateManager::new(temp_dir.path()).unwrap();
        let worktree_manager = WorktreeManager::new(
            temp_dir.path(),
            &state_manager.worktrees_dir(),
        ).unwrap();

        // Create a real worktree for the parent
        let parent_worktree = worktree_manager.create("parent/test-abc123", None).unwrap();

        // Create parent session without cc_session_id (but with valid worktree)
        let parent = SessionMetadata::new(
            "parent-123".to_string(),
            "parent/test".to_string(),
            "parent/test-abc123".to_string(),
            parent_worktree,
            "sonnet".to_string(),
        );
        state_manager.insert_session(parent).unwrap();

        let config = ForkConfig {
            parent_id: "parent-123".to_string(),
            child_slug: "child/test".to_string(),
            child_prompt: "Test prompt".to_string(),
            timeout_secs: 0,
            docker: false,
        };

        let result = fork_session(temp_dir.path(), &config);
        assert!(matches!(result, Err(ForkError::NoCcSessionId)));
    }

    #[test]
    fn test_fork_session_metadata() {
        let temp_dir = setup_git_repo();
        let state_manager = StateManager::new(temp_dir.path()).unwrap();
        let worktree_manager = WorktreeManager::new(
            temp_dir.path(),
            &state_manager.worktrees_dir(),
        ).unwrap();

        // Create parent session with cc_session_id
        let parent_worktree = worktree_manager.create("parent/test-abc123", None).unwrap();
        let mut parent = SessionMetadata::new(
            "parent-123".to_string(),
            "parent/test".to_string(),
            "parent/test-abc123".to_string(),
            parent_worktree,
            "sonnet".to_string(),
        );
        parent.cc_session_id = Some("cc-parent-session".to_string());
        parent.state = SessionState::Completed;
        state_manager.insert_session(parent).unwrap();

        // Test that fork_from creates correct metadata
        let parent_loaded = state_manager.get_session("parent-123").unwrap();
        let child = SessionMetadata::fork_from(
            &parent_loaded,
            "child-456".to_string(),
            "child/test".to_string(),
            "child/test-def456".to_string(),
            temp_dir.path().join(".mantle/worktrees/child-test"),
        );

        assert_eq!(child.id, "child-456");
        assert_eq!(child.parent_id, Some("parent-123".to_string()));
        assert_eq!(child.cc_session_id, Some("cc-parent-session".to_string()));
        assert_eq!(child.model, "sonnet"); // Inherited from parent
        assert_eq!(child.state, SessionState::Pending);
    }
}
