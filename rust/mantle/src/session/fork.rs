//! Session fork command implementation.
//!
//! Creates a child session from a parent, preserving context via --fork-session.
//! The child gets its own worktree and branch while inheriting parent's conversation.
//!
//! ## Stateless Design
//!
//! All parent session data is passed as CLI arguments - no state file lookup.
//! The caller (Haskell) maintains parent info from previous SessionOutput.
//!
//! Hub integration: Forked sessions register with parent_id for lineage tracking.

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::time::Instant;
use tracing::{debug, info, warn};

use super::types::{generate_branch_name, generate_session_id, SessionOutput};
use super::worktree::WorktreeManager;
use crate::docker::{run_claude_direct, ContainerConfig};

use mantle_shared::hub::{
    HubClient, HubConfig, ModelUsage as HubModelUsage, NodeResult as HubNodeResult,
    SessionRegister, SyncEventStream,
};

/// Error types for session fork operations.
#[derive(Debug, thiserror::Error)]
pub enum ForkError {
    #[error("Worktree error: {0}")]
    Worktree(#[from] super::worktree::WorktreeError),

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
///
/// All parent session data is passed as required arguments - no state lookup.
#[derive(Debug, Clone)]
pub struct ForkConfig {
    /// Parent's Claude Code session ID (for --fork-session)
    pub parent_cc_session_id: String,
    /// Parent's worktree path
    pub parent_worktree: PathBuf,
    /// Parent's git branch
    pub parent_branch: String,
    /// Model to use (inherited from parent typically)
    pub model: String,
    /// Slug for the child session
    pub child_slug: String,
    /// Prompt for the child session
    pub child_prompt: String,
    /// Timeout in seconds (0 = no timeout)
    pub timeout_secs: u64,
    /// Optional path to file containing JSON array of MCP decision tools for sum type outputs
    pub decision_tools_file: Option<String>,
}

/// Fork a session to create a child with inherited context.
///
/// All parent session data is passed via config - no state file lookup.
///
/// This function:
/// 1. Validates parent worktree exists and is intact
/// 2. Creates new worktree branching from parent's branch
/// 3. Registers with hub for observability
/// 4. Executes Claude Code with --fork-session flag
/// 5. Returns session output
///
/// # Arguments
/// * `repo_root` - Path to the git repository root
/// * `config` - Fork configuration with all required parent data
///
/// # Returns
/// Session output for the child session
pub fn fork_session(repo_root: &Path, config: &ForkConfig) -> Result<SessionOutput> {
    let start_time = Instant::now();

    // Initialize worktree manager
    let mantle_dir = repo_root.join(".mantle");
    let worktrees_dir = mantle_dir.join("worktrees");
    let worktree_manager = WorktreeManager::new(repo_root, &worktrees_dir)?;

    // Validate parent worktree exists and is intact
    if !config.parent_worktree.exists() {
        return Err(ForkError::ParentWorktreeNotFound(
            config.parent_worktree.display().to_string(),
        ));
    }

    let git_path = config.parent_worktree.join(".git");
    if !git_path.exists() {
        return Err(ForkError::ParentWorktreeCorrupted(
            config.parent_worktree.display().to_string(),
        ));
    }

    // Verify parent branch still exists
    let branch_check = std::process::Command::new("git")
        .arg("-C")
        .arg(&config.parent_worktree)
        .arg("rev-parse")
        .arg("--verify")
        .arg(&config.parent_branch)
        .output();

    match branch_check {
        Ok(output) if !output.status.success() => {
            return Err(ForkError::ParentBranchNotFound(config.parent_branch.clone()));
        }
        Err(e) => {
            debug!(error = %e, "Failed to verify parent branch, continuing anyway");
        }
        _ => {}
    }

    // Generate child identifiers
    let child_id = generate_session_id();
    let child_branch = generate_branch_name(&config.child_slug);

    info!(
        parent_cc_session_id = %config.parent_cc_session_id,
        child_id = %child_id,
        child_slug = %config.child_slug,
        child_branch = %child_branch,
        "Forking session"
    );

    // Create child worktree from parent's branch
    let child_worktree = worktree_manager.create(&child_branch, Some(&config.parent_branch))?;

    debug!(
        worktree = %child_worktree.display(),
        base_branch = %config.parent_branch,
        "Created child worktree"
    );

    // Read decision tools from file if provided
    let tools_json = if let Some(ref tools_file) = config.decision_tools_file {
        Some(std::fs::read_to_string(tools_file)
            .map_err(|e| ForkError::Execution(format!("Failed to read decision tools file {}: {}", tools_file, e)))?)
    } else {
        None
    };

    let result = execute_docker_fork(
        &child_id,
        &child_branch,
        &child_worktree,
        &config.parent_cc_session_id,
        &config.model,
        &config.child_prompt,
        config.timeout_secs,
        tools_json.as_deref(),
    )?;

    let duration = start_time.elapsed().as_secs_f64();

    let mut output = SessionOutput {
        duration_secs: duration,
        ..result
    };

    // Sanitize output to remove control characters before returning for JSON serialization
    output.sanitize();

    Ok(output)
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
///
/// Creates a new session with its root node and returns (session_id, node_id).
fn register_with_hub(
    hub_config: &HubConfig,
    branch: &str,
    worktree_path: &Path,
    model: &str,
    prompt: &str,
) -> Result<(String, String)> {
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
        };

        let resp = client.create_session(&req).await?;
        Ok((resp.session.id, resp.root_node.id))
    })
}

/// Submit a node result to the hub.
fn submit_result_to_hub(
    hub_config: &HubConfig,
    hub_session_id: &str,
    hub_node_id: &str,
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

        let result = HubNodeResult {
            node_id: hub_node_id.to_string(),
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

        client
            .submit_node_result(hub_session_id, hub_node_id, &result)
            .await?;
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
    decision_tools: Option<&str>,
) -> Result<SessionOutput> {
    // Load hub config
    let hub_config = HubConfig::load();

    // 1. Health check hub - FAIL if unreachable
    check_hub_reachable(&hub_config)?;

    info!(session_id = %session_id, "Starting Docker fork execution with hub streaming");

    // 2. Register session with hub (creates new session for this fork)
    let (hub_session_id, hub_node_id) = register_with_hub(
        &hub_config,
        branch,
        worktree_path,
        model,
        &format!("[fork] {}", prompt),
    )?;
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Registered fork with hub");

    // 3. Connect WebSocket for event streaming
    let event_stream =
        SyncEventStream::connect(&hub_config.http_url, &hub_session_id, &hub_node_id)?;
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Connected event stream to hub");

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
    let mut container_config = ContainerConfig::new(
        session_id.to_string(),
        worktree_path.to_path_buf(),
        claude_args,
    )?
    .with_timeout(timeout_secs);

    // Pass decision tools to container via file (avoids shell escaping issues)
    if let Some(tools_json) = decision_tools {
        debug!("Passing decision tools to container via file");
        container_config = container_config.with_decision_tools(tools_json)
            .map_err(|e| ForkError::Execution(format!("Failed to set decision tools: {}", e)))?;
    }

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
        tool_calls: run_result.tool_calls,
    };

    // 5. Submit final result to hub
    submit_result_to_hub(&hub_config, &hub_session_id, &hub_node_id, &output)?;
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Submitted fork result to hub");

    Ok(output)
}

// Tests removed - previous tests depended on StateManager which was removed.
// Fork functionality is now tested via integration tests with the full CLI.
