//! Session continue command implementation.
//!
//! Resumes an existing session with a new prompt while preserving
//! Claude Code's conversation context via --resume.
//!
//! ## Stateless Design
//!
//! All session data is passed as CLI arguments - no local state file.
//! The caller (Haskell) maintains session info from previous SessionOutput.
//!
//! Hub integration: Each continuation registers as a new hub session
//! for independent observability.

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::time::Instant;
use tracing::{debug, info, warn};

use super::types::SessionOutput;
use crate::docker::{run_claude_direct, ContainerConfig};

use mantle_shared::hub::{
    HubClient, HubConfig, ModelUsage as HubModelUsage, NodeResult as HubNodeResult,
    SessionRegister, SyncEventStream,
};

/// Error types for session continue operations.
#[derive(Debug, thiserror::Error)]
pub enum ContinueError {
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

    #[error("Hub error: {0}")]
    Hub(#[from] mantle_shared::error::MantleError),
}

pub type Result<T> = std::result::Result<T, ContinueError>;

/// Configuration for continuing a session.
///
/// All session data is passed as required arguments - no state lookup.
#[derive(Debug, Clone)]
pub struct ContinueConfig {
    /// Claude Code session ID (for --resume)
    pub cc_session_id: String,
    /// Worktree path where code lives
    pub worktree: PathBuf,
    /// Git branch name
    pub branch: String,
    /// Model to use (haiku, sonnet, opus)
    pub model: String,
    /// New prompt for the continuation
    pub prompt: String,
    /// Timeout in seconds (0 = no timeout)
    pub timeout_secs: u64,
    /// Optional path to file containing JSON array of MCP decision tools for sum type outputs
    pub decision_tools_file: Option<String>,
}

/// Continue an existing session with a new prompt.
///
/// All session data is passed via config - no state file lookup.
///
/// This function:
/// 1. Validates worktree exists and is intact
/// 2. Registers with hub for observability
/// 3. Resumes Claude Code with --resume flag
/// 4. Returns session output
///
/// # Arguments
/// * `_repo_root` - Path to the git repository root (unused, for API consistency)
/// * `config` - Continue configuration with all required session data
///
/// # Returns
/// Session output with results
pub fn continue_session(_repo_root: &Path, config: &ContinueConfig) -> Result<SessionOutput> {
    let start_time = Instant::now();

    // Validate worktree exists and is intact
    if !config.worktree.exists() {
        return Err(ContinueError::WorktreeNotFound(
            config.worktree.display().to_string(),
        ));
    }

    // Check worktree has .git (file or directory)
    let git_path = config.worktree.join(".git");
    if !git_path.exists() {
        return Err(ContinueError::WorktreeCorrupted(
            config.worktree.display().to_string(),
        ));
    }

    // Verify branch still exists in git
    let branch_check = std::process::Command::new("git")
        .arg("-C")
        .arg(&config.worktree)
        .arg("rev-parse")
        .arg("--verify")
        .arg(&config.branch)
        .output();

    match branch_check {
        Ok(output) if !output.status.success() => {
            return Err(ContinueError::BranchNotFound(config.branch.clone()));
        }
        Err(e) => {
            debug!(error = %e, "Failed to verify branch, continuing anyway");
            // Git not available or other error - continue anyway
        }
        _ => {}
    }

    info!(
        cc_session_id = %config.cc_session_id,
        branch = %config.branch,
        worktree = %config.worktree.display(),
        "Continuing session"
    );

    // Read decision tools from file if provided
    let tools_json = if let Some(ref tools_file) = config.decision_tools_file {
        Some(std::fs::read_to_string(tools_file)
            .map_err(|e| ContinueError::Execution(format!("Failed to read decision tools file {}: {}", tools_file, e)))?)
    } else {
        None
    };

    // Execute via Docker container with hub streaming
    let result = execute_docker_continue(
        &config.branch,
        &config.worktree,
        &config.cc_session_id,
        &config.model,
        &config.prompt,
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
        .map_err(|e| ContinueError::Execution(format!("Failed to create runtime: {}", e)))?;

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
        .map_err(|e| ContinueError::Execution(format!("Failed to create runtime: {}", e)))?;

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
        .map_err(|e| ContinueError::Execution(format!("Failed to create runtime: {}", e)))?;

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

/// Execute session continue directly in a Docker container with hub streaming.
fn execute_docker_continue(
    branch: &str,
    worktree_path: &Path,
    cc_session_id: &str,
    model: &str,
    prompt: &str,
    timeout_secs: u64,
    decision_tools: Option<&str>,
) -> Result<SessionOutput> {
    // Load hub config
    let hub_config = HubConfig::load();

    // 1. Health check hub - FAIL if unreachable
    check_hub_reachable(&hub_config)?;

    info!(cc_session_id = %cc_session_id, "Starting Docker continue execution with hub streaming");

    // 2. Register session with hub (as a continuation)
    let (hub_session_id, hub_node_id) = register_with_hub(
        &hub_config,
        branch,
        worktree_path,
        model,
        &format!("[continue] {}", prompt),
    )?;
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Registered continuation with hub");

    // 3. Connect WebSocket for event streaming
    let event_stream =
        SyncEventStream::connect(&hub_config.http_url, &hub_session_id, &hub_node_id)?;
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Connected event stream to hub");

    // Build Claude args with --resume flag and stream-json
    let claude_args = vec![
        "--dangerously-skip-permissions".to_string(),
        "--output-format".to_string(),
        "stream-json".to_string(),
        "--verbose".to_string(),
        "--max-turns".to_string(),
        "30".to_string(),
        "--model".to_string(),
        model.to_string(),
        "--resume".to_string(),
        cc_session_id.to_string(),
        "-p".to_string(),
        prompt.to_string(),
    ];

    // Create container configuration using cc_session_id as container name
    let mut container_config = ContainerConfig::new(
        cc_session_id.to_string(),
        worktree_path.to_path_buf(),
        claude_args,
    )?
    .with_timeout(timeout_secs);

    // Pass decision tools to container via file (avoids shell escaping issues)
    if let Some(tools_json) = decision_tools {
        debug!("Passing decision tools to container via file");
        container_config = container_config.with_decision_tools(tools_json)
            .map_err(|e| ContinueError::Execution(format!("Failed to set decision tools: {}", e)))?;
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
        session_id: cc_session_id.to_string(), // Use cc_session_id as session_id
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
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Submitted continuation result to hub");

    Ok(output)
}
