//! Session start command implementation.
//!
//! Creates a new session with:
//! - Unique session ID and branch name
//! - Git worktree for isolation
//! - Claude Code execution (local or Docker)
//! - State persistence
//! - Hub registration and live event streaming

use std::path::Path;
use std::time::Instant;
use tracing::{debug, info, warn};

use super::state::StateManager;
use super::types::{generate_branch_name, generate_session_id, SessionMetadata, SessionOutput};
use super::worktree::WorktreeManager;
use crate::docker::{run_claude_direct, ContainerConfig};

use mantle_shared::hub::{
    HubClient, HubConfig, ModelUsage as HubModelUsage, NodeCreateResponse, NodeRegister,
    NodeResult as HubNodeResult, SessionRegister, SyncEventStream,
};

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

    #[error("Hub error: {0}")]
    Hub(#[from] mantle_shared::error::MantleError),

    #[error("Validation error: {0}")]
    Validation(String),
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
    /// Optional JSON array of MCP decision tools for sum type outputs
    pub decision_tools: Option<String>,

    // === Graph Execution Tracking ===

    /// Hub session ID (for registering nodes in existing hub session)
    /// When provided, registers this node into an existing hub session
    /// instead of creating a new one.
    pub hub_session_id: Option<String>,
    /// Execution ID (e.g., "run-1") - human-readable run identifier
    pub execution_id: Option<String>,
    /// Node path in graph (e.g., "n0" for root, "n2.n0" for nested)
    pub node_path: Option<String>,
    /// Node type/handler name (e.g., "hTypes", "hImpl")
    pub node_type: Option<String>,
    /// Parent hub node ID (for tree structure in hub)
    pub parent_hub_node_id: Option<String>,
}

/// Calculate depth from node path.
///
/// Depth is the number of non-empty path segments.
/// - `""` → 0 (root, no path)
/// - `"n0"` → 1
/// - `"n2.n0"` → 2
/// - `"n2.n0."` → 2 (trailing dots ignored)
fn calculate_depth(node_path: &str) -> usize {
    if node_path.is_empty() {
        0
    } else {
        node_path.split('.').filter(|s| !s.is_empty()).count()
    }
}

/// Build node metadata for graph execution tracking.
///
/// Creates a JSON object with execution_id, node_path, node_type, and depth.
fn build_node_metadata(config: &StartConfig) -> Option<serde_json::Value> {
    if config.execution_id.is_none() && config.node_path.is_none() && config.node_type.is_none() {
        return None;
    }

    let depth = config
        .node_path
        .as_ref()
        .map(|p| calculate_depth(p))
        .unwrap_or(0);

    Some(serde_json::json!({
        "execution_id": config.execution_id,
        "node_path": config.node_path,
        "node_type": config.node_type,
        "depth": depth,
    }))
}

/// Build hierarchical branch name for graph execution tracking.
///
/// Format: `{execution_id}/{node_path_with_slashes}/{node_type}-{6hex}`
/// Examples:
/// - Root node: `run-1/hTypes-a3f2c1`
/// - Nested node: `run-1/n2/n0/hTypeAdversary-b4e5d2`
fn construct_hierarchical_branch(config: &StartConfig) -> String {
    let uuid_suffix = &uuid::Uuid::new_v4().to_string()[..6];

    // Use execution_id or fall back to slug
    let execution_id = config
        .execution_id
        .as_deref()
        .unwrap_or(&config.slug);

    // Use node_type or fall back to "node"
    let node_type = config.node_type.as_deref().unwrap_or("node");

    match &config.node_path {
        Some(path) => {
            let path_with_slashes = path.replace('.', "/");
            format!("{}/{}/{}-{}", execution_id, path_with_slashes, node_type, uuid_suffix)
        }
        None => {
            // Root node - no path prefix
            format!("{}/{}-{}", execution_id, node_type, uuid_suffix)
        }
    }
}

/// Validate graph tracking arguments.
///
/// When `hub_session_id` is provided (graph tracking mode), validates:
/// - `execution_id` is required
/// - No path separators (`/` or `\`) in identifiers
/// - `node_path` only contains alphanumeric chars and dots
fn validate_graph_tracking_args(config: &StartConfig) -> Result<()> {
    // Only validate if in graph tracking mode
    if config.hub_session_id.is_none() {
        return Ok(());
    }

    // execution_id is required for graph tracking
    let execution_id = config.execution_id.as_deref().ok_or_else(|| {
        StartError::Validation(
            "--execution-id is required when --hub-session-id is provided".into(),
        )
    })?;

    // Validate no path separators in execution_id
    if execution_id.contains('/') || execution_id.contains('\\') {
        return Err(StartError::Validation(
            "execution_id cannot contain path separators (/ or \\)".into(),
        ));
    }

    // Validate node_type if provided
    if let Some(ref node_type) = config.node_type {
        if node_type.is_empty() {
            return Err(StartError::Validation("node_type cannot be empty".into()));
        }
        if node_type.contains('/') || node_type.contains('\\') {
            return Err(StartError::Validation(
                "node_type cannot contain path separators (/ or \\)".into(),
            ));
        }
    }

    // Validate node_path if provided (alphanumeric and dots only)
    if let Some(ref node_path) = config.node_path {
        if !node_path.chars().all(|c| c.is_alphanumeric() || c == '.') {
            return Err(StartError::Validation(
                "node_path can only contain alphanumeric characters and dots".into(),
            ));
        }
    }

    Ok(())
}

/// Start a new session.
///
/// This function supports two flows:
///
/// **Legacy Flow** (when `hub_session_id` is not provided):
/// 1. Checks hub is reachable (fails if not)
/// 2. Generates unique session ID and branch name
/// 3. Creates git worktree for isolation
/// 4. Records session in state file
/// 5. Registers session with mantle-hub (creates session + root node)
/// 6. Connects WebSocket for live event streaming
/// 7. Executes Claude Code via Docker (streaming events to hub)
/// 8. Submits final result to mantle-hub
///
/// **Graph Execution Tracking Flow** (when `hub_session_id` is provided):
/// 1. Checks hub is reachable (fails if not)
/// 2. Generates unique session ID
/// 3. Builds hierarchical branch name from execution_id/node_path/node_type
/// 4. Creates git worktree for isolation
/// 5. Records session in state file
/// 6. Registers node in existing hub session (with metadata)
/// 7. Connects WebSocket for live event streaming
/// 8. Executes Claude Code via Docker (streaming events to hub)
/// 9. Submits final result to mantle-hub
///
/// # Arguments
/// * `repo_root` - Path to the git repository root
/// * `config` - Session configuration
///
/// # Returns
/// Session output with results
pub fn start_session(repo_root: &Path, config: &StartConfig) -> Result<SessionOutput> {
    // Validate graph tracking args before anything else
    validate_graph_tracking_args(config)?;

    let start_time = Instant::now();

    // Load hub config
    let hub_config = HubConfig::load();

    // 1. Health check hub - FAIL if unreachable
    check_hub_reachable(&hub_config)?;

    // Initialize managers
    let state_manager = StateManager::new(repo_root)?;
    let worktree_manager =
        WorktreeManager::new(repo_root, &state_manager.worktrees_dir())?;

    // Generate unique identifiers
    let session_id = generate_session_id();

    // Determine branch name based on flow
    let branch = if config.hub_session_id.is_some() {
        // Graph execution tracking: use hierarchical branch
        construct_hierarchical_branch(config)
    } else {
        // Legacy flow: use slug-based branch
        generate_branch_name(&config.slug)
    };

    info!(
        session_id = %session_id,
        slug = %config.slug,
        branch = %branch,
        model = %config.model,
        hub_session_id = ?config.hub_session_id,
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

    // 2. Register with hub - either new session or node in existing session
    let (hub_session_id, hub_node_id) = if let Some(ref existing_hub_session_id) = config.hub_session_id {
        // Graph execution tracking: register node in existing session
        let node_response = register_node_in_existing_session(
            &hub_config,
            existing_hub_session_id,
            &branch,
            &worktree_path,
            config,
        )?;
        (existing_hub_session_id.clone(), node_response.node.id)
    } else {
        // Legacy flow: create new session with root node
        register_with_hub(&hub_config, &branch, &worktree_path, config)?
    };
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Registered with hub");

    // 3. Connect WebSocket for event streaming
    let event_stream =
        SyncEventStream::connect(&hub_config.http_url, &hub_session_id, &hub_node_id)?;
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Connected event stream to hub");

    // 4. Execute Claude Code via Docker with event streaming
    let result = execute_docker_with_streaming(
        &session_id,
        &branch,
        &worktree_path,
        config,
        &state_manager,
        event_stream,
    )?;

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

    // 5. Submit final result to hub
    submit_result_to_hub(&hub_config, &hub_session_id, &hub_node_id, &output)?;
    info!(hub_session_id = %hub_session_id, hub_node_id = %hub_node_id, "Submitted result to hub");

    Ok(output)
}

/// Check if hub is reachable.
fn check_hub_reachable(hub_config: &HubConfig) -> Result<()> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| StartError::Execution(format!("Failed to create runtime: {}", e)))?;

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
    config: &StartConfig,
) -> Result<(String, String)> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| StartError::Execution(format!("Failed to create runtime: {}", e)))?;

    rt.block_on(async {
        let client = HubClient::from_config(hub_config)?;

        let req = SessionRegister {
            branch: branch.to_string(),
            worktree: worktree_path.to_path_buf(),
            prompt: config.prompt.clone(),
            model: config.model.clone(),
        };

        let resp = client.create_session(&req).await?;
        Ok((resp.session.id, resp.root_node.id))
    })
}

/// Register a node in an existing hub session.
///
/// Used for graph execution tracking where the hub session is created by
/// the orchestrator (Haskell) and nodes are registered into it.
///
/// Returns the NodeCreateResponse containing the new node info.
fn register_node_in_existing_session(
    hub_config: &HubConfig,
    hub_session_id: &str,
    branch: &str,
    worktree_path: &Path,
    config: &StartConfig,
) -> Result<NodeCreateResponse> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| StartError::Execution(format!("Failed to create runtime: {}", e)))?;

    rt.block_on(async {
        let client = HubClient::from_config(hub_config)?;

        // Build metadata for graph execution tracking
        let metadata = build_node_metadata(config);

        let req = NodeRegister {
            branch: branch.to_string(),
            worktree: worktree_path.to_path_buf(),
            prompt: config.prompt.clone(),
            model: config.model.clone(),
            parent_node_id: config.parent_hub_node_id.clone(),
            metadata,
        };

        debug!(
            hub_session_id = %hub_session_id,
            parent_hub_node_id = ?config.parent_hub_node_id,
            metadata = ?req.metadata,
            "Registering node in existing hub session"
        );

        client.create_node(hub_session_id, &req).await.map_err(|e| {
            // Provide better error message for common failure cases
            let err_str = e.to_string();
            if err_str.contains("parent") || err_str.contains("FOREIGN KEY") {
                StartError::Validation(format!(
                    "Invalid parent_hub_node_id '{}': parent node does not exist in session '{}'",
                    config.parent_hub_node_id.as_deref().unwrap_or("none"),
                    hub_session_id
                ))
            } else if err_str.contains("404") || err_str.contains("not found") {
                StartError::Validation(format!(
                    "Hub session '{}' not found. Ensure the orchestrator created the session first.",
                    hub_session_id
                ))
            } else {
                // Propagate other hub errors as-is
                StartError::Hub(e)
            }
        })
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
        .map_err(|e| StartError::Execution(format!("Failed to create runtime: {}", e)))?;

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

/// Execute Claude Code in Docker with live event streaming to hub.
///
/// Uses TTY for stream-json support. Events are streamed to hub via WebSocket
/// for real-time supervision.
fn execute_docker_with_streaming(
    session_id: &str,
    branch: &str,
    worktree_path: &Path,
    config: &StartConfig,
    state_manager: &StateManager,
    event_stream: SyncEventStream,
) -> Result<SessionOutput> {
    use std::cell::RefCell;

    info!(session_id = %session_id, "Starting Docker execution with hub streaming");

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
    let mut container_config = ContainerConfig::new(
        session_id.to_string(),
        worktree_path.to_path_buf(),
        claude_args,
    )?
    .with_timeout(config.timeout_secs);

    // Pass decision tools to container via environment variable
    // mantle-agent mcp server will serve these as MCP tools to Claude Code
    if let Some(ref tools_json) = config.decision_tools {
        debug!(tools = %tools_json, "Passing decision tools to container");
        container_config = container_config.with_decision_tools(tools_json);
    }

    // Mark session as running
    state_manager.update_session(session_id, |s| {
        s.mark_running(None);
    })?;

    // Wrap event_stream in RefCell for interior mutability in the closure
    // This allows the closure to capture by shared reference while still mutating
    let event_stream = RefCell::new(event_stream);

    // Run Claude with event sink that forwards to hub
    let run_result = run_claude_direct(&container_config, Some(|event: &_| {
        // Forward each event to hub via WebSocket
        if let Err(e) = event_stream.borrow_mut().send_event(event) {
            warn!("Failed to forward event to hub: {}", e);
        }
    }))?;

    // Close the WebSocket gracefully
    if let Err(e) = event_stream.into_inner().close() {
        warn!("Failed to close event stream: {}", e);
    }

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
        tool_calls: run_result.tool_calls,
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
            decision_tools: None,
            hub_session_id: None,
            execution_id: None,
            node_path: None,
            node_type: None,
            parent_hub_node_id: None,
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
            decision_tools: None,
            hub_session_id: None,
            execution_id: None,
            node_path: None,
            node_type: None,
            parent_hub_node_id: None,
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
            decision_tools: None,
            hub_session_id: None,
            execution_id: None,
            node_path: None,
            node_type: None,
            parent_hub_node_id: None,
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
            decision_tools: None,
            hub_session_id: None,
            execution_id: None,
            node_path: None,
            node_type: None,
            parent_hub_node_id: None,
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
            decision_tools: None,
            hub_session_id: None,
            execution_id: None,
            node_path: None,
            node_type: None,
            parent_hub_node_id: None,
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
            decision_tools: None,
            hub_session_id: None,
            execution_id: None,
            node_path: None,
            node_type: None,
            parent_hub_node_id: None,
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
            decision_tools: None,
            hub_session_id: None,
            execution_id: None,
            node_path: None,
            node_type: None,
            parent_hub_node_id: None,
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

    // =========================================================================
    // Depth Calculation Tests
    // =========================================================================

    #[test]
    fn test_calculate_depth_empty_path() {
        assert_eq!(calculate_depth(""), 0);
    }

    #[test]
    fn test_calculate_depth_single_segment() {
        assert_eq!(calculate_depth("n0"), 1);
        assert_eq!(calculate_depth("hTypes"), 1);
    }

    #[test]
    fn test_calculate_depth_multiple_segments() {
        assert_eq!(calculate_depth("n2.n0"), 2);
        assert_eq!(calculate_depth("n1.n2.n3"), 3);
    }

    #[test]
    fn test_calculate_depth_trailing_dot() {
        // Trailing dots should be ignored
        assert_eq!(calculate_depth("n2.n0."), 2);
        assert_eq!(calculate_depth("n0."), 1);
    }

    #[test]
    fn test_calculate_depth_leading_dot() {
        // Leading dots should be ignored
        assert_eq!(calculate_depth(".n0"), 1);
        assert_eq!(calculate_depth(".n2.n0"), 2);
    }

    #[test]
    fn test_calculate_depth_consecutive_dots() {
        // Multiple consecutive dots should count as empty segments (ignored)
        assert_eq!(calculate_depth("n0..n1"), 2);
        assert_eq!(calculate_depth("..."), 0);
    }

    // =========================================================================
    // Validation Tests
    // =========================================================================

    fn make_config_for_validation() -> StartConfig {
        StartConfig {
            slug: "test".to_string(),
            prompt: "test".to_string(),
            model: "sonnet".to_string(),
            timeout_secs: 0,
            base_branch: None,
            json_schema: None,
            decision_tools: None,
            hub_session_id: None,
            execution_id: None,
            node_path: None,
            node_type: None,
            parent_hub_node_id: None,
        }
    }

    #[test]
    fn test_validation_passes_without_graph_tracking() {
        // When hub_session_id is None, validation passes even with missing fields
        let config = make_config_for_validation();
        assert!(validate_graph_tracking_args(&config).is_ok());
    }

    #[test]
    fn test_validation_requires_execution_id() {
        let mut config = make_config_for_validation();
        config.hub_session_id = Some("session-123".to_string());
        // Missing execution_id should fail
        let result = validate_graph_tracking_args(&config);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("--execution-id is required"));
    }

    #[test]
    fn test_validation_rejects_execution_id_with_slash() {
        let mut config = make_config_for_validation();
        config.hub_session_id = Some("session-123".to_string());
        config.execution_id = Some("run/1".to_string()); // has forward slash
        let result = validate_graph_tracking_args(&config);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("path separators"));
    }

    #[test]
    fn test_validation_rejects_execution_id_with_backslash() {
        let mut config = make_config_for_validation();
        config.hub_session_id = Some("session-123".to_string());
        config.execution_id = Some("run\\1".to_string()); // has backslash
        let result = validate_graph_tracking_args(&config);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("path separators"));
    }

    #[test]
    fn test_validation_rejects_empty_node_type() {
        let mut config = make_config_for_validation();
        config.hub_session_id = Some("session-123".to_string());
        config.execution_id = Some("run-1".to_string());
        config.node_type = Some("".to_string()); // empty
        let result = validate_graph_tracking_args(&config);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("node_type cannot be empty"));
    }

    #[test]
    fn test_validation_rejects_node_type_with_slash() {
        let mut config = make_config_for_validation();
        config.hub_session_id = Some("session-123".to_string());
        config.execution_id = Some("run-1".to_string());
        config.node_type = Some("h/Types".to_string()); // has slash
        let result = validate_graph_tracking_args(&config);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("node_type"));
    }

    #[test]
    fn test_validation_rejects_node_path_with_special_chars() {
        let mut config = make_config_for_validation();
        config.hub_session_id = Some("session-123".to_string());
        config.execution_id = Some("run-1".to_string());
        config.node_path = Some("n0-n1".to_string()); // hyphen not allowed
        let result = validate_graph_tracking_args(&config);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("node_path"));
    }

    #[test]
    fn test_validation_accepts_valid_graph_tracking_args() {
        let mut config = make_config_for_validation();
        config.hub_session_id = Some("session-123".to_string());
        config.execution_id = Some("run-1".to_string());
        config.node_path = Some("n2.n0".to_string());
        config.node_type = Some("hTypes".to_string());
        assert!(validate_graph_tracking_args(&config).is_ok());
    }

    #[test]
    fn test_validation_accepts_empty_node_path() {
        // Empty node_path is valid (represents root)
        let mut config = make_config_for_validation();
        config.hub_session_id = Some("session-123".to_string());
        config.execution_id = Some("run-1".to_string());
        config.node_path = Some("".to_string());
        assert!(validate_graph_tracking_args(&config).is_ok());
    }
}
