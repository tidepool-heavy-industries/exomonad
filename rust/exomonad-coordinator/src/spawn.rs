//! Spawn workflow: effector setup -> open command pane -> running.
//!
//! Handles the async multi-step spawn process coordinated through
//! Zellij's event-driven plugin API.

use crate::protocol::{CoordinatorResponse, SetupArgs};
use crate::state::{AgentStatus, CommandStep, Coordinator, PendingCommand};
use exomonad_ui_protocol::StateUpdate;
use std::collections::BTreeMap;
use std::path::PathBuf;
use zellij_tile::prelude::*;

/// Initiate a spawn workflow.
///
/// 1. Register agent in SettingUp state
/// 2. Block the pipe for async response
/// 3. Run effector agent setup via run_command
pub fn handle_spawn(
    coordinator: &mut Coordinator,
    pipe_name: &str,
    request_id: String,
    agent_id: String,
    worktree_path: String,
    branch: String,
    agent_type: String,
    agent_command: String,
    setup_args: SetupArgs,
) {
    // Register the agent
    coordinator.register_agent(
        agent_id.clone(),
        worktree_path.clone(),
        branch.clone(),
        agent_type.clone(),
    );

    // Store pipe mapping for async response
    coordinator
        .pending_pipes
        .insert(request_id.clone(), pipe_name.to_string());

    // Store command context for correlating RunCommandResult
    let ctx_key = format!("spawn:{}:{}", request_id, agent_id);
    coordinator.pending_commands.insert(
        ctx_key.clone(),
        PendingCommand {
            request_id: request_id.clone(),
            agent_id: agent_id.clone(),
            step: CommandStep::Setup,
            created_at: coordinator.monotonic_clock,
        },
    );

    // Build context BTreeMap for run_command (comes back in RunCommandResult)
    let mut context: BTreeMap<String, String> = BTreeMap::new();
    context.insert("ctx_key".to_string(), ctx_key);
    context.insert("agent_command".to_string(), agent_command);
    context.insert("worktree_path".to_string(), worktree_path.clone());
    context.insert("agent_id".to_string(), agent_id);

    // Block pipe while we work
    block_cli_pipe_input(pipe_name);

    // Run effector as background command (result comes back via RunCommandResult event)
    // run_command takes &[&str], so build the command as string slices
    let cmd: Vec<String> = vec![
        "effector".to_string(),
        "agent".to_string(),
        "setup".to_string(),
        "--project-dir".to_string(),
        setup_args.project_dir,
        "--worktree-path".to_string(),
        worktree_path,
        "--branch".to_string(),
        branch,
        "--start-point".to_string(),
        setup_args.start_point,
        "--role".to_string(),
        setup_args.role,
        "--agent-type".to_string(),
        agent_type,
        "--sidecar-path".to_string(),
        setup_args.sidecar_path,
    ];
    let cmd_refs: Vec<&str> = cmd.iter().map(|s| s.as_str()).collect();

    run_command(&cmd_refs, context);
}

/// Handle RunCommandResult for the setup step.
///
/// On success, proceeds to open the command pane.
/// On failure, responds with error and unblocks pipe.
pub fn handle_setup_result(
    coordinator: &mut Coordinator,
    exit_code: Option<i32>,
    stdout: Vec<u8>,
    _stderr: Vec<u8>,
    context: BTreeMap<String, String>,
) {
    let ctx_key = match context.get("ctx_key") {
        Some(k) => k.clone(),
        None => return,
    };

    let pending = match coordinator.pending_commands.remove(&ctx_key) {
        Some(p) => p,
        None => return,
    };

    if pending.step != CommandStep::Setup {
        return;
    }

    let success = exit_code.map(|c| c == 0).unwrap_or(false);

    if !success {
        let output = String::from_utf8_lossy(&stdout);
        let error_msg = format!(
            "effector agent setup failed (exit={}): {}",
            exit_code.unwrap_or(-1),
            output
        );

        coordinator.set_agent_status(
            &pending.agent_id,
            AgentStatus::Failed {
                error: error_msg.clone(),
            },
        );

        respond_error(coordinator, &pending.request_id, "setup_failed", &error_msg);
        return;
    }

    // Setup succeeded - open command pane
    coordinator.set_agent_status(&pending.agent_id, AgentStatus::PaneOpening);

    let agent_command = context.get("agent_command").cloned().unwrap_or_default();
    let worktree_path = context.get("worktree_path").cloned().unwrap_or_default();
    let agent_id = context.get("agent_id").cloned().unwrap_or_default();

    // Store pending command for pane open step
    let pane_ctx_key = format!("pane:{}:{}", pending.request_id, agent_id);
    coordinator.pending_commands.insert(
        pane_ctx_key.clone(),
        PendingCommand {
            request_id: pending.request_id.clone(),
            agent_id: agent_id.clone(),
            step: CommandStep::PaneOpen,
            created_at: coordinator.monotonic_clock,
        },
    );

    // Build context for correlating CommandPaneOpened
    let mut pane_context: BTreeMap<String, String> = BTreeMap::new();
    pane_context.insert("ctx_key".to_string(), pane_ctx_key);
    pane_context.insert("agent_id".to_string(), agent_id);
    pane_context.insert("request_id".to_string(), pending.request_id);

    // Determine shell for login environment
    let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".to_string());

    // Open command pane with the agent CLI command
    open_command_pane(
        CommandToRun {
            path: PathBuf::from(shell),
            args: vec!["-lc".to_string(), agent_command],
            cwd: Some(PathBuf::from(worktree_path)),
        },
        pane_context,
    );
}

/// Handle CommandPaneOpened event.
///
/// Records pane_id, transitions to Running, responds to sidecar.
pub fn handle_pane_opened(
    coordinator: &mut Coordinator,
    terminal_pane_id: u32,
    context: BTreeMap<String, String>,
) {
    let ctx_key = match context.get("ctx_key") {
        Some(k) => k.clone(),
        None => return,
    };

    let pending = match coordinator.pending_commands.remove(&ctx_key) {
        Some(p) => p,
        None => return,
    };

    if pending.step != CommandStep::PaneOpen {
        return;
    }

    // Transition to Running
    coordinator.set_agent_status(
        &pending.agent_id,
        AgentStatus::Running {
            pane_id: terminal_pane_id,
        },
    );
    coordinator
        .pane_to_agent
        .insert(terminal_pane_id, pending.agent_id.clone());

    // Build success response
    let agent = coordinator.get_agent(&pending.agent_id);
    let result = serde_json::json!({
        "agent_id": pending.agent_id,
        "pane_id": terminal_pane_id,
        "worktree_path": agent.map(|a| a.worktree_path.as_str()).unwrap_or(""),
        "branch": agent.map(|a| a.branch.as_str()).unwrap_or(""),
    });

    respond_ok(coordinator, &pending.request_id, result);

    // Push state update to UI plugin
    push_state_to_ui(coordinator);
}

/// Handle CommandPaneExited event.
///
/// Transitions agent to Completed state.
pub fn handle_pane_exited(
    coordinator: &mut Coordinator,
    terminal_pane_id: u32,
    exit_code: Option<i32>,
) {
    let agent_id = match coordinator.agent_for_pane(terminal_pane_id) {
        Some(id) => id.to_string(),
        None => return,
    };

    coordinator.set_agent_status(
        &agent_id,
        AgentStatus::Completed {
            exit_code: exit_code.unwrap_or(-1),
        },
    );

    // Push state update to UI plugin
    push_state_to_ui(coordinator);
}

// ============================================================================
// Response helpers
// ============================================================================

fn respond_ok(coordinator: &mut Coordinator, request_id: &str, result: serde_json::Value) {
    let response = CoordinatorResponse::ok(request_id, result);
    if let Some(pipe_name) = coordinator.pending_pipes.remove(request_id) {
        let json = serde_json::to_string(&response).unwrap_or_default();
        cli_pipe_output(&pipe_name, &json);
        unblock_cli_pipe_input(&pipe_name);
    }
}

fn respond_error(coordinator: &mut Coordinator, request_id: &str, code: &str, message: &str) {
    let response = CoordinatorResponse::error(request_id, code, message);
    if let Some(pipe_name) = coordinator.pending_pipes.remove(request_id) {
        let json = serde_json::to_string(&response).unwrap_or_default();
        cli_pipe_output(&pipe_name, &json);
        unblock_cli_pipe_input(&pipe_name);
    }
}

/// Push full agent state to the UI plugin via pipe_message_to_plugin.
fn push_state_to_ui(coordinator: &Coordinator) {
    let update = StateUpdate::FullState {
        agents: coordinator.to_coordinator_states(),
    };

    if let Ok(json) = serde_json::to_string(&update) {
        pipe_message_to_plugin(
            MessageToPlugin::new("exomonad-coordinator-state")
                .with_plugin_url("file:~/.config/zellij/plugins/exomonad-plugin.wasm")
                .with_payload(json),
        );
    }
}
