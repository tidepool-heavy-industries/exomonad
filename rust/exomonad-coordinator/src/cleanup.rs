//! Cleanup workflow: close pane -> effector teardown -> remove from state.
//!
//! Handles single and batch agent cleanup.

use crate::protocol::CoordinatorResponse;
use crate::state::{AgentStatus, CommandStep, Coordinator, PendingCommand};
use exomonad_ui_protocol::StateUpdate;
use std::collections::BTreeMap;
use zellij_tile::prelude::*;

/// Initiate cleanup for one or more agents.
///
/// For each agent:
/// 1. If Running, close the pane
/// 2. Run effector agent teardown
/// 3. Remove from state on completion
pub fn handle_cleanup(
    coordinator: &mut Coordinator,
    pipe_name: &str,
    request_id: String,
    agent_ids: Vec<String>,
    force: bool,
) {
    // Block pipe for async response
    block_cli_pipe_input(pipe_name);
    coordinator
        .pending_pipes
        .insert(request_id.clone(), pipe_name.to_string());

    if agent_ids.is_empty() {
        respond_ok(
            coordinator,
            &request_id,
            serde_json::json!({ "cleaned": [] }),
        );
        return;
    }

    for agent_id in &agent_ids {
        let agent = match coordinator.get_agent(agent_id) {
            Some(a) => a.clone(),
            None => continue,
        };

        // Close pane if running
        if let AgentStatus::Running { pane_id } = agent.status {
            close_terminal_pane(pane_id);
        }

        coordinator.set_agent_status(agent_id, AgentStatus::Cleaning);

        // Build context for correlating teardown result
        let ctx_key = format!("teardown:{}:{}", request_id, agent_id);
        coordinator.pending_commands.insert(
            ctx_key.clone(),
            PendingCommand {
                request_id: request_id.clone(),
                agent_id: agent_id.clone(),
                step: CommandStep::Teardown,
                created_at: coordinator.monotonic_clock,
            },
        );

        let mut context: BTreeMap<String, String> = BTreeMap::new();
        context.insert("ctx_key".to_string(), ctx_key);
        context.insert("agent_id".to_string(), agent_id.clone());
        context.insert("request_id".to_string(), request_id.clone());

        // Run effector teardown
        // project_dir is derived from worktree_path (3 levels up)
        let project_dir = derive_project_dir(&agent.worktree_path);
        let mut cmd: Vec<String> = vec![
            "effector".to_string(),
            "agent".to_string(),
            "teardown".to_string(),
            "--project-dir".to_string(),
            project_dir,
            "--worktree-path".to_string(),
            agent.worktree_path.clone(),
        ];
        if force {
            cmd.push("--force".to_string());
        }
        let cmd_refs: Vec<&str> = cmd.iter().map(|s| s.as_str()).collect();

        run_command(&cmd_refs, context);
    }
}

/// Handle RunCommandResult for teardown step.
pub fn handle_teardown_result(
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

    if pending.step != CommandStep::Teardown {
        return;
    }

    let success = exit_code.map(|c| c == 0).unwrap_or(false);

    if success {
        coordinator.remove_agent(&pending.agent_id);
    } else {
        let output = String::from_utf8_lossy(&stdout);
        coordinator.set_agent_status(
            &pending.agent_id,
            AgentStatus::Failed {
                error: format!("teardown failed: {}", output),
            },
        );
    }

    // Check if all teardowns for this request are complete
    let request_id = &pending.request_id;
    let still_pending = coordinator
        .pending_commands
        .values()
        .any(|cmd| cmd.request_id == *request_id && cmd.step == CommandStep::Teardown);

    if !still_pending {
        // All teardowns done, respond
        let cleaned: Vec<String> = coordinator
            .agents
            .values()
            .filter(|a| !matches!(a.status, AgentStatus::Cleaning))
            .map(|a| a.id.clone())
            .collect();

        respond_ok(
            coordinator,
            request_id,
            serde_json::json!({ "cleaned": cleaned }),
        );

        // Push state update to UI plugin
        push_state_to_ui(coordinator);
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Derive project directory from worktree path.
///
/// Worktree path: {project}/.exomonad/worktrees/gh-xxx/
/// Project dir: 3 levels up from worktree.
fn derive_project_dir(worktree_path: &str) -> String {
    let path = std::path::Path::new(worktree_path);
    path.ancestors()
        .nth(3)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|| ".".to_string())
}

fn respond_ok(coordinator: &mut Coordinator, request_id: &str, result: serde_json::Value) {
    let response = CoordinatorResponse::ok(request_id, result);
    if let Some(pipe_name) = coordinator.pending_pipes.remove(request_id) {
        let json = serde_json::to_string(&response).unwrap_or_default();
        cli_pipe_output(&pipe_name, &json);
        unblock_cli_pipe_input(&pipe_name);
    }
}

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
