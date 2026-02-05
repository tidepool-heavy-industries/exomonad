//! Coordinator Plugin: Zellij WASM plugin for agent lifecycle orchestration.
//!
//! Receives requests via `zellij pipe`, manages async workflows through
//! Zellij's event-driven API (RunCommandResult, CommandPaneOpened, etc.),
//! and pushes state updates to the UI plugin.
//!
//! This plugin has no UI rendering - it's a pure state machine.

use std::collections::BTreeMap;
use zellij_tile::prelude::*;

mod cleanup;
mod protocol;
mod spawn;
mod state;

use protocol::{CoordinatorRequest, CoordinatorResponse};
use state::{AgentStatus, CommandStep, Coordinator};

/// Timeout thresholds in seconds.
const SETUP_TIMEOUT_SECS: f64 = 120.0;
const PANE_OPEN_TIMEOUT_SECS: f64 = 60.0;
const TEARDOWN_TIMEOUT_SECS: f64 = 120.0;

struct CoordinatorPlugin {
    coordinator: Coordinator,
}

impl Default for CoordinatorPlugin {
    fn default() -> Self {
        Self {
            coordinator: Coordinator::new(),
        }
    }
}

register_plugin!(CoordinatorPlugin);

impl ZellijPlugin for CoordinatorPlugin {
    fn load(&mut self, _config: BTreeMap<String, String>) {
        subscribe(&[
            EventType::RunCommandResult,
            EventType::CommandPaneOpened,
            EventType::CommandPaneExited,
            EventType::Timer,
        ]);
        set_timeout(1.0);
    }

    fn pipe(&mut self, pipe_message: PipeMessage) -> bool {
        let payload = match pipe_message.payload {
            Some(ref p) => p.clone(),
            None => {
                let response = CoordinatorResponse::error(
                    "unknown",
                    "no_payload",
                    "Pipe message has no payload",
                );
                if let Ok(json) = serde_json::to_string(&response) {
                    cli_pipe_output(&pipe_message.name, &json);
                }
                return true;
            }
        };

        let pipe_name = pipe_message.name.clone();

        let request: CoordinatorRequest = match serde_json::from_str(&payload) {
            Ok(r) => r,
            Err(e) => {
                let response = CoordinatorResponse::error(
                    "unknown",
                    "parse_error",
                    &format!("Failed to parse request: {}", e),
                );
                if let Ok(json) = serde_json::to_string(&response) {
                    cli_pipe_output(&pipe_name, &json);
                }
                return true;
            }
        };

        match request {
            CoordinatorRequest::Spawn {
                request_id,
                agent_id,
                worktree_path,
                branch,
                agent_type,
                agent_command,
                setup_args,
            } => {
                spawn::handle_spawn(
                    &mut self.coordinator,
                    &pipe_name,
                    request_id,
                    agent_id,
                    worktree_path,
                    branch,
                    agent_type,
                    agent_command,
                    setup_args,
                );
            }
            CoordinatorRequest::Cleanup {
                request_id,
                agent_ids,
                force,
            } => {
                cleanup::handle_cleanup(
                    &mut self.coordinator,
                    &pipe_name,
                    request_id,
                    agent_ids,
                    force,
                );
            }
            CoordinatorRequest::List { request_id } => {
                let agents = self.coordinator.list_agents();
                let result = serde_json::json!({ "agents": agents });
                let response = CoordinatorResponse::ok(&request_id, result);
                if let Ok(json) = serde_json::to_string(&response) {
                    cli_pipe_output(&pipe_name, &json);
                }
            }
            CoordinatorRequest::Status {
                request_id,
                agent_id,
            } => {
                let response = match self.coordinator.get_agent(&agent_id) {
                    Some(agent) => CoordinatorResponse::ok(&request_id, serde_json::json!(agent)),
                    None => CoordinatorResponse::error(
                        &request_id,
                        "not_found",
                        &format!("Agent {} not found", agent_id),
                    ),
                };
                if let Ok(json) = serde_json::to_string(&response) {
                    cli_pipe_output(&pipe_name, &json);
                }
            }
        }

        true
    }

    fn update(&mut self, event: Event) -> bool {
        match event {
            Event::RunCommandResult(exit_code, stdout, stderr, context) => {
                // Determine which workflow step this belongs to
                if let Some(ctx_key) = context.get("ctx_key") {
                    if ctx_key.starts_with("spawn:") {
                        spawn::handle_setup_result(
                            &mut self.coordinator,
                            exit_code,
                            stdout,
                            stderr,
                            context,
                        );
                    } else if ctx_key.starts_with("teardown:") {
                        cleanup::handle_teardown_result(
                            &mut self.coordinator,
                            exit_code,
                            stdout,
                            stderr,
                            context,
                        );
                    }
                }
            }
            Event::CommandPaneOpened(terminal_pane_id, context) => {
                spawn::handle_pane_opened(&mut self.coordinator, terminal_pane_id, context);
            }
            Event::CommandPaneExited(terminal_pane_id, exit_code, _context) => {
                spawn::handle_pane_exited(&mut self.coordinator, terminal_pane_id, exit_code);
            }
            Event::Timer(elapsed) => {
                self.handle_timeout(elapsed);
            }
            _ => {}
        }

        // No rendering needed (headless plugin)
        false
    }

    fn render(&mut self, _rows: usize, _cols: usize) {
        // Coordinator has no UI - the UI plugin handles display
    }
}

impl CoordinatorPlugin {
    fn handle_timeout(&mut self, elapsed: f64) {
        self.coordinator.monotonic_clock += elapsed;
        let now = self.coordinator.monotonic_clock;

        // Collect timed-out commands
        let timed_out: Vec<(String, String, String, CommandStep)> = self
            .coordinator
            .pending_commands
            .iter()
            .filter(|(_, cmd)| {
                let threshold = match cmd.step {
                    CommandStep::Setup => SETUP_TIMEOUT_SECS,
                    CommandStep::PaneOpen => PANE_OPEN_TIMEOUT_SECS,
                    CommandStep::Teardown => TEARDOWN_TIMEOUT_SECS,
                };
                (now - cmd.created_at) > threshold
            })
            .map(|(key, cmd)| {
                (
                    key.clone(),
                    cmd.request_id.clone(),
                    cmd.agent_id.clone(),
                    cmd.step.clone(),
                )
            })
            .collect();

        for (ctx_key, request_id, agent_id, step) in timed_out {
            self.coordinator.pending_commands.remove(&ctx_key);

            let step_name = match step {
                CommandStep::Setup => "setup",
                CommandStep::PaneOpen => "pane_open",
                CommandStep::Teardown => "teardown",
            };
            let error_msg = format!(
                "Timed out waiting for {} (agent={}, elapsed={:.0}s)",
                step_name, agent_id, now,
            );

            self.coordinator.set_agent_status(
                &agent_id,
                AgentStatus::Failed {
                    error: error_msg.clone(),
                },
            );

            // Respond with error to unblock the pipe
            let response = CoordinatorResponse::error(&request_id, "timeout", &error_msg);
            if let Some(pipe_name) = self.coordinator.pending_pipes.remove(&request_id) {
                if let Ok(json) = serde_json::to_string(&response) {
                    cli_pipe_output(&pipe_name, &json);
                    unblock_cli_pipe_input(&pipe_name);
                }
            }
        }

        // Re-arm timer
        set_timeout(1.0);
    }
}
