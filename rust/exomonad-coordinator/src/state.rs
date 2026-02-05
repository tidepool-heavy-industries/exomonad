//! Agent state management.
//!
//! Tracks all active agents and their lifecycle status.

use exomonad_ui_protocol::{CoordinatorAgentState, CoordinatorAgentStatus};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Lifecycle status of an agent.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "phase")]
pub enum AgentStatus {
    /// Effector running setup (worktree + config).
    #[serde(rename = "setting_up")]
    SettingUp,
    /// Command pane opening, waiting for CommandPaneOpened.
    #[serde(rename = "pane_opening")]
    PaneOpening,
    /// Agent running in pane.
    #[serde(rename = "running")]
    Running { pane_id: u32 },
    /// Cleanup in progress.
    #[serde(rename = "cleaning")]
    Cleaning,
    /// Agent exited normally.
    #[serde(rename = "completed")]
    Completed { exit_code: i32 },
    /// Agent failed during setup or execution.
    #[serde(rename = "failed")]
    Failed { error: String },
}

/// Complete state of a single agent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentState {
    pub id: String,
    pub worktree_path: String,
    pub branch: String,
    pub agent_type: String,
    pub status: AgentStatus,
}

/// Top-level coordinator state.
pub struct Coordinator {
    /// All tracked agents.
    pub agents: BTreeMap<String, AgentState>,
    /// Pending pipe requests: request_id -> pipe_name.
    pub pending_pipes: BTreeMap<String, String>,
    /// Map command context -> (request_id, agent_id, step).
    /// Used to correlate RunCommandResult/CommandPaneOpened back to requests.
    pub pending_commands: BTreeMap<String, PendingCommand>,
    /// Map pane_id -> agent_id for lifecycle events.
    pub pane_to_agent: BTreeMap<u32, String>,
    /// Monotonic clock accumulated from Timer events (seconds).
    pub monotonic_clock: f64,
}

/// Tracks a pending async command (effector setup or pane open).
#[derive(Debug, Clone)]
pub struct PendingCommand {
    pub request_id: String,
    pub agent_id: String,
    pub step: CommandStep,
    /// Monotonic clock value when this command was created.
    pub created_at: f64,
}

/// Which step of the workflow we're waiting on.
#[derive(Debug, Clone, PartialEq)]
pub enum CommandStep {
    /// Waiting for effector agent setup to complete.
    Setup,
    /// Waiting for command pane to open.
    PaneOpen,
    /// Waiting for teardown to complete.
    Teardown,
}

impl Coordinator {
    pub fn new() -> Self {
        Self {
            agents: BTreeMap::new(),
            pending_pipes: BTreeMap::new(),
            pending_commands: BTreeMap::new(),
            pane_to_agent: BTreeMap::new(),
            monotonic_clock: 0.0,
        }
    }

    /// Get agent state by ID.
    pub fn get_agent(&self, id: &str) -> Option<&AgentState> {
        self.agents.get(id)
    }

    /// Update agent status.
    pub fn set_agent_status(&mut self, id: &str, status: AgentStatus) {
        if let Some(agent) = self.agents.get_mut(id) {
            agent.status = status;
        }
    }

    /// Register a new agent in SettingUp state.
    pub fn register_agent(
        &mut self,
        id: String,
        worktree_path: String,
        branch: String,
        agent_type: String,
    ) {
        self.agents.insert(
            id.clone(),
            AgentState {
                id,
                worktree_path,
                branch,
                agent_type,
                status: AgentStatus::SettingUp,
            },
        );
    }

    /// Remove an agent from tracking.
    pub fn remove_agent(&mut self, id: &str) -> Option<AgentState> {
        let agent = self.agents.remove(id);
        // Clean up pane mapping
        self.pane_to_agent.retain(|_, v| v != id);
        agent
    }

    /// Look up which agent owns a pane.
    pub fn agent_for_pane(&self, pane_id: u32) -> Option<&str> {
        self.pane_to_agent.get(&pane_id).map(|s| s.as_str())
    }

    /// Collect all agent states as a serializable list.
    pub fn list_agents(&self) -> Vec<&AgentState> {
        self.agents.values().collect()
    }

    /// Convert all agent states to protocol types for UI push.
    pub fn to_coordinator_states(&self) -> Vec<CoordinatorAgentState> {
        self.agents
            .values()
            .map(|a| a.to_coordinator_state())
            .collect()
    }
}

impl AgentStatus {
    /// Convert to protocol type for UI plugin communication.
    pub fn to_coordinator_status(&self) -> CoordinatorAgentStatus {
        match self {
            AgentStatus::SettingUp => CoordinatorAgentStatus::SettingUp,
            AgentStatus::PaneOpening => CoordinatorAgentStatus::PaneOpening,
            AgentStatus::Running { pane_id } => {
                CoordinatorAgentStatus::Running { pane_id: *pane_id }
            }
            AgentStatus::Cleaning => CoordinatorAgentStatus::Cleaning,
            AgentStatus::Completed { exit_code } => CoordinatorAgentStatus::Completed {
                exit_code: *exit_code,
            },
            AgentStatus::Failed { error } => CoordinatorAgentStatus::Failed {
                error: error.clone(),
            },
        }
    }
}

impl AgentState {
    /// Convert to protocol type for UI plugin communication.
    pub fn to_coordinator_state(&self) -> CoordinatorAgentState {
        CoordinatorAgentState {
            id: self.id.clone(),
            worktree_path: self.worktree_path.clone(),
            branch: self.branch.clone(),
            agent_type: self.agent_type.clone(),
            status: self.status.to_coordinator_status(),
        }
    }
}
