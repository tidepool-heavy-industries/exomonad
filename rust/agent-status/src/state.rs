use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentStatus {
    pub id: String,
    pub container_id: String,
    pub issue_number: Option<i32>,
    pub status: String,
    pub started_at: String,
    pub last_activity: Option<String>,
    pub last_action: Option<String>,
    pub blocker: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DashboardState {
    pub agents: Vec<AgentStatus>,
    pub connected: bool,
    pub last_updated: Option<String>,
    
    // UI State
    pub selected_index: usize,
    pub logs_cache: HashMap<String, String>,
}