use exomonad_core::effects::EffectRegistry;
use exomonad_core::services::{AgentResolver, EventLog};
use exomonad_core::{AgentName, PluginManager, Role};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Clone)]
pub struct AppState {
    pub plugins: Arc<RwLock<HashMap<AgentName, Arc<PluginManager>>>>,
    pub registry: Arc<EffectRegistry>,
    pub wasm_path: PathBuf,
    pub wasm_dir: PathBuf,
    pub wasm_name: String,
    pub default_role: Role,
    pub worktree_base: PathBuf,
    pub event_log: Option<Arc<EventLog>>,
    pub run_id: Arc<str>,
    pub agent_resolver: Arc<AgentResolver>,
}
