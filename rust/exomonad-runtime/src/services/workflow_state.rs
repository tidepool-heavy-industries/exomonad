use anyhow::Result;
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

// Service

#[derive(Clone)]
pub struct WorkflowStateService {
    attempts: Arc<Mutex<HashMap<String, i32>>>,
}

impl WorkflowStateService {
    pub fn new() -> Self {
        Self {
            attempts: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn get_attempts(&self, agent_id: &str) -> i32 {
        let map = self.attempts.lock().unwrap();
        *map.get(agent_id).unwrap_or(&0)
    }

    pub fn increment_attempts(&self, agent_id: &str) {
        let mut map = self.attempts.lock().unwrap();
        let count = map.entry(agent_id.to_string()).or_insert(0);
        *count += 1;
    }

    pub fn reset_attempts(&self, agent_id: &str) {
        let mut map = self.attempts.lock().unwrap();
        map.remove(agent_id);
    }
}

impl Default for WorkflowStateService {
    fn default() -> Self {
        Self::new()
    }
}

// Host Functions

#[derive(Deserialize)]
struct AgentIdInput {
    #[serde(rename = "agentId")]
    agent_id: String,
}

#[derive(Serialize)]
#[serde(tag = "kind", content = "payload")]
enum StateResult<T> {
    Success(T),
    Error(StateError),
}

#[derive(Serialize)]
struct StateError {
    message: String,
}

fn get_input<T: serde::de::DeserializeOwned>(
    plugin: &mut CurrentPlugin,
    val: Val,
) -> Result<T, Error> {
    let handle = plugin
        .memory_from_val(&val)
        .ok_or_else(|| Error::msg("Invalid memory handle in input"))?;
    let bytes = plugin.memory_bytes(handle)?;
    Ok(serde_json::from_slice(bytes)?)
}

fn set_output<T: Serialize>(plugin: &mut CurrentPlugin, data: &T) -> Result<Val, Error> {
    let json = serde_json::to_vec(data)?;
    let handle = plugin.memory_new(json)?;
    Ok(plugin.memory_to_val(handle))
}

pub fn register_host_functions(service: Arc<WorkflowStateService>) -> Vec<Function> {
    vec![
        Function::new(
            "workflow_state_get_attempts",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            get_attempts,
        )
        .with_namespace("env"),
        Function::new(
            "workflow_state_increment_attempts",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            increment_attempts,
        )
        .with_namespace("env"),
        Function::new(
            "workflow_state_reset_attempts",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service),
            reset_attempts,
        )
        .with_namespace("env"),
    ]
}

fn get_attempts(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<WorkflowStateService>>,
) -> Result<(), Error> {
    let input: AgentIdInput = get_input(plugin, inputs[0].clone())?;
    let service = user_data.get()?;
    let attempts = service.lock().unwrap().get_attempts(&input.agent_id);
    
    let output = StateResult::Success(attempts);
    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

fn increment_attempts(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<WorkflowStateService>>,
) -> Result<(), Error> {
    let input: AgentIdInput = get_input(plugin, inputs[0].clone())?;
    let service = user_data.get()?;
    service.lock().unwrap().increment_attempts(&input.agent_id);
    
    let output = StateResult::<()>::Success(());
    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

fn reset_attempts(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<WorkflowStateService>>,
) -> Result<(), Error> {
    let input: AgentIdInput = get_input(plugin, inputs[0].clone())?;
    let service = user_data.get()?;
    service.lock().unwrap().reset_attempts(&input.agent_id);
    
    let output = StateResult::<()>::Success(());
    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}
