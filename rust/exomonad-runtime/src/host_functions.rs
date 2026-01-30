use crate::services::docker::DockerService;
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::info;

// --- DTOs ---

#[derive(Deserialize)]
struct ExecInput {
    #[serde(rename = "containerId")]
    container_id: String,
    command: Vec<String>,
    #[serde(rename = "workingDir")]
    working_dir: Option<String>,
}

#[derive(Serialize)]
#[serde(tag = "kind", content = "payload")]
enum DockerResult<T> {
    Success(T),
    Error(ErrorMessage),
}

#[derive(Serialize)]
struct ErrorMessage {
    message: String,
}

#[derive(Serialize)]
struct ExecSuccess {
    stdout: String,
    stderr: String,
    #[serde(rename = "exitCode")]
    exit_code: i32,
}

#[derive(Deserialize)]
struct SpawnInput {
    image: String,
    name: String,
    env: HashMap<String, String>,
}

#[derive(Deserialize)]
struct KillInput {
    #[serde(rename = "containerId")]
    container_id: String,
}

// --- Helper to read/write JSON ---

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
    // memory_new expects T: ToBytes. Vec<u8> implements ToBytes.
    let handle = plugin.memory_new(json)?;
    Ok(plugin.memory_to_val(handle))
}

// --- Implementation ---

pub fn docker_functions(service: DockerService) -> Vec<Function> {
    vec![
        Function::new(
            "docker_exec",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            docker_exec,
        ),
        Function::new(
            "docker_spawn",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            docker_spawn,
        ),
        Function::new(
            "docker_kill",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service),
            docker_kill,
        ),
    ]
}

fn block_on<F: std::future::Future>(future: F) -> Result<F::Output, Error> {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => Ok(handle.block_on(future)),
        Err(_) => Err(Error::msg("No Tokio runtime available for async Docker operation")),
    }
}

fn docker_exec(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<DockerService>,
) -> Result<(), Error> {
    // Input is a MemoryHandle (passed as I64)
    let input: ExecInput = get_input(plugin, inputs[0].clone())?;

    let service_arc = user_data.get()?;
    let service = service_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;
    let cmd_strs: Vec<&str> = input.command.iter().map(|s| s.as_str()).collect();

    // Block on async execution
    let result = block_on(service.exec(
        &input.container_id,
        &cmd_strs,
        input.working_dir.as_deref(),
    ))?;

    let output_data = match result {
        Ok(out) => DockerResult::Success(ExecSuccess {
            stdout: out.stdout,
            stderr: out.stderr,
            exit_code: out.exit_code,
        }),
        Err(e) => DockerResult::<ExecSuccess>::Error(ErrorMessage {
            message: e.to_string(),
        }),
    };

    outputs[0] = set_output(plugin, &output_data)?;
    Ok(())
}

fn docker_spawn(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<DockerService>,
) -> Result<(), Error> {
    let input: SpawnInput = get_input(plugin, inputs[0].clone())?;

    let service_arc = user_data.get()?;
    let service = service_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;
    
    let result = block_on(service.spawn(
        &input.image,
        &input.name,
        &input.env,
    ))?;

    let output_data = match result {
        Ok(id) => DockerResult::Success(id),
        Err(e) => DockerResult::<String>::Error(ErrorMessage {
            message: e.to_string(),
        }),
    };

    outputs[0] = set_output(plugin, &output_data)?;
    Ok(())
}

fn docker_kill(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<DockerService>,
) -> Result<(), Error> {
    let input: KillInput = get_input(plugin, inputs[0].clone())?;

    let service_arc = user_data.get()?;
    let service = service_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;
    
    let result = block_on(service.kill(&input.container_id))?;

    let output_data = match result {
        Ok(_) => DockerResult::Success(()),
        Err(e) => DockerResult::<()>::Error(ErrorMessage {
            message: e.to_string(),
        }),
    };

    outputs[0] = set_output(plugin, &output_data)?;
    Ok(())
}

// --- Legacy / Existing Functions ---

pub fn git_get_branch(
    _plugin: &mut CurrentPlugin,
    _inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    info!("Host function called: git_get_branch");
    if outputs.is_empty() {
        return Err(Error::msg(
            "git_get_branch: expected at least one output value",
        ));
    }
    outputs[0] = Val::I64(0);
    Ok(())
}

pub fn log_info(
    _plugin: &mut CurrentPlugin,
    inputs: &[Val],
    _outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    if inputs.len() < 1 {
        return Err(Error::msg(
            "log_info: expected at least 1 input argument, got 0",
        ));
    }
    // Updated to use unwrap_i64() for Extism 1.13 compatibility
    let _offset = inputs[0].unwrap_i64(); 
    // let len = inputs[1].unwrap_i64();

    info!("Host function called: log_info");
    Ok(())
}