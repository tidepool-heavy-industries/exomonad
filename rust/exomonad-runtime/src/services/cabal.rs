use crate::services::docker::DockerExecutor;
use anyhow::Result;
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

// Types

#[derive(Serialize, Deserialize)]
struct GHCError {
    file: String,
    line: i32,
    message: String,
}

#[derive(Serialize)]
#[serde(tag = "tag", content = "contents")]
pub enum BuildResult {
    BuildSuccess,
    BuildFailed(Vec<GHCError>),
}

#[derive(Serialize, Deserialize)]
struct TestFailure {
    name: String,
    message: String,
}

#[derive(Serialize)]
#[serde(tag = "tag", content = "contents")]
pub enum TestResult {
    TestsPassed(i32),
    TestsFailed(Vec<TestFailure>),
    NoTests,
}

#[derive(Clone)]
pub struct CabalService {
    docker: Arc<dyn DockerExecutor>,
}

impl CabalService {
    pub fn new(docker: Arc<dyn DockerExecutor>) -> Self {
        Self { docker }
    }

    pub async fn build(&self, container_id: &str, path: &str) -> Result<BuildResult> {
        let cmd = vec!["cabal", "build"];
        let res = self.docker.exec(container_id, path, &cmd).await;

        match res {
            Ok(_) => Ok(BuildResult::BuildSuccess),
            Err(e) => {
                // TODO: Implement real GHC error parsing
                let msg = e.to_string();
                let stderr = msg.strip_prefix("Docker exec failed: ").unwrap_or(&msg);
                let errs = vec![GHCError {
                    file: "build.log".to_string(),
                    line: 0,
                    message: stderr.to_string(),
                }];
                Ok(BuildResult::BuildFailed(errs))
            }
        }
    }

    pub async fn test(&self, container_id: &str, path: &str) -> Result<TestResult> {
        let cmd = vec!["cabal", "test"];
        let res = self.docker.exec(container_id, path, &cmd).await;

        match res {
            Ok(_) => Ok(TestResult::TestsPassed(1)), // TODO: Parse count
            Err(e) => {
                let msg = e.to_string();
                let stderr = msg.strip_prefix("Docker exec failed: ").unwrap_or(&msg);

                if stderr.contains("no test suites") {
                    Ok(TestResult::NoTests)
                } else {
                    Ok(TestResult::TestsFailed(vec![TestFailure {
                        name: "test-suite".to_string(),
                        message: stderr.to_string(),
                    }]))
                }
            }
        }
    }
}

// Host Functions

#[derive(Deserialize)]
struct CabalInput {
    path: String,
    #[serde(rename = "containerId")]
    container_id: String,
}

#[derive(Serialize)]
#[serde(tag = "kind", content = "payload")]
enum CabalResult<T> {
    Success(T),
    Error(CabalError),
}

#[derive(Serialize)]
struct CabalError {
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

fn block_on<F: std::future::Future>(future: F) -> Result<F::Output, Error> {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) => Ok(handle.block_on(future)),
        Err(_) => Err(Error::msg("No Tokio runtime available")),
    }
}

pub fn register_host_functions(service: Arc<CabalService>) -> Vec<Function> {
    vec![
        Function::new(
            "cabal_build",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            cabal_build,
        )
        .with_namespace("env"),
        Function::new(
            "cabal_test",
            [ValType::I64],
            [ValType::I64],
            UserData::new(service.clone()),
            cabal_test,
        )
        .with_namespace("env"),
    ]
}

fn cabal_build(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<CabalService>>,
) -> Result<(), Error> {
    let input: CabalInput = get_input(plugin, inputs[0].clone())?;
    let service_lock = user_data.get()?;
    let service = service_lock.lock().unwrap();

    let res = block_on(service.build(&input.container_id, &input.path))?;
    let output = match res {
        Ok(r) => CabalResult::Success(r),
        Err(e) => CabalResult::Error(CabalError {
            message: e.to_string(),
        }),
    };

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}

fn cabal_test(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<Arc<CabalService>>,
) -> Result<(), Error> {
    let input: CabalInput = get_input(plugin, inputs[0].clone())?;
    let service_lock = user_data.get()?;
    let service = service_lock.lock().unwrap();

    let res = block_on(service.test(&input.container_id, &input.path))?;
    let output = match res {
        Ok(r) => CabalResult::Success(r),
        Err(e) => CabalResult::Error(CabalError {
            message: e.to_string(),
        }),
    };

    outputs[0] = set_output(plugin, &output)?;
    Ok(())
}
