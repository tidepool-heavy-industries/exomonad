use bollard::exec::{CreateExecOptions, StartExecResults};
use bollard::Docker;
use futures_util::stream::StreamExt;
use serde::Serialize;

#[derive(Serialize)]
pub struct ExecResponse {
    pub exit_code: Option<i64>,
    pub stdout: String,
    pub stderr: String,
}

pub async fn run(
    container: String,
    workdir: Option<String>,
    user: Option<String>,
    env: Vec<String>,
    cmd: Vec<String>,
) -> anyhow::Result<String> {
    let docker = Docker::connect_with_local_defaults()?;
    
    let exec_options = CreateExecOptions {
        cmd: Some(cmd),
        attach_stdout: Some(true),
        attach_stderr: Some(true),
        working_dir: workdir,
        user: user,
        env: Some(env),
        ..Default::default()
    };

    let exec = docker.create_exec(&container, exec_options).await?;
    let start_result = docker.start_exec(&exec.id, None).await?;

    let mut stdout = Vec::new();
    let mut stderr = Vec::new();

    if let StartExecResults::Attached { mut output, .. } = start_result {
        while let Some(chunk) = output.next().await {
            match chunk {
                Ok(bollard::container::LogOutput::StdOut { message }) => {
                    stdout.extend_from_slice(&message);
                }
                Ok(bollard::container::LogOutput::StdErr { message }) => {
                    stderr.extend_from_slice(&message);
                }
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Error reading exec output: {}", e);
                }
            }
        }
    }

    let inspect = docker.inspect_exec(&exec.id).await?;
    let exit_code = inspect.exit_code;

    Ok(serde_json::to_string(&ExecResponse {
        exit_code,
        stdout: String::from_utf8_lossy(&stdout).to_string(),
        stderr: String::from_utf8_lossy(&stderr).to_string(),
    })?)
}
