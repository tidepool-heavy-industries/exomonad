use bollard::exec::{CreateExecOptions, StartExecResults};
use bollard::Docker;
use futures_util::stream::StreamExt;
use serde::Serialize;

/// JSON response from docker-ctl exec
/// IMPORTANT: Field names must match Haskell ExecResult in SshExec.hs
#[derive(Serialize)]
pub struct ExecResponse {
    #[serde(rename = "exit_code")]
    pub exit_code: Option<i64>,
    #[serde(rename = "stdout")]
    pub stdout: String,
    #[serde(rename = "stderr")]
    pub stderr: String,
}

pub async fn run(
    container: Option<String>,
    local: bool,
    workdir: Option<String>,
    user: Option<String>,
    env: Vec<String>,
    cmd: Vec<String>,
) -> anyhow::Result<String> {
    if local || container.is_none() {
        run_local(workdir, env, cmd)
    } else {
        run_in_container(container.unwrap(), workdir, user, env, cmd).await
    }
}

fn run_local(workdir: Option<String>, env: Vec<String>, cmd: Vec<String>) -> anyhow::Result<String> {
    use std::process::Command;

    if cmd.is_empty() {
        return Err(anyhow::anyhow!("No command provided"));
    }

    let mut command = Command::new(&cmd[0]);
    command.args(&cmd[1..]);

    if let Some(dir) = workdir {
        command.current_dir(dir);
    }

    for e in env {
        if let Some((k, v)) = e.split_once('=') {
            command.env(k, v);
        }
    }

    let output = command.output()?;

    Ok(serde_json::to_string(&ExecResponse {
        exit_code: Some(output.status.code().unwrap_or(-1) as i64),
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
    })?)
}

async fn run_in_container(
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
