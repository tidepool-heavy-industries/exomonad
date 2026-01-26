use bollard::container::{Config, CreateContainerOptions, StartContainerOptions, RemoveContainerOptions, InspectContainerOptions};
use bollard::models::HostConfig;
use bollard::Docker;
use serde::Serialize;
use std::collections::HashMap;
use std::path::Path;

use crate::domain::AgentVolumes;

#[derive(Serialize)]
pub struct SpawnResponse {
    pub container_id: String,
    pub hostname: String,
    pub reused: bool,
}

pub async fn run(
    issue_id: String,
    worktree_path: String,
    backend: String,
    uid: Option<u32>,
    gid: Option<u32>,
    expires_at: Option<String>,
    env_vars: Vec<String>,
) -> anyhow::Result<String> {
    let docker = Docker::connect_with_local_defaults()?;
    let container_name = format!("exomonad-agent-{}", issue_id);

    // Idempotency: check if container already exists
    match docker.inspect_container(&container_name, None::<InspectContainerOptions>).await {
        Ok(info) => {
            // Container exists - check if it's running
            let state = info.state.as_ref();
            let is_running = state.and_then(|s| s.running).unwrap_or(false);

            if is_running {
                // Already running, just return its info
                let container_id = info.id.unwrap_or_else(|| container_name.clone());
                return Ok(serde_json::to_string(&SpawnResponse {
                    container_id,
                    hostname: container_name,
                    reused: true,
                })?);
            } else {
                // Stopped - remove it and create fresh (config might have changed)
                docker.remove_container(
                    &container_name,
                    Some(RemoveContainerOptions { force: true, ..Default::default() })
                ).await?;
            }
        }
        Err(bollard::errors::Error::DockerResponseServerError { status_code: 404, .. }) => {
            // Container doesn't exist, proceed to create
        }
        Err(e) => return Err(e.into()),
    }

    let agent_image = std::env::var("EXOMONAD_AGENT_IMAGE").unwrap_or_else(|_| "exomonad-agent:latest".to_string());
    let host_uid: u32 = std::env::var("HOST_UID").unwrap_or_else(|_| "1000".to_string()).parse()?;
    let host_gid: u32 = std::env::var("HOST_GID").unwrap_or_else(|_| "1000".to_string()).parse()?;
    let network_name = std::env::var("EXOMONAD_NETWORK").unwrap_or_else(|_| "exomonad".to_string());

    let mut labels = HashMap::new();
    labels.insert("com.exomonad.issue_id".to_string(), issue_id.clone());
    labels.insert("com.exomonad.role".to_string(), "agent".to_string());
    labels.insert(
        "com.exomonad.expires_at".to_string(),
        expires_at.unwrap_or_else(|| "never".to_string())
    );

    // Extract the worktree directory name from the full path (e.g., "gh-346-test-issue")
    let worktree_dir = Path::new(&worktree_path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(&issue_id);

    // The working directory inside the container
    let working_dir = format!("/worktrees/{}", worktree_dir);

    // Volume mounts from domain model (source of truth for spawned agents)
    let volumes = AgentVolumes::default();
    let mounts = volumes.to_mounts();

    let user_uid = uid.unwrap_or(host_uid);
    let user_gid = gid.unwrap_or(host_gid);
    let user = format!("{}:{}", user_uid, user_gid);

    // Detached Interactive TTY pattern (-dit):
    // - tty + open_stdin: allocate TTY and keep stdin open
    // - attach_* = false: don't expect immediate client attachment
    // This lets the container run in background, attachable later via `docker attach`
    let config = Config {
        image: Some(agent_image),
        labels: Some(labels),
        working_dir: Some(working_dir),
        tty: Some(true),
        open_stdin: Some(true),
        attach_stdin: Some(false),
        attach_stdout: Some(false),
        attach_stderr: Some(false),
        host_config: Some(HostConfig {
            mounts: Some(mounts),
            network_mode: Some(network_name),
            ..Default::default()
        }),
        user: Some(user),
        env: Some({
            let mut all_env = vec![
                format!("EXOMONAD_ISSUE_ID={}", issue_id),
                format!("EXOMONAD_BACKEND={}", backend),
            ];
            // Add user-provided env vars (already in KEY=VALUE format)
            all_env.extend(env_vars);
            all_env
        }),
        ..Default::default()
    };

    let create_options = CreateContainerOptions {
        name: container_name.clone(),
        ..Default::default()
    };

    let container = docker.create_container(Some(create_options), config).await?;
    docker.start_container(&container.id, None::<StartContainerOptions<String>>).await?;

    Ok(serde_json::to_string(&SpawnResponse {
        container_id: container.id,
        hostname: container_name,
        reused: false,
    })?)
}
