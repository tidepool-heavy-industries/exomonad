use bollard::container::{Config, CreateContainerOptions, StartContainerOptions};
use bollard::models::{HostConfig, Mount, MountTypeEnum};
use bollard::Docker;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Serialize)]
pub struct SpawnResponse {
    pub container_id: String,
    pub hostname: String,
}

pub async fn run(
    issue_id: String,
    worktree_path: String,
    backend: String,
    uid: Option<u32>,
    gid: Option<u32>,
    expires_at: Option<String>,
) -> anyhow::Result<String> {
    let docker = Docker::connect_with_local_defaults()?;
    
    let agent_image = std::env::var("TIDEPOOL_AGENT_IMAGE").unwrap_or_else(|_| "tidepool-agent:latest".to_string());
    let host_uid: u32 = std::env::var("HOST_UID").unwrap_or_else(|_| "1000".to_string()).parse()?;
    let host_gid: u32 = std::env::var("HOST_GID").unwrap_or_else(|_| "1000".to_string()).parse()?;
    let network_name = std::env::var("TIDEPOOL_NETWORK").unwrap_or_else(|_| "tidepool".to_string());

    let container_name = format!("tidepool-agent-{}", issue_id);
    
    let mut labels = HashMap::new();
    labels.insert("com.tidepool.issue_id".to_string(), issue_id.clone());
    labels.insert("com.tidepool.role".to_string(), "agent".to_string());
    labels.insert(
        "com.tidepool.expires_at".to_string(), 
        expires_at.unwrap_or_else(|| "never".to_string())
    ); 

    let worktree_mount_target = format!("/worktrees/{}", issue_id);
    
    let mounts = vec![
        Mount {
            target: Some(worktree_mount_target.clone()),
            source: Some(worktree_path.clone()),
            typ: Some(MountTypeEnum::BIND),
            ..Default::default()
        },
        Mount {
            target: Some("/home/agent/.config/gh".to_string()),
            source: Some("tidepool-gh-auth".to_string()),
            typ: Some(MountTypeEnum::VOLUME),
            ..Default::default()
        }
    ];

    let user_uid = uid.unwrap_or(host_uid);
    let user_gid = gid.unwrap_or(host_gid);
    let user = format!("{}:{}", user_uid, user_gid);

    let config = Config {
        image: Some(agent_image),
        labels: Some(labels),
        host_config: Some(HostConfig {
            mounts: Some(mounts),
            network_mode: Some(network_name),
            ..Default::default()
        }),
        user: Some(user),
        env: Some(vec![
            format!("TIDEPOOL_ISSUE_ID={}", issue_id),
            format!("TIDEPOOL_BACKEND={}", backend),
        ]),
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
    })?)
}
