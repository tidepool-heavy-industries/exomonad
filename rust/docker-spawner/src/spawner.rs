use bollard::container::{Config, CreateContainerOptions, StartContainerOptions, StopContainerOptions, RemoveContainerOptions, InspectContainerOptions};
use bollard::exec::{CreateExecOptions, StartExecResults};
use bollard::Docker;
use bollard::models::{HostConfig, Mount, MountTypeEnum};
use futures_util::stream::StreamExt;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use tracing::{info, warn};

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct SpawnRequest {
    pub bead_id: String,
    pub worktree_path: PathBuf,
    pub backend: String,  // "claude" | "gemini"
    pub uid: Option<u32>,
    pub gid: Option<u32>,
    pub expires_at: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SpawnResponse {
    pub container_id: String,
    pub hostname: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StatusResponse {
    pub status: String,
}

#[derive(Debug, Deserialize)]
pub struct ExecRequest {
    pub cmd: Vec<String>,
    #[serde(default)]
    pub workdir: Option<String>,
    #[serde(default)]
    pub env: Option<Vec<String>>,
    #[serde(default)]
    pub user: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct ExecResponse {
    pub exit_code: Option<i64>,
    pub stdout: String,
    pub stderr: String,
}

pub struct Spawner {
    docker: Docker,
    agent_image: String,
    host_uid: u32,
    host_gid: u32,
    network_name: String,
}

impl Spawner {
    pub fn new() -> anyhow::Result<Self> {
        let docker = Docker::connect_with_local_defaults()?;
        let agent_image = std::env::var("TIDEPOOL_AGENT_IMAGE").unwrap_or_else(|_| "tidepool-agent:latest".to_string());
        let host_uid = std::env::var("HOST_UID").unwrap_or_else(|_| "1000".to_string()).parse()?;
        let host_gid = std::env::var("HOST_GID").unwrap_or_else(|_| "1000".to_string()).parse()?;
        let network_name = std::env::var("TIDEPOOL_NETWORK").unwrap_or_else(|_| "tidepool".to_string());

        Ok(Self {
            docker,
            agent_image,
            host_uid,
            host_gid,
            network_name,
        })
    }

    pub async fn spawn(&self, req: SpawnRequest) -> anyhow::Result<SpawnResponse> {
        let container_name = format!("tidepool-agent-{}", req.bead_id);
        let config = self.build_container_config(&req);

        info!("Creating container {} with image {}", container_name, self.agent_image);

        let create_options = CreateContainerOptions {
            name: container_name.clone(),
            ..Default::default()
        };

        let container = self.docker.create_container(Some(create_options), config).await?;
        
        info!("Starting container {}", container.id);
        self.docker.start_container(&container.id, None::<StartContainerOptions<String>>).await?;

        Ok(SpawnResponse {
            container_id: container.id,
            hostname: container_name,
        })
    }

    fn build_container_config(&self, req: &SpawnRequest) -> Config<String> {
        let mut labels = HashMap::new();
        labels.insert("com.tidepool.bead_id".to_string(), req.bead_id.clone());
        labels.insert("com.tidepool.role".to_string(), "agent".to_string());
        labels.insert(
            "com.tidepool.expires_at".to_string(), 
            req.expires_at.clone().unwrap_or_else(|| "never".to_string())
        ); 

        let worktree_mount_target = format!("/worktrees/{}", req.bead_id);
        
        let mut mounts = vec![
            Mount {
                target: Some(worktree_mount_target.clone()),
                source: Some(req.worktree_path.to_string_lossy().to_string()),
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

        let uid = req.uid.unwrap_or(self.host_uid);
        let gid = req.gid.unwrap_or(self.host_gid);
        let user = format!("{}:{}", uid, gid);

        Config {
            image: Some(self.agent_image.clone()),
            labels: Some(labels),
            host_config: Some(HostConfig {
                mounts: Some(mounts),
                network_mode: Some(self.network_name.clone()),
                ..Default::default()
            }),
            user: Some(user),
            env: Some(vec![
                format!("TIDEPOOL_BEAD_ID={}", req.bead_id),
                format!("TIDEPOOL_BACKEND={}", req.backend),
            ]),
            ..Default::default()
        }
    }

    pub async fn status(&self, id: &str) -> anyhow::Result<String> {
        match self.docker.inspect_container(id, None::<InspectContainerOptions>).await {
            Ok(inspect) => {
                let status = inspect.state.and_then(|s| s.status).map(|s| s.to_string()).unwrap_or_else(|| "unknown".to_string());
                Ok(status)
            },
            Err(bollard::errors::Error::DockerResponseServerError { status_code: 404, .. }) => {
                Ok("not_found".to_string())
            },
            Err(e) => Err(e.into()),
        }
    }

    pub async fn stop(&self, id: &str) -> anyhow::Result<()> {
        info!("Stopping container {} with 10s timeout", id);
        let stop_options = StopContainerOptions {
            t: 10,
        };
        match self.docker.stop_container(id, Some(stop_options)).await {
            Ok(_) => (),
            Err(bollard::errors::Error::DockerResponseServerError { status_code: 404, .. }) => {
                warn!("Container {} not found while stopping", id);
                return Ok(());
            },
            Err(e) => return Err(e.into()),
        }

        info!("Removing container {}", id);
        self.docker.remove_container(id, None::<RemoveContainerOptions>).await?;
        Ok(())
    }

    /// Execute a command in a running container
    pub async fn exec(&self, id: &str, req: ExecRequest) -> anyhow::Result<ExecResponse> {
        info!("Executing command in container {}: {:?}", id, req.cmd);

        let exec_options = CreateExecOptions {
            cmd: Some(req.cmd),
            attach_stdout: Some(true),
            attach_stderr: Some(true),
            working_dir: req.workdir,
            env: req.env,
            user: req.user,
            ..Default::default()
        };

        let exec = self.docker.create_exec(id, exec_options).await?;

        let start_result = self.docker.start_exec(&exec.id, None).await?;

        let mut stdout = Vec::new();
        let mut stderr = Vec::new();

        match start_result {
            StartExecResults::Attached { mut output, .. } => {
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
                            warn!("Error reading exec output: {}", e);
                        }
                    }
                }
            }
            StartExecResults::Detached => {
                warn!("Exec started in detached mode unexpectedly");
            }
        }

        // Get exit code
        let inspect = self.docker.inspect_exec(&exec.id).await?;
        let exit_code = inspect.exit_code;

        Ok(ExecResponse {
            exit_code,
            stdout: String::from_utf8_lossy(&stdout).to_string(),
            stderr: String::from_utf8_lossy(&stderr).to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_build_container_config() {
        let spawner = Spawner {
            docker: Docker::connect_with_local_defaults().unwrap_or_else(|_| {
                Docker::connect_with_http_defaults().unwrap()
            }),
            agent_image: "tidepool-agent:latest".to_string(),
            host_uid: 1000,
            host_gid: 1000,
            network_name: "tidepool-test".to_string(),
        };

        let req = SpawnRequest {
            bead_id: "test-bead".to_string(),
            worktree_path: PathBuf::from("/tmp/worktree"),
            backend: "claude".to_string(),
            uid: Some(2000),
            gid: Some(2000),
            expires_at: Some("2026-01-26T00:00:00Z".to_string()),
        };

        let config = spawner.build_container_config(&req);

        assert_eq!(config.image, Some("tidepool-agent:latest".to_string()));
        assert_eq!(config.user, Some("2000:2000".to_string()));
        
        let labels = config.labels.unwrap();
        assert_eq!(labels.get("com.tidepool.bead_id").unwrap(), "test-bead");
        assert_eq!(labels.get("com.tidepool.role").unwrap(), "agent");
        assert_eq!(labels.get("com.tidepool.expires_at").unwrap(), "2026-01-26T00:00:00Z");

        let host_config = config.host_config.unwrap();
        assert_eq!(host_config.network_mode, Some("tidepool-test".to_string()));
        
        let mounts = host_config.mounts.unwrap();
        assert_eq!(mounts.len(), 2);
        assert_eq!(mounts[0].target, Some("/worktrees/test-bead".to_string()));
        assert_eq!(mounts[0].source, Some("/tmp/worktree".to_string()));
        
        assert_eq!(mounts[1].target, Some("/home/agent/.config/gh".to_string()));
        assert_eq!(mounts[1].source, Some("tidepool-gh-auth".to_string()));
        assert_eq!(mounts[1].typ, Some(MountTypeEnum::VOLUME));

        let env = config.env.unwrap();
        assert!(env.contains(&"TIDEPOOL_BEAD_ID=test-bead".to_string()));
        assert!(env.contains(&"TIDEPOOL_BACKEND=claude".to_string()));
    }
}
