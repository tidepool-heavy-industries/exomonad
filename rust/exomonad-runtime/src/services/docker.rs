use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::process::Stdio;
use tokio::process::Command;

pub trait DockerExecutor: Send + Sync {
    fn exec<'a>(
        &'a self,
        container: &'a str,
        dir: &'a str,
        cmd: &'a [&'a str],
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>>;
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExecOutput {
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ContainerInfo {
    pub id: String,
    pub names: String,
    pub image: String,
    pub status: String,
}

#[derive(Clone)]
pub struct DockerService {
    docker_cmd: String,
}

impl DockerService {
    pub fn new() -> Self {
        Self {
            docker_cmd: "docker".to_string(),
        }
    }

    #[cfg(test)]
    pub fn with_cmd(cmd: &str) -> Self {
        Self {
            docker_cmd: cmd.to_string(),
        }
    }

    pub async fn exec(
        &self,
        container: &str,
        cmd: &[&str],
        workdir: Option<&str>,
    ) -> Result<ExecOutput> {
        let mut args = vec!["exec"];
        if let Some(dir) = workdir {
            args.extend(["-w", dir]);
        }
        args.push(container);
        args.extend(cmd);

        let output = Command::new(&self.docker_cmd)
            .args(&args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .context("Failed to execute docker command")?;

        Ok(ExecOutput {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            exit_code: output.status.code().unwrap_or(-1),
        })
    }

    pub async fn spawn(
        &self,
        image: &str,
        name: &str,
        env: &HashMap<String, String>,
    ) -> Result<String> {
        let mut command = Command::new(&self.docker_cmd);
        command.arg("run").arg("-d").arg("--name").arg(name);

        for (k, v) in env {
            command.arg("-e").arg(format!("{}={}", k, v));
        }

        command.arg(image);

        let output = command
            .output()
            .await
            .context("Failed to spawn container")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Docker spawn failed: {}", stderr);
        }

        let id = String::from_utf8_lossy(&output.stdout).trim().to_string();
        Ok(id)
    }

    pub async fn kill(&self, container: &str) -> Result<()> {
        let output = Command::new(&self.docker_cmd)
            .arg("rm")
            .arg("-f")
            .arg(container)
            .output()
            .await
            .context("Failed to kill container")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Docker kill failed: {}", stderr);
        }
        Ok(())
    }

    pub async fn list(&self, filter: Option<&str>) -> Result<Vec<ContainerInfo>> {
        let mut command = Command::new(&self.docker_cmd);
        command.arg("ps").arg("--format").arg("json");
        if let Some(f) = filter {
            command.arg("--filter").arg(f);
        }

        let output = command
            .output()
            .await
            .context("Failed to list containers")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Docker list failed: {}", stderr);
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut containers = Vec::new();

        for line in stdout.lines() {
            if line.trim().is_empty() {
                continue;
            }
            // Parse partial JSON from docker ps
            #[derive(Deserialize)]
            struct DockerPsEntry {
                #[serde(rename = "ID")]
                id: String,
                #[serde(rename = "Names")]
                names: String,
                #[serde(rename = "Image")]
                image: String,
                #[serde(rename = "Status")]
                status: String,
            }

            match serde_json::from_str::<DockerPsEntry>(line) {
                Ok(entry) => {
                    containers.push(ContainerInfo {
                        id: entry.id,
                        names: entry.names,
                        image: entry.image,
                        status: entry.status,
                    });
                }
                Err(e) => {
                    tracing::warn!("Failed to parse docker ps line: {} - {}", line, e);
                }
            }
        }

        Ok(containers)
    }
}

impl DockerExecutor for DockerService {
    fn exec<'a>(
        &'a self,
        container: &'a str,
        dir: &'a str,
        cmd: &'a [&'a str],
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>> {
        let this = self.clone();
        let container = container.to_string();
        let dir = dir.to_string();
        let cmd = cmd.iter().map(|s| s.to_string()).collect::<Vec<_>>();

        Box::pin(async move {
            let cmd_refs: Vec<&str> = cmd.iter().map(|s| s.as_str()).collect();
            let output = this.exec(&container, &cmd_refs, Some(&dir)).await?;
            
            if output.exit_code != 0 {
                 return Err(anyhow::anyhow!("Docker exec failed: {}", output.stderr));
            }
            Ok(output.stdout)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    #[cfg(unix)]
    use std::os::unix::fs::PermissionsExt;
    use tempfile::TempDir;

    // Helper to create a mock docker script
    fn setup_mock_docker() -> (TempDir, String) {
        let temp_dir = TempDir::new().unwrap();
        let docker_path = temp_dir.path().join("docker_mock.sh");

        let script = r#"#!/bin/sh
# Mock docker script
case "$1" in
    exec)
        # Check if -w is passed
        if [ "$2" = "-w" ]; then
             echo "Workdir: $3"
        fi
        echo "Mock Exec Output"
        ;; 
    run)
        echo "mock-container-id"
        ;; 
    rm)
        if [ "$3" = "missing-container" ]; then
            echo "Error: No such container: missing-container" >&2
            exit 1
        fi
        echo "mock-container-id"
        ;; 
    ps)
        echo '{"ID":"123","Names":"test-container","Image":"alpine","Status":"Up"}'
        ;; 
    *)
        echo "Unknown command: $@" >&2
        exit 1
        ;; 
esac
"#;
        fs::write(&docker_path, script).unwrap();
        
        #[cfg(unix)]
        {
            let mut perms = fs::metadata(&docker_path).unwrap().permissions();
            perms.set_mode(0o755);
            fs::set_permissions(&docker_path, perms).unwrap();
        }

        (temp_dir, docker_path.to_string_lossy().to_string())
    }

    #[tokio::test]
    async fn test_exec() {
        let (_tmp, mock_cmd) = setup_mock_docker();
        let service = DockerService::with_cmd(&mock_cmd);
        let res = service
            .exec("test-container", &["ls"], Some("/app"))
            .await
            .unwrap();

        // The mock script prints "Workdir: /app\nMock Exec Output"
        assert!(res.stdout.contains("Mock Exec Output"));
        assert!(res.stdout.contains("Workdir: /app"));
    }

    #[tokio::test]
    async fn test_spawn() {
        let (_tmp, mock_cmd) = setup_mock_docker();
        let service = DockerService::with_cmd(&mock_cmd);
        let env_vars = HashMap::new();
        let id = service
            .spawn("alpine", "test-agent", &env_vars)
            .await
            .unwrap();
        assert_eq!(id, "mock-container-id");
    }

    #[tokio::test]
    async fn test_kill() {
        let (_tmp, mock_cmd) = setup_mock_docker();
        let service = DockerService::with_cmd(&mock_cmd);
        
        // Test success
        let res = service.kill("test-container").await;
        assert!(res.is_ok());

        // Test failure (mock script returns error for "missing-container")
        let res = service.kill("missing-container").await;
        assert!(res.is_err());
    }

    #[tokio::test]
    async fn test_list() {
        let (_tmp, mock_cmd) = setup_mock_docker();
        let service = DockerService::with_cmd(&mock_cmd);
        let list = service.list(None).await.unwrap();
        assert_eq!(list.len(), 1);
        assert_eq!(list[0].id, "123");
        assert_eq!(list[0].names, "test-container");
    }
}
