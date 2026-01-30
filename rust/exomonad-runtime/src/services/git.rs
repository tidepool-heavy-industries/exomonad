use crate::services::docker::DockerExecutor;
use anyhow::Result;
use extism::{CurrentPlugin, Error, Function, UserData, Val, ValType};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Commit {
    pub hash: String,
    pub message: String,
    pub author: String,
    pub date: String,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct WorktreeInfo {
    pub path: String,
    pub branch: String,
}

#[derive(Clone)]
pub struct GitService {
    docker: Arc<dyn DockerExecutor>,
}

impl GitService {
    pub fn new(docker: Arc<dyn DockerExecutor>) -> Self {
        Self { docker }
    }

    async fn exec_git(&self, container: &str, dir: &str, args: &[&str]) -> Result<String> {
        let mut cmd = vec!["git"];
        cmd.extend_from_slice(args);
        self.docker.exec(container, dir, &cmd).await
    }

    pub async fn get_branch(&self, container: &str, dir: &str) -> Result<String> {
        let output = self
            .exec_git(container, dir, &["rev-parse", "--abbrev-ref", "HEAD"])
            .await?;
        Ok(output.trim().to_string())
    }

    pub async fn get_worktree(&self, container: &str, dir: &str) -> Result<WorktreeInfo> {
        let path = self
            .exec_git(container, dir, &["rev-parse", "--show-toplevel"])
            .await?;
        let branch = self.get_branch(container, dir).await?;
        Ok(WorktreeInfo {
            path: path.trim().to_string(),
            branch,
        })
    }

    pub async fn get_dirty_files(&self, container: &str, dir: &str) -> Result<Vec<String>> {
        let output = self
            .exec_git(container, dir, &["status", "--porcelain"])
            .await?;
        Ok(output.lines().map(|l| l.to_string()).collect())
    }

    pub async fn get_recent_commits(
        &self,
        container: &str,
        dir: &str,
        n: u32,
    ) -> Result<Vec<Commit>> {
        let format = "%H|%an|%ad|%s";
        let output = self
            .exec_git(
                container,
                dir,
                &["log", &format!("-n{}", n), &format!("--format={}", format)],
            )
            .await?;

        let mut commits = Vec::new();
        for line in output.lines() {
            let parts: Vec<&str> = line.split('|').collect();
            if parts.len() >= 4 {
                commits.push(Commit {
                    hash: parts[0].to_string(),
                    message: parts[3..].join("|"),
                    author: parts[1].to_string(),
                    date: parts[2].to_string(),
                });
            }
        }
        Ok(commits)
    }
}

// Host Function Types

#[derive(Deserialize)]
struct GitHostInput {
    #[serde(rename = "workingDir")]
    working_dir: String,
    #[serde(rename = "containerId")]
    container_id: String,
}

#[derive(Deserialize)]
struct GitLogInput {
    #[serde(rename = "workingDir")]
    working_dir: String,
    #[serde(rename = "containerId")]
    container_id: String,
    limit: u32,
}

#[derive(Serialize)]
#[serde(tag = "kind", content = "payload")]
enum GitHostOutput<T> {
    Success(T),
    Error(GitError),
}

#[derive(Serialize)]
struct GitError {
    message: String,
}

impl<T> From<Result<T>> for GitHostOutput<T> {
    fn from(res: Result<T>) -> Self {
        match res {
            Ok(val) => GitHostOutput::Success(val),
            Err(e) => GitHostOutput::Error(GitError {
                message: e.to_string(),
            }),
        }
    }
}

// Helper functions for Extism memory access (Pattern A: memory handles)

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
        Err(_) => Err(Error::msg(
            "No Tokio runtime available for async Git operation",
        )),
    }
}

// Host Function Factories

pub fn git_get_branch_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_branch",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let input: GitHostInput = get_input(plugin, inputs[0].clone())?;

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_branch(&input.container_id, &input.working_dir))?;
            let output: GitHostOutput<String> = result.into();

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_get_worktree_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_worktree",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let input: GitHostInput = get_input(plugin, inputs[0].clone())?;

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_worktree(&input.container_id, &input.working_dir))?;
            let output: GitHostOutput<WorktreeInfo> = result.into();

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_get_dirty_files_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_dirty_files",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let input: GitHostInput = get_input(plugin, inputs[0].clone())?;

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_dirty_files(&input.container_id, &input.working_dir))?;
            let output: GitHostOutput<Vec<String>> = result.into();

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

pub fn git_get_recent_commits_host_fn(git_service: Arc<GitService>) -> Function {
    Function::new(
        "git_get_recent_commits",
        [ValType::I64],
        [ValType::I64],
        UserData::new(git_service),
        |plugin: &mut CurrentPlugin,
         inputs: &[Val],
         outputs: &mut [Val],
         user_data: UserData<Arc<GitService>>|
         -> Result<(), Error> {
            let input: GitLogInput = get_input(plugin, inputs[0].clone())?;

            let git_arc = user_data.get()?;
            let git = git_arc.lock().map_err(|_| Error::msg("Poisoned lock"))?;

            let result = block_on(git.get_recent_commits(
                &input.container_id,
                &input.working_dir,
                input.limit,
            ))?;
            let output: GitHostOutput<Vec<Commit>> = result.into();

            outputs[0] = set_output(plugin, &output)?;
            Ok(())
        },
    )
    .with_namespace("env")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Mutex;

    struct MockDocker {
        responses: Mutex<Vec<Result<String>>>,
    }

    impl MockDocker {
        fn new(responses: Vec<Result<String>>) -> Self {
            Self {
                responses: Mutex::new(responses),
            }
        }
    }

    impl DockerExecutor for MockDocker {
        fn exec<'a>(
            &'a self,
            _container: &'a str,
            _dir: &'a str,
            _cmd: &'a [&'a str],
        ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>> {
            let response = self.responses.lock().unwrap().remove(0);
            Box::pin(async move { response })
        }
    }

    #[tokio::test]
    async fn test_get_branch() {
        let mock = Arc::new(MockDocker::new(vec![Ok("main\n".to_string())]));
        let git = GitService::new(mock);
        let branch = git.get_branch("c1", "/app").await.unwrap();
        assert_eq!(branch, "main");
    }

    #[tokio::test]
    async fn test_get_worktree() {
        let mock = Arc::new(MockDocker::new(vec![
            Ok("/app\n".to_string()),        // rev-parse --show-toplevel
            Ok("feature/123\n".to_string()), // rev-parse --abbrev-ref HEAD
        ]));
        let git = GitService::new(mock);
        let wt = git.get_worktree("c1", "/app/src").await.unwrap();
        assert_eq!(wt.path, "/app");
        assert_eq!(wt.branch, "feature/123");
    }

    #[tokio::test]
    async fn test_get_commits() {
        let log_output =
            "hash1|Author One|2023-01-01|Message 1\nhash2|Author Two|2023-01-02|Message 2\n";
        let mock = Arc::new(MockDocker::new(vec![Ok(log_output.to_string())]));
        let git = GitService::new(mock);
        let commits = git.get_recent_commits("c1", "/app", 2).await.unwrap();
        assert_eq!(commits.len(), 2);
        assert_eq!(commits[0].hash, "hash1");
        assert_eq!(commits[1].message, "Message 2");
    }
}
