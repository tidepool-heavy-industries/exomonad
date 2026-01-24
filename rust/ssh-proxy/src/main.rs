use anyhow::{Context, Result};
use async_ssh2_tokio::{AuthMethod, Client, ServerCheckMethod};
use axum::{
    extract::State,
    http::StatusCode,
    routing::{get, post},
    Json, Router,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{error, info, instrument};
use tracing_subscriber::EnvFilter;

#[derive(Debug, Deserialize)]
struct ExecRequest {
    container: String,
    command: String,
    #[serde(default)]
    args: Vec<String>,
    #[serde(default = "default_working_dir")]
    working_dir: String,
    #[serde(default)]
    env: HashMap<String, String>,
    #[serde(default = "default_timeout")]
    timeout_secs: u64,
}

fn default_working_dir() -> String {
    "/".to_string()
}

fn default_timeout() -> u64 {
    300
}

#[derive(Debug, Serialize)]
struct ExecResponse {
    exit_code: i32,
    stdout: String,
    stderr: String,
}

#[derive(Debug, Serialize, Clone)]
struct ErrorResponse {
    error: String,
    container: String,
}

struct ConnectionPool {
    clients: HashMap<String, Arc<Client>>,
    key_path: String,
    // TODO: Implement periodic pruning of idle connections (e.g., >30m unused)
}

impl ConnectionPool {
    fn new(key_path: String) -> Self {
        Self {
            clients: HashMap::new(),
            key_path,
        }
    }

    async fn get_or_connect(&mut self, container: &str) -> Result<Arc<Client>> {
        if let Some(client) = self.clients.get(container) {
            return Ok(Arc::clone(client));
        }

        let hostname = format!("{}.docker.internal", container);
        info!(%container, %hostname, "Connecting to agent");

        let auth = AuthMethod::with_key_file(self.key_path.as_str(), None);
        let client = Client::connect(
            (hostname.as_str(), 22),
            "root",
            auth,
            ServerCheckMethod::NoCheck,
        )
        .await
        .context("SSH connection failed")?;

        let client = Arc::new(client);
        self.clients.insert(container.to_string(), Arc::clone(&client));
        Ok(client)
    }

    fn remove(&mut self, container: &str) {
        self.clients.remove(container);
    }
}

type SharedPool = Arc<Mutex<ConnectionPool>>;

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::INFO.into()))
        .init();

    let key_path = std::env::var("SSH_PROXY_KEY_PATH")
        .unwrap_or_else(|_| "/etc/ssh-proxy/orchestrator_key".to_string());
    let port = std::env::var("SSH_PROXY_PORT")
        .unwrap_or_else(|_| "7433".to_string())
        .parse::<u16>()
        .context("Invalid SSH_PROXY_PORT")?;

    info!(%key_path, %port, "Starting ssh-proxy");

    let pool = Arc::new(Mutex::new(ConnectionPool::new(key_path)));

    let app = Router::new()
        .route("/exec", post(handle_exec))
        .route("/health", get(|| async { "ok" }))
        .with_state(pool);

    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

#[instrument(skip(pool))]
async fn handle_exec(
    State(pool): State<SharedPool>,
    Json(req): Json<ExecRequest>,
) -> (StatusCode, Json<serde_json::Value>) {
    let container = req.container.clone();

    let result = execute_in_container(pool, req).await;

    match result {
        Ok(res) => (StatusCode::OK, Json(serde_json::to_value(res).unwrap())),
        Err(e) => {
            error!(error = %e, "Command execution failed");
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(
                    serde_json::to_value(ErrorResponse {
                        error: e.to_string(),
                        container,
                    })
                    .unwrap(),
                ),
            )
        }
    }
}

async fn execute_in_container(pool: SharedPool, req: ExecRequest) -> Result<ExecResponse> {
    let client = {
        let mut pool_guard = pool.lock().await;
        pool_guard.get_or_connect(&req.container).await?
    };

    let full_cmd = build_command_string(&req);
    info!(%req.container, %full_cmd, "Executing command");

    let result = match tokio::time::timeout(
        std::time::Duration::from_secs(req.timeout_secs),
        client.execute(&full_cmd),
    )
    .await
    {
        Ok(Ok(res)) => res,
        Ok(Err(e)) => {
            // If execution fails, maybe the connection is dead?
            let mut pool_guard = pool.lock().await;
            pool_guard.remove(&req.container);
            return Err(anyhow::anyhow!("SSH execution error: {}", e));
        }
        Err(_) => {
            return Err(anyhow::anyhow!(
                "Command timed out after {}s",
                req.timeout_secs
            ));
        }
    };

    Ok(ExecResponse {
        exit_code: result.exit_status as i32,
        stdout: result.stdout,
        stderr: result.stderr,
    })
}

fn build_command_string(req: &ExecRequest) -> String {
    use shell_escape::escape;
    use std::borrow::Cow;

    let escaped_working_dir = escape(Cow::Borrowed(&req.working_dir));

    let mut env_prefix = String::new();
    for (k, v) in &req.env {
        env_prefix.push_str(&format!("{}={} ", k, escape(Cow::Borrowed(v))));
    }

    let escaped_command = escape(Cow::Borrowed(&req.command));

    let escaped_args: Vec<_> = req.args.iter().map(|a| escape(Cow::Borrowed(a))).collect();
    let args_str = escaped_args.join(" ");

    let cmd_with_args = if args_str.is_empty() {
        escaped_command.into_owned()
    } else {
        format!("{} {}", escaped_command, args_str)
    };

    if env_prefix.is_empty() {
        format!("cd {} && {}", escaped_working_dir, cmd_with_args)
    } else {
        format!(
            "cd {} && {}{}",
            escaped_working_dir, env_prefix, cmd_with_args
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_command_string_simple() {
        let mut env = HashMap::new();
        env.insert("FOO".to_string(), "bar".to_string());
        let req = ExecRequest {
            container: "test".to_string(),
            command: "ls".to_string(),
            args: vec!["-la".to_string()],
            working_dir: "/tmp".to_string(),
            env,
            timeout_secs: 30,
        };

        let cmd = build_command_string(&req);
        assert!(cmd.contains("cd /tmp"));
        assert!(cmd.contains("FOO=bar"));
        assert!(cmd.contains("ls -la"));
    }

    #[test]
    fn test_build_command_string_escaping() {
        let req = ExecRequest {
            container: "test".to_string(),
            command: "echo".to_string(),
            args: vec!["hello world; rm -rf /".to_string()],
            working_dir: "/path with spaces".to_string(),
            env: HashMap::new(),
            timeout_secs: 30,
        };

        let cmd = build_command_string(&req);
        assert!(cmd.contains("cd '/path with spaces'"));
        assert!(cmd.contains("echo 'hello world; rm -rf /'"));
    }
}