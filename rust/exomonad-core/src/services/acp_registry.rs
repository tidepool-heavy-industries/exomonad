//! ACP connection registry for Gemini agent communication.
//!
//! Manages active ACP (Agent Client Protocol) connections to spawned Gemini agents.
//! Each connection allows structured messaging (prompts, notifications) instead of
//! fragile Zellij STDIN injection.

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use anyhow::Context;
use agent_client_protocol::{
    Agent, ClientSideConnection, InitializeRequest, NewSessionRequest, PromptRequest,
    Implementation, ProtocolVersion,
};
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};
use std::process::Stdio;
use tokio::process::Command;

/// Metadata for an active ACP connection.
#[derive(Debug)]
pub struct AcpConnection {
    /// Agent identifier (e.g., "worker-rust-impl").
    pub agent_id: String,
    /// ACP session ID from `new_session` response.
    pub session_id: agent_client_protocol::SessionId,
    /// The underlying ACP connection for sending prompts.
    pub conn: agent_client_protocol::ClientSideConnection,
}

/// Registry of active ACP connections, keyed by agent name.
#[derive(Debug, Clone)]
pub struct AcpRegistry {
    connections: Arc<RwLock<HashMap<String, Arc<AcpConnection>>>>,
}

impl AcpRegistry {
    pub fn new() -> Self {
        Self {
            connections: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn register(&self, agent_id: String, conn: AcpConnection) {
        let mut connections = self.connections.write().await;
        tracing::info!(agent = %agent_id, "Registering ACP connection");
        connections.insert(agent_id, Arc::new(conn));
    }

    pub async fn get(&self, agent_id: &str) -> Option<Arc<AcpConnection>> {
        self.connections.read().await.get(agent_id).cloned()
    }

    pub async fn remove(&self, agent_id: &str) -> Option<Arc<AcpConnection>> {
        let mut connections = self.connections.write().await;
        let removed = connections.remove(agent_id);
        if removed.is_some() {
            tracing::info!(agent = %agent_id, "Removed ACP connection");
        }
        removed
    }
}

/// Spawn a Gemini CLI subprocess with ACP and send the initial prompt.
///
/// Returns the AcpConnection (with session_id and connection) for registry storage.
/// The caller is responsible for registering it in the AcpRegistry.
pub async fn connect_and_prompt(
    agent_id: String,
    gemini_command: &str,
    working_dir: &std::path::Path,
    initial_prompt: &str,
    env_vars: Vec<(String, String)>,
) -> anyhow::Result<AcpConnection> {
    tracing::info!(agent = %agent_id, cwd = %working_dir.display(), "Spawning ACP subprocess");

    let mut child = Command::new(gemini_command)
        .arg("--experimental-acp")
        .current_dir(working_dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .envs(env_vars)
        .spawn()
        .context("Failed to spawn gemini with --experimental-acp")?;

    let stdin = child.stdin.take().context("No stdin on child")?;
    let stdout = child.stdout.take().context("No stdout on child")?;

    // Convert tokio AsyncRead/Write to futures AsyncRead/Write
    let outgoing = stdin.compat_write();
    let incoming = stdout.compat();

    let client = super::acp_client::ExoMonadAcpClient {
        agent_id: agent_id.clone(),
    };

    let (conn, io_task) = ClientSideConnection::new(
        client,
        outgoing,
        incoming,
        |fut| {
            tokio::spawn(fut);
        },
    );

    // Spawn the I/O task
    tokio::spawn(async move {
        if let Err(e) = io_task.await {
            tracing::warn!(error = %e, "ACP I/O task ended with error");
        }
    });

    // Initialize
    tracing::info!(agent = %agent_id, "Initializing ACP connection");
    conn.initialize(
        InitializeRequest::new(ProtocolVersion::V1)
            .client_info(Implementation::new("exomonad", env!("CARGO_PKG_VERSION"))),
    )
    .await
    .map_err(|e| anyhow::anyhow!("ACP initialize failed: {:?}", e))?;

    // Create session
    tracing::info!(agent = %agent_id, "Creating ACP session");
    let session = conn.new_session(NewSessionRequest::new(working_dir.to_path_buf()))
        .await
        .map_err(|e| anyhow::anyhow!("ACP new_session failed: {:?}", e))?;

    let session_id = session.session_id.clone();
    tracing::info!(agent = %agent_id, session_id = %session_id, "ACP session created");

    // Send initial prompt
    tracing::info!(agent = %agent_id, "Sending initial ACP prompt");
    conn.prompt(PromptRequest::new(
        session_id.clone(),
        vec![initial_prompt.into()],
    ))
    .await
    .map_err(|e| anyhow::anyhow!("ACP prompt failed: {:?}", e))?;

    tracing::info!(agent = %agent_id, "Initial ACP prompt completed");

    Ok(AcpConnection {
        agent_id,
        session_id,
        conn,
    })
}
