//! ACP connection registry for Gemini agent communication.
//!
//! Manages active ACP (Agent Client Protocol) connections to spawned Gemini agents.
//! Each connection allows structured messaging (prompts, notifications) instead of
//! fragile Zellij STDIN injection.

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

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

    pub async fn register(&self, _agent_id: String, _conn: AcpConnection) {
        todo!("Insert connection into registry")
    }

    pub async fn get(&self, _agent_id: &str) -> Option<Arc<AcpConnection>> {
        todo!("Look up connection by agent ID")
    }

    pub async fn remove(&self, _agent_id: &str) -> Option<Arc<AcpConnection>> {
        todo!("Remove and return connection")
    }
}
