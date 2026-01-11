//! HTTP client for mantle-hub API.

use reqwest::Client;
use std::time::Duration;

use super::config::HubConfig;
use super::types::{
    GraphData, NodeCreateResponse, NodeInfo, NodeRegister, NodeResult,
    SessionCreateEmptyRequest, SessionCreateEmptyResponse, SessionCreateResponse, SessionInfo,
    SessionRegister, SessionWithNodes,
};
use crate::error::{MantleError, Result};
use crate::events::StreamEvent;

/// Client for communicating with mantle-hub.
#[derive(Debug, Clone)]
pub struct HubClient {
    client: Client,
    base_url: String,
}

impl HubClient {
    /// Create a new hub client with the given base URL.
    pub fn new(base_url: impl Into<String>) -> Result<Self> {
        let client = Client::builder()
            .timeout(Duration::from_secs(30))
            .build()
            .map_err(|e| MantleError::Hub(format!("Failed to create HTTP client: {}", e)))?;

        Ok(Self {
            client,
            base_url: base_url.into(),
        })
    }

    /// Create a client from configuration.
    pub fn from_config(config: &HubConfig) -> Result<Self> {
        Self::new(&config.http_url)
    }

    /// Create a client with default configuration.
    pub fn from_default_config() -> Result<Self> {
        let config = HubConfig::load();
        Self::from_config(&config)
    }

    /// Check if the hub is reachable.
    pub async fn health_check(&self) -> Result<()> {
        let url = format!("{}/api/sessions", self.base_url);
        self.client
            .get(&url)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Hub not reachable at {}: {}", self.base_url, e)))?;
        Ok(())
    }

    // =========================================================================
    // Session Operations
    // =========================================================================

    /// Create a new session with its root node.
    ///
    /// Returns both the session and the root node.
    pub async fn create_session(&self, req: &SessionRegister) -> Result<SessionCreateResponse> {
        let url = format!("{}/api/sessions", self.base_url);
        let response = self
            .client
            .post(&url)
            .json(req)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to create session: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        response
            .json()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to parse session response: {}", e)))
    }

    /// Create an empty session (no root node).
    ///
    /// Used for graph execution tracking where Haskell creates the session first,
    /// then mantle registers nodes into it.
    ///
    /// Returns the session info with the generated session_id.
    pub async fn create_empty_session(&self, name: &str) -> Result<SessionCreateEmptyResponse> {
        let url = format!("{}/api/sessions/empty", self.base_url);
        let req = SessionCreateEmptyRequest {
            name: name.to_string(),
        };
        let response = self
            .client
            .post(&url)
            .json(&req)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to create empty session: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        response
            .json()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to parse empty session response: {}", e)))
    }

    /// Get a session with all its nodes by ID.
    pub async fn get_session(&self, session_id: &str) -> Result<SessionWithNodes> {
        let url = format!("{}/api/sessions/{}", self.base_url, session_id);
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to get session: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        response
            .json()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to parse session response: {}", e)))
    }

    /// List all sessions.
    pub async fn list_sessions(&self) -> Result<Vec<SessionInfo>> {
        let url = format!("{}/api/sessions", self.base_url);
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to list sessions: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        response
            .json()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to parse sessions response: {}", e)))
    }

    /// Delete a session.
    pub async fn delete_session(&self, session_id: &str) -> Result<()> {
        let url = format!("{}/api/sessions/{}", self.base_url, session_id);
        let response = self
            .client
            .delete(&url)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to delete session: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        Ok(())
    }

    /// Get graph data for a session.
    pub async fn get_graph(&self, session_id: &str) -> Result<GraphData> {
        let url = format!("{}/api/sessions/{}/graph", self.base_url, session_id);
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to get graph: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        response
            .json()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to parse graph response: {}", e)))
    }

    // =========================================================================
    // Node Operations
    // =========================================================================

    /// Add a child node to a session.
    pub async fn create_node(
        &self,
        session_id: &str,
        req: &NodeRegister,
    ) -> Result<NodeCreateResponse> {
        let url = format!("{}/api/sessions/{}/nodes", self.base_url, session_id);
        let response = self
            .client
            .post(&url)
            .json(req)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to create node: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        response
            .json()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to parse node response: {}", e)))
    }

    /// Get a node by ID.
    pub async fn get_node(&self, session_id: &str, node_id: &str) -> Result<NodeInfo> {
        let url = format!(
            "{}/api/sessions/{}/nodes/{}",
            self.base_url, session_id, node_id
        );
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to get node: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        response
            .json()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to parse node response: {}", e)))
    }

    /// Submit a node result.
    pub async fn submit_node_result(
        &self,
        session_id: &str,
        node_id: &str,
        result: &NodeResult,
    ) -> Result<()> {
        let url = format!(
            "{}/api/sessions/{}/nodes/{}/result",
            self.base_url, session_id, node_id
        );
        let response = self
            .client
            .post(&url)
            .json(result)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to submit result: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(MantleError::Hub(format!(
                "Hub returned error {}: {}",
                status, body
            )));
        }

        Ok(())
    }
}

// ============================================================================
// Sync Event Stream (for use in sync code like run_claude_direct)
// ============================================================================

use std::net::TcpStream as StdTcpStream;
use tungstenite::{connect, stream::MaybeTlsStream as SyncMaybeTlsStream, WebSocket};

/// Synchronous WebSocket stream for sending events to hub.
///
/// Unlike `EventStream`, this uses blocking I/O and can be used
/// in synchronous code like `run_claude_direct`.
pub struct SyncEventStream {
    ws: WebSocket<SyncMaybeTlsStream<StdTcpStream>>,
    session_id: String,
    node_id: String,
}

impl SyncEventStream {
    /// Connect to hub WebSocket for event streaming (blocking).
    ///
    /// The WebSocket path now includes both session_id and node_id:
    /// `/ws/push/{session_id}/{node_id}`
    pub fn connect(base_url: &str, session_id: &str, node_id: &str) -> Result<Self> {
        let ws_url = base_url
            .replace("http://", "ws://")
            .replace("https://", "wss://");
        let url = format!("{}/ws/push/{}/{}", ws_url, session_id, node_id);

        let (ws, _response) = connect(&url)
            .map_err(|e| MantleError::Hub(format!("WebSocket connect failed to {}: {}", url, e)))?;

        Ok(Self {
            ws,
            session_id: session_id.to_string(),
            node_id: node_id.to_string(),
        })
    }

    /// Send a stream event to the hub (blocking).
    pub fn send_event(&mut self, event: &StreamEvent) -> Result<()> {
        let json = serde_json::to_string(event)
            .map_err(|e| MantleError::Hub(format!("Failed to serialize event: {}", e)))?;

        self.ws
            .send(tungstenite::Message::Text(json.into()))
            .map_err(|e| {
                MantleError::Hub(format!(
                    "Failed to send event for node {}: {}",
                    self.node_id, e
                ))
            })?;

        Ok(())
    }

    /// Close the WebSocket connection.
    pub fn close(mut self) -> Result<()> {
        self.ws.close(None).map_err(|e| {
            MantleError::Hub(format!("Failed to close WebSocket: {}", e))
        })?;
        // Flush any remaining messages
        while let Ok(msg) = self.ws.read() {
            if msg.is_close() {
                break;
            }
        }
        Ok(())
    }

    /// Get the session ID this stream is connected for.
    pub fn session_id(&self) -> &str {
        &self.session_id
    }

    /// Get the node ID this stream is connected for.
    pub fn node_id(&self) -> &str {
        &self.node_id
    }
}
