//! HTTP client for mantle-hub API.

use reqwest::Client;
use std::time::Duration;

use super::config::HubConfig;
use super::types::{GraphData, SessionInfo, SessionRegister, SessionResult};
use crate::error::{MantleError, Result};

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

    /// Register a new session with the hub.
    pub async fn register_session(&self, req: &SessionRegister) -> Result<SessionInfo> {
        let url = format!("{}/api/sessions", self.base_url);
        let response = self
            .client
            .post(&url)
            .json(req)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to register session: {}", e)))?;

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

    /// Get a session by ID.
    pub async fn get_session(&self, session_id: &str) -> Result<SessionInfo> {
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

    /// Submit a session result.
    pub async fn submit_result(&self, result: &SessionResult) -> Result<()> {
        let url = format!("{}/api/sessions/{}/result", self.base_url, result.session_id);
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

    /// Poll for a session result (blocking with timeout).
    pub async fn poll_result(&self, session_id: &str) -> Result<SessionResult> {
        let url = format!("{}/api/sessions/{}/result", self.base_url, session_id);
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| MantleError::Hub(format!("Failed to poll result: {}", e)))?;

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
            .map_err(|e| MantleError::Hub(format!("Failed to parse result response: {}", e)))
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

    /// Get graph data for visualization.
    pub async fn get_graph(&self) -> Result<GraphData> {
        let url = format!("{}/api/graph", self.base_url);
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
}
