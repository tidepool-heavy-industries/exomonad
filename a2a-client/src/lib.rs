//! Generic A2A Web Client Library
//!
//! This library provides reusable components and utilities for building web-based
//! frontends for A2A agents. It includes:
//!
//! - Generic task and chat viewing components
//! - SSE/WebSocket streaming helpers
//! - Display formatters for A2A types
//! - Axum route builders
//!
//! # Examples
//!
//! ```rust,no_run
//! use a2a_client::{WebA2AClient, components::TaskView};
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     let client = WebA2AClient::auto_connect("http://localhost:8080").await?;
//!     // Use client to interact with A2A agent...
//!     Ok(())
//! }
//! ```

pub mod components;
pub mod utils;

use a2a_rs::{HttpClient, WebSocketClient};
use std::sync::Arc;

/// Web-friendly A2A client that wraps both HTTP and WebSocket clients
pub struct WebA2AClient {
    pub http: HttpClient,
    pub ws: Option<Arc<WebSocketClient>>,
}

impl WebA2AClient {
    /// Create a new client with HTTP only
    pub fn new_http(base_url: String) -> Self {
        Self {
            http: HttpClient::new(base_url),
            ws: None,
        }
    }

    /// Create a new client with both HTTP and WebSocket
    pub fn new_with_websocket(http_url: String, ws_url: String) -> Self {
        Self {
            http: HttpClient::new(http_url),
            ws: Some(Arc::new(WebSocketClient::new(ws_url))),
        }
    }

    /// Auto-connect to an agent, detecting available transports
    pub async fn auto_connect(base_url: &str) -> anyhow::Result<Self> {
        // For now, just use HTTP
        // TODO: Try to detect WebSocket support
        Ok(Self::new_http(base_url.to_string()))
    }

    /// Check if WebSocket is available
    pub fn has_websocket(&self) -> bool {
        self.ws.is_some()
    }

    /// Get WebSocket client if available
    pub fn websocket(&self) -> Option<&Arc<WebSocketClient>> {
        self.ws.as_ref()
    }
}

/// Application state for Axum web applications
pub struct AppState {
    pub client: WebA2AClient,
    pub webhook_token: Option<String>,
}

impl AppState {
    pub fn new(client: WebA2AClient) -> Self {
        Self {
            client,
            webhook_token: None,
        }
    }

    pub fn with_webhook_token(mut self, token: String) -> Self {
        self.webhook_token = Some(token);
        self
    }
}
