//! Typed HTTP-over-UDS client for communicating with the exomonad server.
//!
//! Callers work with JSON types, not raw bytes or HTTP status codes.

use anyhow::{Context, Result};
use exomonad_core::mcp::ToolDefinition;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_json::Value;
use std::path::PathBuf;

/// Walk up from CWD to find `.exo/server.sock`. Follows symlinks.
pub fn find_server_socket() -> Result<PathBuf> {
    let start = std::env::current_dir()?;
    let mut current = start.as_path();
    loop {
        let sock = current.join(".exo/server.sock");
        if sock.exists() {
            return Ok(sock);
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => {
                return Err(anyhow::anyhow!(
                    "No .exo/server.sock found (walked up from {}). Is exomonad serve running?",
                    start.display()
                ));
            }
        }
    }
}

/// Request body for calling a tool via the REST API.
#[derive(Serialize)]
pub struct ToolCallRequest {
    pub name: String,
    pub arguments: Value,
}

/// Response from the tools list endpoint.
#[derive(Deserialize)]
struct ToolListResponse {
    tools: Vec<ToolDefinition>,
}

/// Client for the exomonad UDS server.
pub struct ServerClient {
    socket: PathBuf,
}

impl ServerClient {
    pub fn new(socket: PathBuf) -> Self {
        Self { socket }
    }

    /// Check if the server is alive and responding.
    pub async fn is_healthy(&self) -> bool {
        self.get("/health").await.is_ok()
    }

    /// List available tools for an agent.
    pub async fn list_tools(&self, role: &str, name: &str) -> Result<Vec<ToolDefinition>> {
        let path = format!("/agents/{}/{}/tools", role, name);
        let resp: ToolListResponse = self.get_json(&path).await?;
        Ok(resp.tools)
    }

    /// Call a tool via the REST API.
    pub async fn call_tool(
        &self,
        role: &str,
        name: &str,
        request: &ToolCallRequest,
    ) -> Result<exomonad_core::mcp::tools::MCPCallOutput> {
        let path = format!("/agents/{}/{}/tools/call", role, name);
        self.post_json(&path, request).await
    }

    /// POST typed JSON, receive typed JSON response.
    pub async fn post_json<T: Serialize, R: DeserializeOwned>(
        &self,
        path: &str,
        body: &T,
    ) -> Result<R> {
        let body_bytes = serde_json::to_vec(body).context("Failed to serialize request")?;
        let resp = self.raw_post(path, &body_bytes).await?;
        serde_json::from_slice(&resp)
            .with_context(|| format!("Failed to deserialize response from {}", path))
    }

    /// GET typed JSON response.
    async fn get_json<R: DeserializeOwned>(&self, path: &str) -> Result<R> {
        let resp = self.raw_get(path).await?;
        serde_json::from_slice(&resp)
            .with_context(|| format!("Failed to deserialize response from {}", path))
    }

    /// GET request, returns Ok if 2xx.
    async fn get(&self, path: &str) -> Result<()> {
        let _ = self.raw_get(path).await?;
        Ok(())
    }

    /// Low-level GET over UDS. Returns response body bytes.
    async fn raw_get(&self, path: &str) -> Result<Vec<u8>> {
        use hyper::Method;
        self.raw_request(Method::GET, path, None).await
    }

    /// Low-level POST over UDS. Returns response body bytes.
    async fn raw_post(&self, path: &str, body: &[u8]) -> Result<Vec<u8>> {
        use hyper::Method;
        self.raw_request(Method::POST, path, Some(body)).await
    }

    /// Shared low-level request logic over UDS.
    async fn raw_request(
        &self,
        method: hyper::Method,
        path: &str,
        body: Option<&[u8]>,
    ) -> Result<Vec<u8>> {
        use http_body_util::{BodyExt, Full};
        use hyper::Request;
        use hyper_util::rt::TokioIo;
        use tokio::net::UnixStream;

        let stream = UnixStream::connect(&self.socket)
            .await
            .with_context(|| format!("Failed to connect to {}", self.socket.display()))?;
        let io = TokioIo::new(stream);

        let (mut sender, conn) = hyper::client::conn::http1::handshake(io)
            .await
            .context("HTTP handshake failed")?;
        tokio::spawn(async move {
            let _ = conn.await;
        });

        let mut req_builder = Request::builder()
            .method(&method)
            .uri(path)
            .header("host", "localhost");

        if body.is_some() {
            req_builder = req_builder.header("content-type", "application/json");
        }

        let req = if let Some(b) = body {
            req_builder.body(Full::new(hyper::body::Bytes::from(b.to_vec())))?
        } else {
            req_builder.body(Full::new(hyper::body::Bytes::new()))?
        };

        let resp = sender
            .send_request(req)
            .await
            .with_context(|| format!("{} request failed", method))?;
        let status = resp.status().as_u16();
        let resp_body = resp
            .into_body()
            .collect()
            .await
            .context("Failed to read response body")?
            .to_bytes()
            .to_vec();

        if !(200..300).contains(&status) {
            return Err(anyhow::anyhow!(
                "Server returned {} for {}: {}",
                status,
                path,
                String::from_utf8_lossy(&resp_body)
            ));
        }

        Ok(resp_body)
    }
}
