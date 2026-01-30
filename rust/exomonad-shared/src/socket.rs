//! Unix socket client for control envelope communication.
//!
//! Provides an async client for sending hook events and MCP tool calls
//! to the host control server via HTTP over Unix Domain Socket.
//!
//! ## Protocol
//!
//! - Transport: HTTP/1.1 over Unix Domain Socket
//! - Implementation: Uses `hyper` + `tokio`
//!
//! Endpoints:
//! - POST /hook      (HookEvent)
//! - POST /mcp/call  (McpToolCall)
//! - GET  /mcp/tools (ToolsListRequest)
//! - GET  /ping      (Ping)

use crate::error::{ExoMonadError, Result};
use crate::protocol::{ControlMessage, ControlResponse};
use http_body_util::{BodyExt, Full};
use hyper::body::Bytes;
use hyper::Request;
use hyper_util::rt::TokioIo;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tokio::net::UnixStream;
use tracing::{debug, error, trace};

/// Unix socket client for control envelope communication.
pub struct ControlSocket {
    socket_path: PathBuf,
    #[allow(dead_code)] // Used in future for custom timeouts
    timeout: Duration,
}

impl ControlSocket {
    /// Create a new control socket client.
    ///
    /// Does not establish a connection immediately; connection is established per-request.
    pub fn connect<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = path.as_ref().to_path_buf();
        debug!(path = %path.display(), "Initializing control socket client");

        Ok(Self {
            socket_path: path,
            timeout: Duration::from_secs(300), // 5 min default matching server
        })
    }

    /// Set custom timeout.
    pub fn set_timeout(&mut self, timeout: Duration) -> Result<()> {
        debug!(timeout = ?timeout, "Setting custom control socket timeout");
        self.timeout = timeout;
        Ok(())
    }

    /// Send a message and receive the response.
    ///
    /// Establishes a fresh connection to the Unix socket for each request (HTTP/1.1).
    pub async fn send(&self, message: &ControlMessage) -> Result<ControlResponse> {
        // Prepare request parameters
        let (method, endpoint, body_bytes) = match message {
            ControlMessage::HookEvent {
                input,
                runtime,
                role,
                container_id,
            } => {
                let body = serde_json::json!([input, runtime, role, container_id]);
                let bytes = serde_json::to_vec(&body).map_err(ExoMonadError::JsonSerialize)?;
                ("POST", "/hook".to_string(), Some(bytes))
            }
            ControlMessage::McpToolCall {
                id,
                tool_name,
                arguments,
                role,
                container_id,
            } => {
                let mut endpoint = match role {
                    Some(r) => format!("/role/{}/mcp/call", r),
                    None => "/mcp/call".to_string(),
                };
                
                if let (Some(_), Some(cid)) = (role, container_id) {
                    endpoint.push_str(&format!("?container={}", cid));
                }

                let body = serde_json::json!({
                    "id": id,
                    "tool_name": tool_name,
                    "arguments": arguments,
                    "container_id": container_id
                });
                let bytes = serde_json::to_vec(&body).map_err(ExoMonadError::JsonSerialize)?;
                ("POST", endpoint, Some(bytes))
            }
            ControlMessage::ToolsListRequest { role } => {
                let endpoint = match role {
                    Some(r) => format!("/role/{}/mcp/tools", r),
                    None => "/mcp/tools".to_string(),
                };
                ("GET", endpoint, None)
            }
            ControlMessage::Ping => ("GET", "/ping".to_string(), None),
        };

        // Connect to UDS
        let stream = UnixStream::connect(&self.socket_path).await.map_err(|e| {
            ExoMonadError::UnixConnect {
                path: self.socket_path.clone(),
                source: e,
            }
        })?;

        // Handshake HTTP/1.1
        let io = TokioIo::new(stream);
        let (mut sender, conn) = hyper::client::conn::http1::handshake(io).await?;

        // Spawn a task to poll the connection
        tokio::task::spawn(async move {
            if let Err(err) = conn.await {
                error!("Connection failed: {:?}", err);
            }
        });

        // Build request
        let url = format!("http://localhost{}", endpoint);
        let mut builder = Request::builder()
            .method(method)
            .uri(&url)
            .header("Host", "localhost");

        if body_bytes.is_some() {
            builder = builder.header("Content-Type", "application/json");
        }

        let body = match body_bytes {
            Some(b) => Full::new(Bytes::from(b)),
            None => Full::new(Bytes::new()),
        };

        let req = builder.body(body)?;

        trace!(url = %url, "Sending HTTP request");

        // Send request
        let res = sender.send_request(req).await?;

        if !res.status().is_success() {
            let status = res.status();
            // Try to read body for error message
            let body = res.collect().await?.to_bytes();
            let body_str = String::from_utf8_lossy(&body);
            return Err(ExoMonadError::McpServer(format!(
                "HTTP {} error: {}",
                status, body_str
            )));
        }

        // Read response body
        let body = res.collect().await?.to_bytes();
        trace!(response_len = body.len(), "Received response");

        let response: ControlResponse = serde_json::from_slice(&body).map_err(|e| {
            error!(body = %String::from_utf8_lossy(&body), "Failed to parse response JSON");
            ExoMonadError::JsonParse { source: e }
        })?;

        Ok(response)
    }
}

/// Get control server socket path from environment.
pub fn control_socket_path() -> Result<PathBuf> {
    std::env::var("EXOMONAD_CONTROL_SOCKET")
        .map(PathBuf::from)
        .map_err(|_| ExoMonadError::UnixConnect {
            path: PathBuf::from("UNKNOWN"),
            source: std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "EXOMONAD_CONTROL_SOCKET environment variable not set. This should be set via Docker or .env"
            ),
        })
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::io::{Read, Write};
    use std::os::unix::net::UnixListener;
    use std::thread;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_socket_roundtrip() {
        let dir = tempdir().unwrap();
        let socket_path = dir.path().join("control.sock");
        let socket_path_inner = socket_path.clone();

        // Start a simple HTTP-over-Unix echo server
        // Note: Using blocking listener for test simplicity, but client is async
        let listener = UnixListener::bind(&socket_path).unwrap();

        let server_handle = thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();

            // Read request (just consume it)
            let mut buf = [0u8; 1024];
            let _ = stream.read(&mut buf).unwrap();

            // Construct response
            use crate::protocol::{ControlResponse, HookOutput};
            let response_obj =
                ControlResponse::hook_success(HookOutput::pre_tool_use_allow(None, None));
            let response_json = serde_json::to_string(&response_obj).unwrap();

            // Minimal HTTP response
            let http_response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{}\r\n",
                response_json.len(),
                response_json
            );

            stream.write_all(http_response.as_bytes()).unwrap();
        });

        // Connect and send
        let client = ControlSocket::connect(&socket_path_inner).unwrap();

        use crate::protocol::{HookInput, Role};
        let message = ControlMessage::HookEvent {
            input: Box::new(HookInput {
                session_id: "test".to_string(),
                transcript_path: String::new(),
                cwd: String::new(),
                permission_mode: "default".to_string(),
                hook_event_name: "PreToolUse".to_string(),
                tool_name: Some("Write".to_string()),
                tool_input: None,
                tool_use_id: None,
                tool_response: None,
                prompt: None,
                message: None,
                notification_type: None,
                stop_hook_active: None,
                trigger: None,
                custom_instructions: None,
                source: None,
                reason: None,
            }),
            runtime: crate::protocol::Runtime::Claude,
            role: Role::Dev,
            container_id: None,
        };

        let response = client.send(&message).await.unwrap();

        match response {
            ControlResponse::HookResponse { exit_code, .. } => {
                assert_eq!(exit_code, 0);
            }
            _ => panic!("Unexpected response type"),
        }

        server_handle.join().unwrap();
    }
}
