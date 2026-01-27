//! Unix socket client for control envelope communication.
//! 
//! Provides a synchronous client for sending hook events and MCP tool calls
//! to the host control server via HTTP over Unix Domain Socket.
//! 
//! ## Protocol
//! 
//! - Transport: HTTP/1.1 over Unix Domain Socket
//! - Implementation: Uses `curl` subprocess to avoid adding async dependencies (hyper/reqwest)
//!   to the synchronous exomonad CLI.
//! 
//!   NOTE: This implementation requires `curl` to be available in the system PATH.
//! 
//! Endpoints:
//! - POST /hook      (HookEvent)
//! - POST /mcp/call  (McpToolCall)
//! - GET  /mcp/tools (ToolsListRequest)
//! - GET  /ping      (Ping)

use crate::error::{ExoMonadError, Result};
use crate::protocol::{ControlMessage, ControlResponse};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;
use tracing::{debug, error, trace};

/// Unix socket client for control envelope communication.
pub struct ControlSocket {
    socket_path: PathBuf,
    #[allow(dead_code)] // Used in future for custom timeouts
    timeout: Duration,
}

impl ControlSocket {
    /// Connect to the control server at the given socket path.
    ///
    /// For this implementation, "connect" just validates the path and stores it,
    /// as `curl` will establish a fresh connection for each request.
    pub fn connect<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = path.as_ref().to_path_buf();
        debug!(path = %path.display(), "Initializing control socket client");
        
        // Simple validation that path exists (optional, curl would fail anyway)
        if !path.exists() {
             // Don't fail hard here if socket doesn't exist yet (race condition?), 
             // but maybe we should. For now, just logging.
             debug!("Socket path does not exist yet: {:?}", path);
        }

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
    /// Executes `curl --unix-socket ...` to perform the HTTP request.
    pub fn send(&mut self, message: &ControlMessage) -> Result<ControlResponse> {
        let (method, endpoint, body_json) = match message {
            ControlMessage::HookEvent { input, runtime, role } => {
                // API expects [HookInput, Runtime, Role]
                let body = serde_json::json!([input, runtime, role]);
                ("POST", "/hook".to_string(), Some(body))
            },
            ControlMessage::McpToolCall { id, tool_name, arguments, role } => {
                // API expects {"id": ..., "tool_name": ..., "arguments": ...}
                let endpoint = match role {
                    Some(r) => format!("/role/{}/mcp/call", r),
                    None => "/mcp/call".to_string(),
                };
                let body = serde_json::json!({
                    "id": id,
                    "tool_name": tool_name,
                    "arguments": arguments
                });
                ("POST", endpoint, Some(body))
            },
            ControlMessage::ToolsListRequest { role } => {
                let endpoint = match role {
                    Some(r) => format!("/role/{}/mcp/tools", r),
                    None => "/mcp/tools".to_string(),
                };
                ("GET", endpoint, None)
            },
            ControlMessage::Ping => {
                ("GET", "/ping".to_string(), None)
            }
        };

        let url = format!("http://localhost{}", endpoint);
        
        let mut cmd = Command::new("curl");
        cmd.arg("--unix-socket").arg(&self.socket_path)
           .arg("-s") // Silent
           .arg("-S") // Show error if fails
           .arg("-X").arg(method)
           .arg("--max-time").arg(self.timeout.as_secs().to_string())
           .arg(&url);

        if let Some(body) = body_json {
            let body_str = serde_json::to_string(&body).map_err(ExoMonadError::JsonSerialize)?;
            trace!(url = %url, body_len = body_str.len(), "Sending HTTP request");
            
            cmd.arg("-H").arg("Content-Type: application/json")
               .arg("-d").arg(body_str);
        } else {
            trace!(url = %url, "Sending HTTP request");
        }

        let output = cmd.output().map_err(ExoMonadError::Io)?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                return Err(ExoMonadError::Io(std::io::Error::other(format!(
                    "curl failed: {}",
                    stderr
                ))));
            }

        let stdout = String::from_utf8_lossy(&output.stdout);
        trace!(response_len = stdout.len(), "Received response");

        let response: ControlResponse = serde_json::from_str(&stdout)
            .map_err(|e| {
                error!(body = %stdout, "Failed to parse response JSON");
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
                "EXOMONAD_CONTROL_SOCKET environment variable not set. This should be set via start-augmented.sh or .env"
            ),
        })
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::net::UnixListener;
    use std::thread;
    use tempfile::tempdir;

    #[test]
    fn test_socket_roundtrip() {
        // Ensure curl is installed
        if Command::new("curl").arg("--version").output().is_err() {
            println!("Skipping test: curl not found");
            return;
        }

        let dir = tempdir().unwrap();
        let socket_path = dir.path().join("control.sock");
        let socket_path_inner = socket_path.clone();

        // Start a simple HTTP-over-Unix echo server
        let listener = UnixListener::bind(&socket_path).unwrap();

        let server_handle = thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            
            // Read request (just consume it, don't parse strictly for this test)
            use std::io::Read;
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

            use std::io::Write;
            stream.write_all(http_response.as_bytes()).unwrap();
        });

        // Connect and send
        let mut client = ControlSocket::connect(&socket_path_inner).unwrap();

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
        };

        let response = client.send(&message).unwrap();

        match response {
            ControlResponse::HookResponse { exit_code, .. } => {
                assert_eq!(exit_code, 0);
            }
            _ => panic!("Unexpected response type"),
        }

        server_handle.join().unwrap();
    }
}