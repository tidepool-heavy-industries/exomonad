//! TCP socket client for control envelope communication.
//!
//! Provides a synchronous TCP client for sending hook events and MCP tool
//! calls to the host control server and receiving responses.
//!
//! ## Design Decision: Synchronous
//!
//! This module uses synchronous I/O (std::net) rather than async.
//! This is a deliberate choice because:
//!
//! 1. Hook commands block anyway - Claude Code waits for hook completion
//! 2. Simpler code without async runtime overhead
//! 3. Each hook invocation is a separate process with one request/response
//!
//! ## Protocol
//!
//! - Transport: TCP (host.docker.internal:<port> from container)
//! - Framing: Newline-delimited JSON (NDJSON)
//! - Flow: Connect -> Write message + newline -> Read response + newline -> Close

use crate::error::{MantleError, Result};
use crate::protocol::{ControlMessage, ControlResponse};
use std::io::{BufRead, BufReader, Write};
use std::net::TcpStream;
use std::time::Duration;
use tracing::{debug, trace};

/// Default timeout for socket operations (30 seconds).
const DEFAULT_TIMEOUT: Duration = Duration::from_secs(30);

/// TCP socket client for control envelope communication.
pub struct ControlSocket {
    stream: TcpStream,
}

impl ControlSocket {
    /// Connect to the control server at the given host and port.
    ///
    /// # Errors
    ///
    /// Returns an error if the connection fails.
    pub fn connect(host: &str, port: u16) -> Result<Self> {
        let addr = format!("{}:{}", host, port);
        debug!(addr = %addr, "Connecting to control server");

        let stream = TcpStream::connect(&addr).map_err(|e| MantleError::TcpConnect {
            addr: addr.clone(),
            source: e,
        })?;

        // Set default timeouts
        stream
            .set_read_timeout(Some(DEFAULT_TIMEOUT))
            .map_err(|e| MantleError::SocketConfig { source: e })?;
        stream
            .set_write_timeout(Some(DEFAULT_TIMEOUT))
            .map_err(|e| MantleError::SocketConfig { source: e })?;

        debug!("Connected to control server");
        Ok(Self { stream })
    }

    /// Set custom read/write timeout.
    pub fn set_timeout(&self, timeout: Duration) -> Result<()> {
        self.stream
            .set_read_timeout(Some(timeout))
            .map_err(|e| MantleError::SocketConfig { source: e })?;
        self.stream
            .set_write_timeout(Some(timeout))
            .map_err(|e| MantleError::SocketConfig { source: e })?;
        Ok(())
    }

    /// Send a message and receive the response.
    ///
    /// Protocol: Write JSON + newline, read JSON + newline.
    pub fn send(&mut self, message: &ControlMessage) -> Result<ControlResponse> {
        // Serialize and send
        let json = serde_json::to_string(message).map_err(MantleError::JsonSerialize)?;
        trace!(json = %json, "Sending message");

        self.stream
            .write_all(json.as_bytes())
            .map_err(|e| MantleError::SocketWrite { source: e })?;
        self.stream
            .write_all(b"\n")
            .map_err(|e| MantleError::SocketWrite { source: e })?;
        self.stream
            .flush()
            .map_err(|e| MantleError::SocketWrite { source: e })?;

        // Read response
        let mut reader = BufReader::new(&self.stream);
        let mut response_line = String::new();

        reader
            .read_line(&mut response_line)
            .map_err(|e| MantleError::SocketRead { source: e })?;

        trace!(response = %response_line.trim(), "Received response");

        let response: ControlResponse = serde_json::from_str(response_line.trim())
            .map_err(|e| MantleError::JsonParse { source: e })?;

        Ok(response)
    }
}

/// Get control server connection info from environment.
///
/// Returns (host, port) if both `MANTLE_CONTROL_HOST` and `MANTLE_CONTROL_PORT` are set.
pub fn control_server_addr() -> Option<(String, u16)> {
    let host = std::env::var("MANTLE_CONTROL_HOST").ok()?;
    let port_str = std::env::var("MANTLE_CONTROL_PORT").ok()?;
    let port = port_str.parse().ok()?;
    Some((host, port))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::TcpListener;
    use std::thread;

    #[test]
    fn test_socket_roundtrip() {
        // Start a simple echo server
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();

        let server_handle = thread::spawn(move || {
            let (stream, _) = listener.accept().unwrap();
            let mut reader = BufReader::new(&stream);
            let mut line = String::new();
            reader.read_line(&mut line).unwrap();

            // Parse the message and echo back a response
            let _msg: ControlMessage = serde_json::from_str(&line).unwrap();

            use crate::protocol::{ControlResponse, HookOutput};
            let response =
                ControlResponse::hook_success(HookOutput::pre_tool_use_allow(None, None));
            let response_json = serde_json::to_string(&response).unwrap();

            use std::io::Write;
            let mut stream = stream;
            writeln!(stream, "{}", response_json).unwrap();
        });

        // Connect and send
        let mut client = ControlSocket::connect("127.0.0.1", port).unwrap();

        use crate::protocol::HookInput;
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
