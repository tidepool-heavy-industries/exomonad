//! Unix socket client for control envelope communication.
//!
//! Provides a synchronous Unix socket client for sending hook events and MCP tool
//! calls to the host control server and receiving responses.
//!
//! ## Design Decision: Synchronous
//!
//! This module uses synchronous I/O (std::os::unix::net) rather than async.
//! This is a deliberate choice because:
//! 
//! 1. Hook commands block anyway - Claude Code waits for hook completion
//! 2. Simpler code without async runtime overhead
//! 3. Each hook invocation is a separate process with one request/response
//! 
//! ## Protocol
//!
//! - Transport: Unix Domain Socket
//! - Framing: Newline-delimited JSON (NDJSON)
//! - Flow: Connect -> Write message + newline -> Read response + newline -> Close

use crate::error::{MantleError, Result};
use crate::protocol::{ControlMessage, ControlResponse};
use std::io::{BufRead, BufReader, Write};
#[cfg(unix)]
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tracing::{debug, trace};

/// Default timeout for socket operations (30 seconds).
const DEFAULT_TIMEOUT: Duration = Duration::from_secs(30);

/// Unix socket client for control envelope communication.
pub struct ControlSocket {
    #[cfg(unix)]
    stream: UnixStream,
    #[cfg(not(unix))]
    _phantom: std::marker::PhantomData<()>
}

impl ControlSocket {
    /// Connect to the control server at the given socket path.
    ///
    /// # Errors
    ///
    /// Returns an error if the connection fails or if the platform is not supported.
    pub fn connect<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = path.as_ref();
        debug!(path = %path.display(), "Connecting to control server");

        #[cfg(unix)]
        {
            let stream = UnixStream::connect(path).map_err(|e| MantleError::UnixConnect {
                path: path.to_path_buf(),
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
        #[cfg(not(unix))]
        {
            let _ = path;
            Err(MantleError::Io(std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                "Unix sockets are only supported on Unix systems",
            )))
        }
    }

    /// Set custom read/write timeout.
    pub fn set_timeout(&self, timeout: Duration) -> Result<()> {
        #[cfg(unix)]
        {
            self.stream
                .set_read_timeout(Some(timeout))
                .map_err(|e| MantleError::SocketConfig { source: e })?;
            self.stream
                .set_write_timeout(Some(timeout))
                .map_err(|e| MantleError::SocketConfig { source: e })?;
            Ok(())
        }
        #[cfg(not(unix))] 
        {
            let _ = timeout;
            Ok(())
        }
    }

    /// Send a message and receive the response.
    ///
    /// Protocol: Write JSON + newline, read JSON + newline.
    pub fn send(&mut self, message: &ControlMessage) -> Result<ControlResponse> {
        #[cfg(unix)]
        {
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
        #[cfg(not(unix))] 
        {
            let _ = message;
            Err(MantleError::Io(std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                "Unix sockets are only supported on Unix systems",
            )))
        }
    }
}

/// Get control server socket path from environment.
///
/// Returns absolute path:
/// 1. TIDEPOOL_CONTROL_SOCKET (if set)
/// 2. $TIDEPOOL_PROJECT_DIR/.tidepool/sockets/control.sock
/// 3. $PWD/.tidepool/sockets/control.sock
pub fn control_socket_path() -> PathBuf {
    if let Ok(path) = std::env::var("TIDEPOOL_CONTROL_SOCKET") {
        return PathBuf::from(path);
    }

    let base = std::env::var("TIDEPOOL_PROJECT_DIR")
        .map(PathBuf::from)
        .or_else(|_| std::env::current_dir())
        .unwrap_or_else(|_| PathBuf::from("."));

    base.join(".tidepool").join("sockets").join("control.sock")
}

#[cfg(all(test, unix))] 
mod tests {
    use super::*;
    use std::os::unix::net::UnixListener;
    use std::thread;
    use tempfile::tempdir;

    #[test]
    fn test_socket_roundtrip() {
        let dir = tempdir().unwrap();
        let socket_path = dir.path().join("control.sock");
        let socket_path_inner = socket_path.clone();

        // Start a simple echo server
        let listener = UnixListener::bind(&socket_path).unwrap();

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
        let mut client = ControlSocket::connect(&socket_path_inner).unwrap();

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
            runtime: crate::protocol::Runtime::Claude,
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
