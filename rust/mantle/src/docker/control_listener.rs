//! Control socket listener for container↔host communication.
//!
//! Provides a synchronous socket server that handles MCP tool calls from
//! `mantle-agent mcp` running inside Docker containers. Tool calls are
//! accumulated and returned when the container exits.
//!
//! ## Protocol
//!
//! - Transport: Unix domain socket (stream)
//! - Framing: Newline-delimited JSON (NDJSON)
//! - Flow: Accept -> Read message + newline -> Process -> Write response + newline
//!
//! ## Design
//!
//! The listener runs in a background thread, accepting connections and handling
//! messages synchronously. Tool calls are accumulated in a shared `Vec<ToolCall>`
//! protected by a mutex. When the container exits, the caller retrieves the
//! accumulated tool calls.

use mantle_shared::events::ToolCall;
use mantle_shared::protocol::{ControlMessage, ControlResponse};
use serde_json::json;
use std::fs;
use std::io::{BufRead, BufReader, ErrorKind, Write};
use std::os::unix::fs::PermissionsExt;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;
use tracing::{debug, error, info, trace, warn};

/// Control socket listener for MCP tool calls.
///
/// Listens on a Unix socket and handles `McpToolCall` messages from
/// `mantle-agent mcp` running inside containers.
pub struct ControlListener {
    socket_path: PathBuf,
    listener: Option<UnixListener>,
}

/// Handle to accumulated tool calls.
///
/// Returned by `ControlListener::spawn()`. Use `collect()` to retrieve
/// the accumulated tool calls after the container exits.
pub struct ToolCallCollector {
    tool_calls: Arc<Mutex<Vec<ToolCall>>>,
    handle: JoinHandle<()>,
    shutdown: Arc<AtomicBool>,
    socket_path: PathBuf,
}

impl ControlListener {
    /// Create a new listener bound to the given socket path.
    ///
    /// Removes any stale socket file before binding.
    /// Sets permissions to 0o666 so containers can connect.
    pub fn bind(socket_path: &Path) -> std::io::Result<Self> {
        // Remove stale socket if it exists
        if socket_path.exists() {
            debug!(path = %socket_path.display(), "Removing stale socket file");
            fs::remove_file(socket_path)?;
        }

        // Bind the listener
        let listener = UnixListener::bind(socket_path)?;

        // Set permissions so container can connect (containers run as different user)
        fs::set_permissions(socket_path, fs::Permissions::from_mode(0o666))?;

        info!(path = %socket_path.display(), "Control socket listener bound");

        Ok(Self {
            socket_path: socket_path.to_path_buf(),
            listener: Some(listener),
        })
    }

    /// Spawn the listener thread and return a handle to collect tool calls.
    ///
    /// The listener runs until shutdown is signaled via the collector.
    pub fn spawn(mut self) -> ToolCallCollector {
        let tool_calls = Arc::new(Mutex::new(Vec::new()));
        let tool_calls_clone = Arc::clone(&tool_calls);
        let shutdown = Arc::new(AtomicBool::new(false));
        let shutdown_clone = Arc::clone(&shutdown);
        let socket_path = self.socket_path.clone();

        // Take the listener out of self (allows move into thread while self gets dropped)
        let listener = self.listener.take().expect("Listener already taken");

        // Set non-blocking mode so we can poll for shutdown
        listener
            .set_nonblocking(true)
            .expect("Failed to set non-blocking mode");

        let handle = thread::spawn(move || {
            Self::run_listener(listener, tool_calls_clone, shutdown_clone);
        });

        ToolCallCollector { tool_calls, handle, shutdown, socket_path }
    }

    /// Run the listener loop.
    ///
    /// Polls for connections in non-blocking mode, checking the shutdown flag periodically.
    fn run_listener(
        listener: UnixListener,
        tool_calls: Arc<Mutex<Vec<ToolCall>>>,
        shutdown: Arc<AtomicBool>,
    ) {
        debug!("Control listener thread started");

        while !shutdown.load(Ordering::Relaxed) {
            match listener.accept() {
                Ok((stream, _)) => {
                    debug!("Accepted connection on control socket");
                    Self::handle_connection(stream, &tool_calls);
                }
                Err(e) if e.kind() == ErrorKind::WouldBlock => {
                    // No pending connection, sleep briefly and check shutdown flag
                    thread::sleep(Duration::from_millis(10));
                }
                Err(e) => {
                    // Real error - log and exit
                    if e.kind() != ErrorKind::Other && e.kind() != ErrorKind::InvalidInput {
                        error!(error = %e, "Error accepting connection");
                    }
                    break;
                }
            }
        }

        debug!("Control listener thread exiting");
    }

    /// Handle a single connection.
    fn handle_connection(stream: UnixStream, tool_calls: &Arc<Mutex<Vec<ToolCall>>>) {
        let mut reader = BufReader::new(&stream);
        let mut line = String::new();

        // Read message
        match reader.read_line(&mut line) {
            Ok(0) => {
                debug!("Connection closed by peer (EOF)");
                return;
            }
            Ok(_) => {}
            Err(e) => {
                error!(error = %e, "Failed to read from control socket");
                return;
            }
        }

        trace!(message = %line.trim(), "Received control message");

        // Parse message
        let message: ControlMessage = match serde_json::from_str(line.trim()) {
            Ok(msg) => msg,
            Err(e) => {
                error!(error = %e, "Failed to parse control message");
                // Send error response
                let response = ControlResponse::hook_error(format!("Parse error: {}", e));
                Self::send_response(&stream, &response);
                return;
            }
        };

        // Handle message
        let response = Self::handle_message(message, tool_calls);

        // Send response
        Self::send_response(&stream, &response);
    }

    /// Handle a control message and return the response.
    fn handle_message(
        message: ControlMessage,
        tool_calls: &Arc<Mutex<Vec<ToolCall>>>,
    ) -> ControlResponse {
        match message {
            ControlMessage::McpToolCall {
                id,
                tool_name,
                arguments,
            } => {
                info!(tool = %tool_name, "Recording MCP tool call");

                // Record the tool call
                tool_calls.lock().unwrap().push(ToolCall {
                    name: tool_name.clone(),
                    input: arguments,
                });

                // Respond with success
                ControlResponse::McpToolResponse {
                    id,
                    result: Some(json!({
                        "content": [{
                            "type": "text",
                            "text": format!("Decision recorded: {}", tool_name.replace("decision::", ""))
                        }]
                    })),
                    error: None,
                }
            }
            ControlMessage::HookEvent { .. } => {
                warn!("HookEvent not supported on this socket");
                ControlResponse::hook_error("Hook events not supported on this socket".to_string())
            }
        }
    }

    /// Send a response to the client.
    fn send_response(stream: &UnixStream, response: &ControlResponse) {
        let json = match serde_json::to_string(response) {
            Ok(j) => j,
            Err(e) => {
                error!(error = %e, "Failed to serialize response");
                return;
            }
        };

        trace!(response = %json, "Sending response");

        let mut stream = stream;
        if let Err(e) = writeln!(stream, "{}", json) {
            error!(error = %e, "Failed to write response");
        }
    }

    /// Get the socket path.
    pub fn socket_path(&self) -> &Path {
        &self.socket_path
    }
}

impl ToolCallCollector {
    /// Collect the accumulated tool calls.
    ///
    /// This consumes the collector and joins the listener thread.
    /// If the thread panicked, logs a warning and returns whatever
    /// tool calls were accumulated before the panic.
    pub fn collect(self) -> Vec<ToolCall> {
        // Signal the listener thread to shut down
        debug!("Signaling listener shutdown");
        self.shutdown.store(true, Ordering::Relaxed);

        // Join the listener thread and check for panics
        match self.handle.join() {
            Ok(()) => {
                debug!("Control listener thread joined successfully");
            }
            Err(panic_info) => {
                // Thread panicked - log the error
                let panic_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                    s.to_string()
                } else if let Some(s) = panic_info.downcast_ref::<String>() {
                    s.clone()
                } else {
                    "unknown panic".to_string()
                };
                error!(
                    panic = %panic_msg,
                    "Control listener thread panicked! Some tool calls may have been lost."
                );
            }
        }

        // Clean up socket file
        if self.socket_path.exists() {
            debug!(path = %self.socket_path.display(), "Cleaning up socket file");
            let _ = fs::remove_file(&self.socket_path);
        }

        // Get the accumulated tool calls
        match Arc::try_unwrap(self.tool_calls) {
            Ok(mutex) => mutex.into_inner().unwrap(),
            Err(arc) => {
                // Shouldn't happen after join(), but handle gracefully
                warn!("Unexpected: Arc still has references after thread join");
                arc.lock().unwrap().clone()
            }
        }
    }

    /// Get a snapshot of current tool calls without consuming.
    pub fn snapshot(&self) -> Vec<ToolCall> {
        self.tool_calls.lock().unwrap().clone()
    }

    /// Check if the listener thread is still running.
    pub fn is_running(&self) -> bool {
        !self.handle.is_finished()
    }
}

impl Drop for ControlListener {
    fn drop(&mut self) {
        // Only clean up socket if listener wasn't spawned (i.e., still present)
        // If spawn() was called, the socket is owned by the listener thread
        if self.listener.is_some() && self.socket_path.exists() {
            debug!(path = %self.socket_path.display(), "Cleaning up socket file");
            let _ = fs::remove_file(&self.socket_path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mantle_shared::protocol::ControlMessage;
    use std::io::{BufRead, BufReader, Write};
    use std::os::unix::net::UnixStream;

    fn create_temp_socket() -> (PathBuf, tempfile::TempDir) {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.sock");
        (path, dir)
    }

    /// Wait for listener to be ready (polling)
    fn wait_for_listener() {
        thread::sleep(Duration::from_millis(50));
    }

    #[test]
    fn test_listener_binds_and_accepts() {
        let (socket_path, _dir) = create_temp_socket();

        let listener = ControlListener::bind(&socket_path).unwrap();
        let collector = listener.spawn();
        wait_for_listener();

        // Connect as client
        let mut client = UnixStream::connect(&socket_path).unwrap();

        // Send an MCP tool call
        let message = ControlMessage::McpToolCall {
            id: "test-1".to_string(),
            tool_name: "decision::approve".to_string(),
            arguments: json!({"notes": "LGTM"}),
        };
        let json = serde_json::to_string(&message).unwrap();
        writeln!(client, "{}", json).unwrap();

        // Read response
        let mut reader = BufReader::new(&client);
        let mut response_line = String::new();
        reader.read_line(&mut response_line).unwrap();

        let response: ControlResponse = serde_json::from_str(&response_line).unwrap();
        match response {
            ControlResponse::McpToolResponse { id, error, .. } => {
                assert_eq!(id, "test-1");
                assert!(error.is_none());
            }
            _ => panic!("Expected McpToolResponse"),
        }

        // Collect tool calls
        let tool_calls = collector.collect();
        assert_eq!(tool_calls.len(), 1);
        assert_eq!(tool_calls[0].name, "decision::approve");
    }

    #[test]
    fn test_multiple_tool_calls() {
        let (socket_path, _dir) = create_temp_socket();

        let listener = ControlListener::bind(&socket_path).unwrap();
        let collector = listener.spawn();
        wait_for_listener();

        // Send multiple tool calls from separate connections
        for i in 0..3 {
            let mut client = UnixStream::connect(&socket_path).unwrap();
            let message = ControlMessage::McpToolCall {
                id: format!("test-{}", i),
                tool_name: format!("decision::option_{}", i),
                arguments: json!({}),
            };
            writeln!(client, "{}", serde_json::to_string(&message).unwrap()).unwrap();

            // Drain response
            let mut reader = BufReader::new(&client);
            let mut response = String::new();
            reader.read_line(&mut response).unwrap();

            // Small delay between connections to avoid race
            thread::sleep(Duration::from_millis(20));
        }

        let tool_calls = collector.collect();
        assert_eq!(tool_calls.len(), 3);
    }

    #[test]
    fn test_hook_event_returns_error() {
        let (socket_path, _dir) = create_temp_socket();

        let listener = ControlListener::bind(&socket_path).unwrap();
        let _collector = listener.spawn();
        wait_for_listener();

        let mut client = UnixStream::connect(&socket_path).unwrap();

        use mantle_shared::protocol::HookInput;
        let message = ControlMessage::HookEvent {
            input: Box::new(HookInput {
                session_id: "test".to_string(),
                transcript_path: String::new(),
                cwd: String::new(),
                permission_mode: "default".to_string(),
                hook_event_name: "PreToolUse".to_string(),
                tool_name: None,
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
        writeln!(client, "{}", serde_json::to_string(&message).unwrap()).unwrap();

        let mut reader = BufReader::new(&client);
        let mut response_line = String::new();
        reader.read_line(&mut response_line).unwrap();

        let response: ControlResponse = serde_json::from_str(&response_line).unwrap();
        match response {
            ControlResponse::HookResponse { exit_code, .. } => {
                assert_eq!(exit_code, 2); // Error exit code
            }
            _ => panic!("Expected HookResponse"),
        }
    }

    #[test]
    fn test_socket_cleanup_on_drop() {
        let (socket_path, _dir) = create_temp_socket();

        {
            let _listener = ControlListener::bind(&socket_path).unwrap();
            assert!(socket_path.exists());
        }

        // Socket should be cleaned up after drop
        assert!(!socket_path.exists());
    }
}
