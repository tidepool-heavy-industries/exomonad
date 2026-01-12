//! Control listener for container↔host communication via TCP.
//!
//! Provides a synchronous TCP server that handles MCP tool calls from
//! `mantle-agent mcp` running inside Docker containers. Tool calls are
//! accumulated and returned when the container exits.
//!
//! ## Protocol
//!
//! - Transport: TCP (127.0.0.1:0, OS-assigned port)
//! - Framing: Newline-delimited JSON (NDJSON)
//! - Flow: Accept -> Read message + newline -> Process -> Write response + newline
//!
//! ## Design
//!
//! The listener runs in a background thread, accepting connections and handling
//! messages synchronously. Tool calls are accumulated in a shared `Vec<ToolCall>`
//! protected by a mutex. When the container exits, the caller retrieves the
//! accumulated tool calls.
//!
//! ## Decision Tool Termination
//!
//! When a `decision::` prefixed tool is called, the listener:
//! 1. Records the tool call as usual
//! 2. Sends the tool call on a decision channel (notifies main thread)
//! 3. Returns a strong "end session now" response to Claude
//!
//! The main thread uses this signal to set a 30-second deadline for graceful
//! termination. If Claude doesn't exit within the deadline, the container is killed.

use mantle_shared::events::ToolCall;
use mantle_shared::protocol::{ControlMessage, ControlResponse};
use serde_json::json;
use std::io::{BufRead, BufReader, ErrorKind, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;
use tracing::{debug, error, info, trace, warn};

/// TCP control listener for MCP tool calls.
///
/// Listens on a TCP port and handles `McpToolCall` messages from
/// `mantle-agent mcp` running inside containers.
pub struct ControlListener {
    port: u16,
    listener: Option<TcpListener>,
}

/// Handle to accumulated tool calls.
///
/// Returned by `ControlListener::spawn()`. Use `collect()` to retrieve
/// the accumulated tool calls after the container exits.
pub struct ToolCallCollector {
    tool_calls: Arc<Mutex<Vec<ToolCall>>>,
    handle: JoinHandle<()>,
    shutdown: Arc<AtomicBool>,
}

impl ControlListener {
    /// Create a new listener bound to an OS-assigned port on localhost.
    ///
    /// The port can be retrieved via `port()` after binding.
    pub fn bind() -> std::io::Result<Self> {
        // Bind to localhost with port 0 (OS assigns available port)
        let listener = TcpListener::bind("127.0.0.1:0")?;
        let port = listener.local_addr()?.port();

        info!(port = port, "Control listener bound to TCP port");

        Ok(Self {
            port,
            listener: Some(listener),
        })
    }

    /// Get the port this listener is bound to.
    pub fn port(&self) -> u16 {
        self.port
    }

    /// Spawn the listener thread and return a handle to collect tool calls.
    ///
    /// The listener runs until shutdown is signaled via the collector.
    ///
    /// Returns a tuple of:
    /// - `ToolCallCollector`: Handle to accumulated tool calls
    /// - `Receiver<ToolCall>`: Channel that fires when a `decision::` tool is called
    ///
    /// The receiver is used by the main thread to detect decision tool calls
    /// and set a termination deadline.
    pub fn spawn(mut self) -> (ToolCallCollector, Receiver<ToolCall>) {
        let tool_calls = Arc::new(Mutex::new(Vec::new()));
        let tool_calls_clone = Arc::clone(&tool_calls);
        let shutdown = Arc::new(AtomicBool::new(false));
        let shutdown_clone = Arc::clone(&shutdown);

        // Channel for notifying main thread when decision tool is called
        let (decision_tx, decision_rx) = mpsc::channel();

        // Take the listener out of self (allows move into thread while self gets dropped)
        let listener = self.listener.take().expect("Listener already taken");

        // Set non-blocking mode so we can poll for shutdown
        listener
            .set_nonblocking(true)
            .expect("Failed to set non-blocking mode");

        let handle = thread::spawn(move || {
            Self::run_listener(listener, tool_calls_clone, shutdown_clone, decision_tx);
        });

        let collector = ToolCallCollector {
            tool_calls,
            handle,
            shutdown,
        };
        (collector, decision_rx)
    }

    /// Run the listener loop.
    ///
    /// Polls for connections in non-blocking mode, checking the shutdown flag periodically.
    fn run_listener(
        listener: TcpListener,
        tool_calls: Arc<Mutex<Vec<ToolCall>>>,
        shutdown: Arc<AtomicBool>,
        decision_tx: Sender<ToolCall>,
    ) {
        debug!("Control listener thread started");

        while !shutdown.load(Ordering::Relaxed) {
            match listener.accept() {
                Ok((stream, addr)) => {
                    debug!(addr = %addr, "Accepted TCP connection");
                    Self::handle_connection(stream, &tool_calls, &decision_tx);
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
    fn handle_connection(
        stream: TcpStream,
        tool_calls: &Arc<Mutex<Vec<ToolCall>>>,
        decision_tx: &Sender<ToolCall>,
    ) {
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
        let response = Self::handle_message(message, tool_calls, decision_tx);

        // Send response
        Self::send_response(&stream, &response);
    }

    /// Handle a control message and return the response.
    fn handle_message(
        message: ControlMessage,
        tool_calls: &Arc<Mutex<Vec<ToolCall>>>,
        decision_tx: &Sender<ToolCall>,
    ) -> ControlResponse {
        match message {
            ControlMessage::McpToolCall {
                id,
                tool_name,
                arguments,
            } => {
                let is_decision_tool = tool_name.starts_with("decision::");
                let tool_name_short = tool_name.replace("decision::", "");

                if is_decision_tool {
                    info!(tool = %tool_name, "Decision tool called - notifying main thread");
                } else {
                    info!(tool = %tool_name, "Recording MCP tool call");
                }

                // Record the tool call
                let tool_call = ToolCall {
                    name: tool_name.clone(),
                    input: arguments,
                };
                tool_calls.lock().unwrap().push(tool_call.clone());

                // For decision tools, notify main thread to set termination deadline
                if is_decision_tool {
                    if let Err(e) = decision_tx.send(tool_call) {
                        // Channel closed - main thread already exiting, that's fine
                        debug!(error = %e, "Decision channel send failed (receiver dropped)");
                    }
                }

                // Response text differs for decision vs non-decision tools
                let response_text = if is_decision_tool {
                    format!(
                        "Decision recorded: {}. This decision triggers a graph state transition. \
                         You MUST end this session NOW. Do not perform any more actions, \
                         do not call any more tools, do not write any more files. Simply stop.",
                        tool_name_short
                    )
                } else {
                    format!("Tool call recorded: {}", tool_name_short)
                };

                ControlResponse::McpToolResponse {
                    id,
                    result: Some(json!({
                        "content": [{
                            "type": "text",
                            "text": response_text
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
    fn send_response(stream: &TcpStream, response: &ControlResponse) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use mantle_shared::protocol::ControlMessage;
    use std::io::{BufRead, BufReader, Write};
    use std::net::TcpStream;

    /// Wait for listener to be ready (polling)
    fn wait_for_listener() {
        thread::sleep(Duration::from_millis(50));
    }

    #[test]
    fn test_listener_binds_and_accepts() {
        let listener = ControlListener::bind().unwrap();
        let port = listener.port();
        let (collector, decision_rx) = listener.spawn();
        wait_for_listener();

        // Connect as client
        let mut client = TcpStream::connect(format!("127.0.0.1:{}", port)).unwrap();

        // Send an MCP tool call (decision tool)
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

        // Decision channel should have received the tool call
        let decision_tool = decision_rx.try_recv().unwrap();
        assert_eq!(decision_tool.name, "decision::approve");

        // Collect tool calls
        let tool_calls = collector.collect();
        assert_eq!(tool_calls.len(), 1);
        assert_eq!(tool_calls[0].name, "decision::approve");
    }

    #[test]
    fn test_multiple_tool_calls() {
        let listener = ControlListener::bind().unwrap();
        let port = listener.port();
        let (collector, _decision_rx) = listener.spawn();
        wait_for_listener();

        // Send multiple tool calls from separate connections
        for i in 0..3 {
            let mut client = TcpStream::connect(format!("127.0.0.1:{}", port)).unwrap();
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
        let listener = ControlListener::bind().unwrap();
        let port = listener.port();
        let (_collector, _decision_rx) = listener.spawn();
        wait_for_listener();

        let mut client = TcpStream::connect(format!("127.0.0.1:{}", port)).unwrap();

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
}
