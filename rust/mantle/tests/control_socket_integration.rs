//! Integration tests for control socket communication with Haskell server.
//!
//! These tests spawn the Haskell `control-socket-test-server` and verify
//! the Rust client can communicate with it correctly.

use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::Duration;

use mantle::protocol::{ControlMessage, ControlResponse, HookInput, HookSpecificOutput};
use mantle::socket::ControlSocket;
use serde_json::json;

/// Helper to create a HookInput with defaults filled in.
fn make_hook_input(event_name: &str) -> HookInput {
    HookInput {
        session_id: "test-session".to_string(),
        transcript_path: String::new(),
        cwd: "/tmp".to_string(),
        permission_mode: "default".to_string(),
        hook_event_name: event_name.to_string(),
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
    }
}

/// Find the built test server executable.
fn find_test_server() -> PathBuf {
    // dist-newstyle is at the silas level (parent of tools/)
    // CARGO_MANIFEST_DIR = .../silas/tools/mantle
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let silas_dir = manifest_dir
        .parent()
        .unwrap() // tools/
        .parent()
        .unwrap(); // silas/

    // Find the executable in dist-newstyle (platform-agnostic)
    let dist = silas_dir.join("dist-newstyle/build");

    // Find platform directory (e.g., x86_64-linux, aarch64-darwin, etc.)
    for platform_entry in std::fs::read_dir(&dist).expect("dist-newstyle/build not found") {
        let platform_entry = platform_entry.unwrap();
        let platform_path = platform_entry.path();
        if !platform_path.is_dir() {
            continue;
        }

        // Find ghc version directory
        if let Ok(ghc_entries) = std::fs::read_dir(&platform_path) {
            for entry in ghc_entries {
                let entry = entry.unwrap();
                let path = entry.path();
                if path.is_dir()
                    && path
                        .file_name()
                        .unwrap()
                        .to_string_lossy()
                        .starts_with("ghc-")
                {
                    let exe = path.join(
                        "tidepool-claude-code-executor-0.1.0.0/x/control-socket-test-server/build/control-socket-test-server/control-socket-test-server",
                    );
                    if exe.exists() {
                        return exe;
                    }
                }
            }
        }
    }

    panic!(
        "Test server not found. Run `cabal build control-socket-test-server` first.\n\
         Searched in: {:?}",
        dist
    );
}

/// Spawn the Haskell test server and return the socket path.
struct TestServer {
    child: Child,
    socket_path: PathBuf,
}

impl TestServer {
    fn spawn() -> Self {
        let exe = find_test_server();

        let mut child = Command::new(&exe)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .expect("Failed to spawn test server");

        // Read socket path from stdout
        let stdout = child.stdout.take().unwrap();
        let mut reader = BufReader::new(stdout);
        let mut socket_path = String::new();
        reader
            .read_line(&mut socket_path)
            .expect("Failed to read socket path");
        let socket_path = PathBuf::from(socket_path.trim());

        // Give the server a moment to start listening
        std::thread::sleep(Duration::from_millis(100));

        TestServer { child, socket_path }
    }
}

impl Drop for TestServer {
    fn drop(&mut self) {
        // Close stdin to signal the server to exit
        if let Some(stdin) = self.child.stdin.take() {
            drop(stdin);
        }
        // Wait for clean exit
        let _ = self.child.wait();
    }
}

#[test]
fn test_hook_allow() {
    let server = TestServer::spawn();

    let mut socket =
        ControlSocket::connect(&server.socket_path).expect("Failed to connect to socket");

    // Send a PreToolUse hook event that should be allowed
    let mut input = make_hook_input("PreToolUse");
    input.tool_name = Some("Read".to_string());
    input.tool_input = Some(json!({"file_path": "/tmp/test.txt"}));

    let message = ControlMessage::HookEvent {
        input: Box::new(input),
    };

    let response = socket.send(&message).expect("Failed to send message");

    match response {
        ControlResponse::HookResponse { output, exit_code } => {
            assert_eq!(exit_code, 0, "Expected exit code 0 for allowed hook");
            assert!(output.continue_, "Expected continue=true for allowed hook");
            // PreToolUse should have permission_decision = "allow"
            if let Some(HookSpecificOutput::PreToolUse {
                permission_decision,
                ..
            }) = &output.hook_specific_output
            {
                assert_eq!(
                    permission_decision, "allow",
                    "Expected permissionDecision=allow"
                );
            }
        }
        _ => panic!("Expected HookResponse, got {:?}", response),
    }
}

#[test]
fn test_hook_deny() {
    let server = TestServer::spawn();

    let mut socket =
        ControlSocket::connect(&server.socket_path).expect("Failed to connect to socket");

    // Send a PreToolUse hook event for "test_deny" which should be denied
    let mut input = make_hook_input("PreToolUse");
    input.tool_name = Some("test_deny".to_string());
    input.tool_input = Some(json!({}));

    let message = ControlMessage::HookEvent {
        input: Box::new(input),
    };

    let response = socket.send(&message).expect("Failed to send message");

    match response {
        ControlResponse::HookResponse { output, exit_code } => {
            assert_eq!(exit_code, 2, "Expected exit code 2 for denied hook");
            assert!(
                output.continue_,
                "Expected continue=true even for denied hook"
            );
            // PreToolUse denial should have permission_decision = "deny"
            if let Some(HookSpecificOutput::PreToolUse {
                permission_decision,
                ..
            }) = &output.hook_specific_output
            {
                assert_eq!(
                    permission_decision, "deny",
                    "Expected permissionDecision=deny"
                );
            }
        }
        _ => panic!("Expected HookResponse, got {:?}", response),
    }
}

#[test]
fn test_hook_stop_blocked() {
    let server = TestServer::spawn();

    let mut socket =
        ControlSocket::connect(&server.socket_path).expect("Failed to connect to socket");

    // Send a Stop hook event which should be blocked by test server
    let input = make_hook_input("Stop");
    let message = ControlMessage::HookEvent {
        input: Box::new(input),
    };

    let response = socket.send(&message).expect("Failed to send message");

    match response {
        ControlResponse::HookResponse { output, exit_code } => {
            assert_eq!(exit_code, 2, "Expected exit code 2 for blocked hook");
            // Blocked Stop should have continue=false (to stop processing)
            assert!(
                !output.continue_,
                "Expected continue=false for blocked hook"
            );
            // The stopReason should contain our test message
            if let Some(reason) = &output.stop_reason {
                assert!(
                    reason.contains("blocking") || reason.contains("Test server"),
                    "Expected stopReason to contain blocking message, got: {}",
                    reason
                );
            }
        }
        _ => panic!("Expected HookResponse, got {:?}", response),
    }
}

#[test]
fn test_multiple_connections() {
    let server = TestServer::spawn();

    // Send multiple messages, each on a new connection
    // (matches how Claude Code hooks work - each hook is a separate process)
    for i in 0..5 {
        let mut socket =
            ControlSocket::connect(&server.socket_path).expect("Failed to connect to socket");

        let mut input = make_hook_input("Notification");
        input.session_id = format!("test-session-{}", i);
        input.notification_type = Some("info".to_string());
        input.message = Some(format!("Test message {}", i));

        let message = ControlMessage::HookEvent {
            input: Box::new(input),
        };

        let response = socket.send(&message).expect("Failed to send message");

        match response {
            ControlResponse::HookResponse { exit_code, .. } => {
                assert_eq!(exit_code, 0, "Expected exit code 0 for notification");
            }
            _ => panic!("Expected HookResponse, got {:?}", response),
        }
    }
}
