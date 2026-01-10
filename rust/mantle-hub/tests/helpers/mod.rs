//! Test helpers for mantle-hub integration tests.

mod test_hub;

pub use test_hub::{unique_id, TestHub};

use mantle_shared::hub::types::{SessionInfo, SessionRegister, SessionResult};
use std::collections::HashMap;
use std::path::PathBuf;

/// Create a test SessionResult with minimal required fields.
pub fn make_test_result(session_id: &str) -> SessionResult {
    SessionResult {
        session_id: session_id.to_string(),
        exit_code: 0,
        is_error: false,
        result_text: Some("Test result".into()),
        structured_output: None,
        total_cost_usd: 0.01,
        num_turns: 1,
        cc_session_id: format!("cc-{}", session_id),
        duration_secs: 1.0,
        model_usage: HashMap::new(),
    }
}

/// Create a test SessionRegister with minimal required fields.
///
/// Since session IDs are now generated server-side, this helper just
/// provides reasonable defaults for the other fields.
pub fn make_test_register() -> SessionRegister {
    SessionRegister {
        branch: "test-branch".into(),
        worktree: PathBuf::from("/tmp"),
        prompt: "Test prompt".into(),
        model: "sonnet".into(),
        parent_id: None,
    }
}

/// Create a test SessionRegister with a parent.
pub fn make_test_register_with_parent(parent_id: &str) -> SessionRegister {
    SessionRegister {
        branch: "test-child-branch".into(),
        worktree: PathBuf::from("/tmp"),
        prompt: "Test prompt".into(),
        model: "sonnet".into(),
        parent_id: Some(parent_id.to_string()),
    }
}

/// Register a session via HTTP and return the SessionInfo with generated ID.
pub async fn register_session(
    client: &reqwest::Client,
    base_url: &str,
) -> Result<SessionInfo, reqwest::Error> {
    let req = make_test_register();
    client
        .post(format!("{}/api/sessions", base_url))
        .json(&req)
        .send()
        .await?
        .json()
        .await
}

/// Register a session with parent via HTTP.
pub async fn register_session_with_parent(
    client: &reqwest::Client,
    base_url: &str,
    parent_id: &str,
) -> Result<SessionInfo, reqwest::Error> {
    let req = make_test_register_with_parent(parent_id);
    client
        .post(format!("{}/api/sessions", base_url))
        .json(&req)
        .send()
        .await?
        .json()
        .await
}

/// Submit a result via the Unix socket.
pub async fn submit_via_socket(
    socket_path: &std::path::Path,
    result: &SessionResult,
) -> Result<(), mantle_shared::error::MantleError> {
    use mantle_shared::hub::socket_client::write_result_to_socket;

    let socket_path = socket_path.to_path_buf();
    let result = result.clone();

    tokio::task::spawn_blocking(move || write_result_to_socket(&socket_path, &result))
        .await
        .expect("spawn_blocking failed")
}
