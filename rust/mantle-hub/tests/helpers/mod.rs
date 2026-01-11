//! Test helpers for mantle-hub integration tests.

mod test_hub;

pub use test_hub::{unique_id, TestHub};

use mantle_shared::hub::types::{NodeResult, SessionCreateResponse, SessionRegister};
use std::collections::HashMap;
use std::path::PathBuf;

/// Create a test NodeResult with minimal required fields.
pub fn make_test_node_result(node_id: &str) -> NodeResult {
    NodeResult {
        node_id: node_id.to_string(),
        exit_code: 0,
        is_error: false,
        result_text: Some("Test result".into()),
        structured_output: None,
        total_cost_usd: 0.01,
        num_turns: 1,
        cc_session_id: format!("cc-{}", node_id),
        duration_secs: 1.0,
        model_usage: HashMap::new(),
    }
}

/// Create a test SessionRegister with minimal required fields.
pub fn make_test_register() -> SessionRegister {
    SessionRegister {
        branch: "test-branch".into(),
        worktree: PathBuf::from("/tmp"),
        prompt: "Test prompt".into(),
        model: "sonnet".into(),
    }
}

/// Create a test SessionRegister with custom branch.
pub fn make_test_register_with_branch(branch: &str) -> SessionRegister {
    SessionRegister {
        branch: branch.into(),
        worktree: PathBuf::from("/tmp"),
        prompt: "Test prompt".into(),
        model: "sonnet".into(),
    }
}

/// Register a session via HTTP and return the SessionCreateResponse.
pub async fn create_session(
    client: &reqwest::Client,
    base_url: &str,
) -> Result<SessionCreateResponse, reqwest::Error> {
    let req = make_test_register();
    client
        .post(format!("{}/api/sessions", base_url))
        .json(&req)
        .send()
        .await?
        .json()
        .await
}

/// Create a child node via HTTP.
pub async fn create_child_node(
    client: &reqwest::Client,
    base_url: &str,
    session_id: &str,
    parent_node_id: &str,
) -> Result<serde_json::Value, reqwest::Error> {
    let req = serde_json::json!({
        "parent_node_id": parent_node_id,
        "branch": "test-child-branch",
        "worktree": "/tmp",
        "prompt": "Child prompt",
        "model": "sonnet"
    });
    client
        .post(format!("{}/api/sessions/{}/nodes", base_url, session_id))
        .json(&req)
        .send()
        .await?
        .json()
        .await
}

/// Submit a result via the Unix socket.
pub async fn submit_via_socket(
    socket_path: &std::path::Path,
    result: &NodeResult,
) -> Result<(), mantle_shared::error::MantleError> {
    use mantle_shared::hub::socket_client::write_result_to_socket;

    let socket_path = socket_path.to_path_buf();
    let result = result.clone();

    tokio::task::spawn_blocking(move || write_result_to_socket(&socket_path, &result))
        .await
        .expect("spawn_blocking failed")
}
