//! Unix socket integration tests for mantle-hub.
//!
//! These tests verify the Unix socket communication path that containers use
//! to submit node results to the hub.
//!
//! Tests are marked #[serial(hub)] because they spawn TestHub instances that
//! can interfere with each other when run in parallel.

mod helpers;

use helpers::{create_session, make_test_node_result, submit_via_socket, unique_id, TestHub};
use serial_test::serial;
use std::collections::HashMap;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::UnixStream;

#[tokio::test]
#[serial(hub)]
async fn test_socket_submit_result() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session first (returns SessionCreateResponse with session + root_node)
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Submit result via socket
    let result = make_test_node_result(node_id);
    submit_via_socket(hub.socket_path(), &result)
        .await
        .expect("Socket submission should succeed");

    // Verify node state changed via HTTP
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .unwrap();

    let node: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(node["state"], "completed");
    assert!(node["result"].is_object());
}

#[tokio::test]
#[serial(hub)]
async fn test_socket_raw_protocol() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session first
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let node_id = &created.root_node.id;

    // Connect to socket directly
    let stream = UnixStream::connect(hub.socket_path()).await.unwrap();
    let (reader, mut writer) = stream.into_split();

    // Send NodeResult as JSON
    let result = make_test_node_result(node_id);
    let json = serde_json::to_string(&result).unwrap();
    writer
        .write_all(format!("{}\n", json).as_bytes())
        .await
        .unwrap();

    // Read response
    let mut reader = BufReader::new(reader);
    let mut response = String::new();
    reader.read_line(&mut response).await.unwrap();

    let resp: serde_json::Value = serde_json::from_str(&response).unwrap();
    assert_eq!(resp["status"], "ok");
    assert_eq!(resp["node_id"], node_id.as_str());
}

#[tokio::test]
#[serial(hub)]
async fn test_socket_result_for_nonexistent_node() {
    // Test: submitting a result for a non-existent node should fail
    // due to foreign key constraint
    let hub = TestHub::spawn().await;

    // Use a made-up node ID that doesn't exist
    let fake_node_id = format!("orphan-{}", unique_id());

    // Submit result for non-existent node - should fail
    let result = make_test_node_result(&fake_node_id);
    let res = submit_via_socket(hub.socket_path(), &result).await;

    // With FK constraints, this should fail
    assert!(
        res.is_err(),
        "Socket submission should fail for non-existent node"
    );
}

#[tokio::test]
#[serial(hub)]
async fn test_socket_concurrent_submissions() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // First, create 10 sessions with nodes
    let mut node_ids = Vec::new();
    for _ in 0..10 {
        let created = create_session(&client, &hub.http_url).await.unwrap();
        node_ids.push(created.root_node.id.clone());
    }

    // Spawn 10 concurrent socket connections
    // Each submits a result for one of the created nodes
    let handles: Vec<_> = node_ids
        .into_iter()
        .enumerate()
        .map(|(i, node_id)| {
            let socket_path = hub.socket_path().to_path_buf();
            tokio::spawn(async move {
                let result = make_test_node_result(&node_id);
                (i, submit_via_socket(&socket_path, &result).await)
            })
        })
        .collect();

    // All should succeed
    for handle in handles {
        let (i, result) = handle.await.expect("Task should not panic");
        assert!(result.is_ok(), "Concurrent submission {} should succeed", i);
    }
}

#[tokio::test]
#[serial(hub)]
async fn test_socket_malformed_json() {
    let hub = TestHub::spawn().await;

    // Connect to socket directly
    let stream = UnixStream::connect(hub.socket_path()).await.unwrap();
    let (reader, mut writer) = stream.into_split();

    // Send malformed JSON
    writer.write_all(b"not valid json\n").await.unwrap();

    // Read response
    let mut reader = BufReader::new(reader);
    let mut response = String::new();
    reader.read_line(&mut response).await.unwrap();

    let resp: serde_json::Value = serde_json::from_str(&response).unwrap();
    assert_eq!(resp["status"], "error");
    assert!(
        resp["message"].as_str().unwrap().contains("expected"),
        "Error message should mention parsing failure"
    );
}

#[tokio::test]
#[serial(hub)]
async fn test_socket_error_result() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session first
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Submit error result via socket
    let result = mantle_shared::hub::types::NodeResult {
        node_id: node_id.clone(),
        exit_code: 1,
        is_error: true,
        result_text: Some("Task failed".into()),
        structured_output: None,
        total_cost_usd: 0.05,
        num_turns: 3,
        cc_session_id: format!("cc-{}", node_id),
        duration_secs: 10.0,
        model_usage: HashMap::new(),
    };

    submit_via_socket(hub.socket_path(), &result)
        .await
        .expect("Socket submission should succeed");

    // Verify node state is failed
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .unwrap();

    let node: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(node["state"], "failed", "Node should be marked failed");
    assert_eq!(node["result"]["exit_code"], 1);
    assert_eq!(node["result"]["is_error"], true);

    // Verify session state is also failed (derived from nodes)
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let session: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(
        session["session"]["state"], "failed",
        "Session should be marked failed"
    );
}

#[tokio::test]
#[serial(hub)]
async fn test_socket_with_structured_output() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Create session first
    let created = create_session(&client, &hub.http_url).await.unwrap();
    let session_id = &created.session.id;
    let node_id = &created.root_node.id;

    // Submit result with structured output
    let structured = serde_json::json!({
        "file_a_content": "/tmp/file_b.txt",
        "file_b_content": "The answer is: 42"
    });

    let result = mantle_shared::hub::types::NodeResult {
        node_id: node_id.clone(),
        exit_code: 0,
        is_error: false,
        result_text: Some("Task completed".into()),
        structured_output: Some(structured.clone()),
        total_cost_usd: 0.02,
        num_turns: 2,
        cc_session_id: format!("cc-{}", node_id),
        duration_secs: 5.0,
        model_usage: HashMap::new(),
    };

    submit_via_socket(hub.socket_path(), &result)
        .await
        .expect("Socket submission should succeed");

    // Verify structured output is preserved
    let resp = client
        .get(format!(
            "{}/api/sessions/{}/nodes/{}",
            hub.http_url, session_id, node_id
        ))
        .send()
        .await
        .unwrap();

    let node: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(node["result"]["structured_output"], structured);
}
