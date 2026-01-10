//! Unix socket integration tests for mantle-hub.
//!
//! These tests verify the Unix socket communication path that containers use
//! to submit results to the hub.

mod helpers;

use helpers::{
    make_test_result, register_session, submit_via_socket, unique_id, TestHub,
};
use std::collections::HashMap;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::UnixStream;

#[tokio::test]
async fn test_socket_submit_result() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register session first (returns SessionInfo with generated ID)
    let session_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let session_id = &session_info.id;

    // Submit result via socket
    let result = make_test_result(session_id);
    submit_via_socket(hub.socket_path(), &result)
        .await
        .expect("Socket submission should succeed");

    // Verify session state changed via HTTP
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let session: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(session["state"], "completed");
    assert!(session["result"].is_object());
}

#[tokio::test]
async fn test_socket_raw_protocol() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register session first
    let session_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let session_id = &session_info.id;

    // Connect to socket directly
    let stream = UnixStream::connect(hub.socket_path()).await.unwrap();
    let (reader, mut writer) = stream.into_split();

    // Send SessionResult as JSON
    let result = make_test_result(session_id);
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
    assert_eq!(resp["session_id"], session_id.as_str());
}

#[tokio::test]
async fn test_socket_result_before_registration() {
    // Test: result can be submitted even before session registration
    // With server-side ID generation, this is less likely to happen in practice,
    // but we still handle it gracefully (the result is stored, and when
    // queried after registration, the session will show the result)
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Use a made-up session ID that doesn't exist yet
    let fake_session_id = format!("orphan-{}", unique_id());

    // 1. Submit result FIRST (no session exists yet)
    let result = make_test_result(&fake_session_id);
    submit_via_socket(hub.socket_path(), &result)
        .await
        .expect("Socket submission should succeed even without session");

    // 2. Session doesn't exist in sessions table
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, fake_session_id))
        .send()
        .await
        .unwrap();
    assert_eq!(
        resp.status(),
        404,
        "Session should not exist (only orphan result in results table)"
    );

    // Note: Unlike before, we can't easily "register" a session with the same ID
    // since IDs are now generated server-side. The orphan result will remain
    // in the results table until cleaned up. This is an edge case that shouldn't
    // happen in normal operation with server-side ID generation.
}

#[tokio::test]
async fn test_socket_concurrent_submissions() {
    let hub = TestHub::spawn().await;

    // Spawn 10 concurrent socket connections
    // Each submits a result for a made-up session ID
    let handles: Vec<_> = (0..10)
        .map(|i| {
            let socket_path = hub.socket_path().to_path_buf();
            tokio::spawn(async move {
                let session_id = format!("concurrent-{}-{}", i, unique_id());
                let result = make_test_result(&session_id);
                submit_via_socket(&socket_path, &result).await
            })
        })
        .collect();

    // All should succeed (results are stored even without sessions)
    for (i, handle) in handles.into_iter().enumerate() {
        let result = handle.await.expect("Task should not panic");
        assert!(result.is_ok(), "Concurrent submission {} should succeed", i);
    }
}

#[tokio::test]
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
async fn test_socket_error_result() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register session first
    let session_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let session_id = &session_info.id;

    // Submit error result via socket
    let result = mantle_shared::hub::types::SessionResult {
        session_id: session_id.clone(),
        exit_code: 1,
        is_error: true,
        result_text: Some("Task failed".into()),
        structured_output: None,
        total_cost_usd: 0.05,
        num_turns: 3,
        cc_session_id: format!("cc-{}", session_id),
        duration_secs: 10.0,
        model_usage: HashMap::new(),
    };

    submit_via_socket(hub.socket_path(), &result)
        .await
        .expect("Socket submission should succeed");

    // Verify session state is failed
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let session: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(session["state"], "failed", "Session should be marked failed");
    assert_eq!(session["result"]["exit_code"], 1);
    assert_eq!(session["result"]["is_error"], true);
}

#[tokio::test]
async fn test_socket_with_structured_output() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register session first
    let session_info = register_session(&client, &hub.http_url)
        .await
        .unwrap();
    let session_id = &session_info.id;

    // Submit result with structured output
    let structured = serde_json::json!({
        "file_a_content": "/tmp/file_b.txt",
        "file_b_content": "The answer is: 42"
    });

    let result = mantle_shared::hub::types::SessionResult {
        session_id: session_id.clone(),
        exit_code: 0,
        is_error: false,
        result_text: Some("Task completed".into()),
        structured_output: Some(structured.clone()),
        total_cost_usd: 0.02,
        num_turns: 2,
        cc_session_id: format!("cc-{}", session_id),
        duration_secs: 5.0,
        model_usage: HashMap::new(),
    };

    submit_via_socket(hub.socket_path(), &result)
        .await
        .expect("Socket submission should succeed");

    // Verify structured output is preserved
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let session: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(session["result"]["structured_output"], structured);
}
