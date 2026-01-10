//! HTTP API integration tests for mantle-hub.
//!
//! These tests verify all HTTP endpoints work correctly.
//! They spawn a real hub server but don't require Docker or Claude.

mod helpers;

use helpers::{make_test_register, make_test_result, TestHub};

#[tokio::test]
async fn test_register_session() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let req = make_test_register();

    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200, "Registration should succeed");

    let session: serde_json::Value = resp.json().await.unwrap();
    // ID is generated server-side - just verify it exists and is non-empty
    assert!(session["id"].as_str().map(|s| !s.is_empty()).unwrap_or(false), "ID should be non-empty");
    assert_eq!(session["state"], "running");
    assert_eq!(session["branch"], "test-branch");
}

#[tokio::test]
async fn test_list_sessions_empty() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let resp = client
        .get(format!("{}/api/sessions", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let sessions: Vec<serde_json::Value> = resp.json().await.unwrap();
    assert!(sessions.is_empty(), "New hub should have no sessions");
}

#[tokio::test]
async fn test_list_sessions_with_data() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register two sessions
    for _ in 0..2 {
        let req = make_test_register();
        client
            .post(format!("{}/api/sessions", hub.http_url))
            .json(&req)
            .send()
            .await
            .unwrap();
    }

    let resp = client
        .get(format!("{}/api/sessions", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let sessions: Vec<serde_json::Value> = resp.json().await.unwrap();
    assert_eq!(sessions.len(), 2, "Should have 2 sessions");
}

#[tokio::test]
async fn test_get_session() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let req = make_test_register();

    // Register session
    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .unwrap();

    let registered: serde_json::Value = resp.json().await.unwrap();
    let session_id = registered["id"].as_str().unwrap();

    // Get session
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let session: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(session["id"], session_id);
    assert_eq!(session["prompt"], "Test prompt");
}

#[tokio::test]
async fn test_get_session_not_found() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let resp = client
        .get(format!("{}/api/sessions/nonexistent", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 404, "Should return 404 for missing session");
}

#[tokio::test]
async fn test_delete_session() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let req = make_test_register();

    // Register session
    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .unwrap();

    let registered: serde_json::Value = resp.json().await.unwrap();
    let session_id = registered["id"].as_str().unwrap();

    // Delete session
    let resp = client
        .delete(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 204, "Delete should return 204 No Content");

    // Verify it's gone
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 404, "Session should no longer exist");
}

#[tokio::test]
async fn test_submit_result_via_http() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let req = make_test_register();

    // Register session
    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .unwrap();

    let registered: serde_json::Value = resp.json().await.unwrap();
    let session_id = registered["id"].as_str().unwrap();

    // Submit result via HTTP
    let result = make_test_result(session_id);
    let resp = client
        .post(format!("{}/api/sessions/{}/result", hub.http_url, session_id))
        .json(&result)
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200, "Result submission should succeed");

    // Verify session state changed
    let resp = client
        .get(format!("{}/api/sessions/{}", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    let session: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(session["state"], "completed");
    assert!(session["result"].is_object(), "Should have result attached");
    assert_eq!(session["result"]["exit_code"], 0);
}

#[tokio::test]
async fn test_poll_result_not_ready() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let req = make_test_register();

    // Register session (no result submitted)
    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .unwrap();

    let registered: serde_json::Value = resp.json().await.unwrap();
    let session_id = registered["id"].as_str().unwrap();

    // Poll for result
    let resp = client
        .get(format!("{}/api/sessions/{}/result", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(
        resp.status(),
        400,
        "Should return 400 when result not ready"
    );
}

#[tokio::test]
async fn test_poll_result_ready() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let req = make_test_register();

    // Register session
    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .unwrap();

    let registered: serde_json::Value = resp.json().await.unwrap();
    let session_id = registered["id"].as_str().unwrap();

    // Submit result
    let result = make_test_result(session_id);
    client
        .post(format!("{}/api/sessions/{}/result", hub.http_url, session_id))
        .json(&result)
        .send()
        .await
        .unwrap();

    // Poll for result
    let resp = client
        .get(format!("{}/api/sessions/{}/result", hub.http_url, session_id))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200, "Should return 200 when result ready");

    let result: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(result["exit_code"], 0);
    assert_eq!(result["result_text"], "Test result");
}

#[tokio::test]
async fn test_graph_data_empty() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    let resp = client
        .get(format!("{}/api/graph", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let graph: serde_json::Value = resp.json().await.unwrap();
    assert!(graph["nodes"].as_array().unwrap().is_empty());
    assert!(graph["edges"].as_array().unwrap().is_empty());
}

#[tokio::test]
async fn test_graph_data_with_sessions() {
    let hub = TestHub::spawn().await;
    let client = hub.http_client();

    // Register a session
    let req = make_test_register();
    let resp = client
        .post(format!("{}/api/sessions", hub.http_url))
        .json(&req)
        .send()
        .await
        .unwrap();

    let registered: serde_json::Value = resp.json().await.unwrap();
    let session_id = registered["id"].as_str().unwrap();

    let resp = client
        .get(format!("{}/api/graph", hub.http_url))
        .send()
        .await
        .unwrap();

    assert_eq!(resp.status(), 200);

    let graph: serde_json::Value = resp.json().await.unwrap();
    let nodes = graph["nodes"].as_array().unwrap();
    assert_eq!(nodes.len(), 1);
    assert_eq!(nodes[0]["id"], session_id);
}
